# created to allow more distance between the intense colors at end and white 
# used: https://meyerweb.com/eric/tools/color-blend/
# used the end color and white, set midpoints = 3 and took the step before white
custom_palette <- c("#2780E3", "#C9DFF8", "white", "#FFDDC5", "#ff7518") 

# used for the density plots
custom_palette_gray <- c("white",  "#F5F5F5", "#DCDCDC", "#BEBEBE", "#888888")


get_interpolated_color <- function(value, cp = custom_palette) {
    # Create a color ramp function using the custom palette
    color_ramp <- colorRamp(cp)
    return(rgb(color_ramp(value), maxColorValue = 255))
}

create_current_week_summary_bar_chart <- function(data_all, symptoms, input_params, color_scale = 3.){
    # Produce a horizontal bar chart showing the symptoms for the selected patient on the selected week
    # in relation to the reference population.  If input_params$show_median or input_params$show_density are TRUE,
    # then the reference population is also plotted in the figure (either as a median line or a grayscale density 
    # distribution).  color_scale determines how many "sigma" until the end of the colormap (larger numbers mean
    # the patient can have large deviations from the median before getting a bold color)

    # create a summary for each of the columns for all patients as a comparison
    data_week <- data_all[data_all$week_event_number == input_params$patient_week, ]
    df <- select(data_week, -c(ID, arm, Week, week_event_number))
    summary_df <- data.frame(
        Median = apply(df, 2, function(x) quantile(x, probs = 0.5, na.rm = TRUE, type = 1)),
        Percentile_16 = apply(df, 2, function(x) quantile(x, probs = 0.16, na.rm = TRUE, type = 1)),
        Percentile_84 = apply(df, 2, function(x) quantile(x, probs = 0.84, na.rm = TRUE, type = 1))
    )
    # add the symptoms in the correct order
    summary_df$Symptom <- factor(rownames(summary_df), levels = symptoms)

    # create a dataframe for the reference population
    df_long <- pivot_longer(df, cols = symptoms, names_to = "Symptom", values_to = "Value")
    df_long$Symptom <- factor(df_long$Symptom, levels = symptoms)



    # get the patient data for the chart
    p <- select(data_week[data_week$ID == input_params$patient_id, ], -c(ID, arm, Week, week_event_number))
    patient_df <- as.data.frame(t(p))
    names(patient_df) <- c('Value')
    patient_df$Symptom <- factor(rownames(patient_df), levels = symptoms)

    # if baseline is selected for the reference population, replace values in summary_df
    if (input_params$reference_population == "baseline"){
        patient_baseline <- data_all[data_all$week_event_number == 0 & data_all$ID == input_params$patient_id, ]
        patient_baseline_long <- pivot_longer(patient_baseline, cols = symptoms, names_to = "Symptom", values_to = "Baseline")
        summary_df <- merge(summary_df, patient_baseline_long[,c("Symptom", "Baseline")], by = "Symptom", all.x = TRUE)
        summary_df$Median <- summary_df$Baseline
        # what should we do for the percentiles in this case??
        # I will try using the percentiles from the full population (but what if the baseline is outside?)
        # summary_df$Percentile_16 <- summary_df$Baseline
        # summary_df$Percentile_84 <- summary_df$Baseline
    }

    # get the colors
    # patient_df$Percentile <- 0
    patient_df$Color <- '#D3D3D3'
    for (s in symptoms){
        val <- patient_df[s,]$Value
        med <- summary_df[summary_df$Symptom == s,]$Median
        q16 <- summary_df[summary_df$Symptom == s,]$Percentile_16
        q84 <- summary_df[summary_df$Symptom == s,]$Percentile_84
        ref <- df[[s]]
        if (!is.nan(val)  & !is.na(val)){
            # the precentile that comes from ecdf does not always agree with the quantiles from above
            # so instead of coloring by the percentile, I will color by the distance from the median
            # percentile <- ecdf(ref)
            # pct <- percentile(val)
            # # fix for scenarios where every entry is the same
            # if (diff(range(ref, na.rm = TRUE)) == 0 & pct == 1) pct <- 0.5
            # # fix for scenarios where the Median is at the edge of the distribution
            # if (val == med) pct <- 0.5
            # patient_df[s,]$Percentile <- pct
            ifelse(val <= med, wd <- med - q16, wd <- q84 - med)
            cval <- ((val - med)/wd)/color_scale + 0.5
            if (wd == 0) cval <- 0.5
            cval <- pmin(pmax(cval, 0), 1)
            patient_df[s,]$Color <- get_interpolated_color(cval)
        }
    }
    summary_df <- merge(summary_df, patient_df[,c("Symptom", "Value", "Color")], by = "Symptom", all.x = TRUE)

    # for labelling (so that I can match the size of the other plot)
    breaks <- seq(0, 100, 20)
    labels <- c("0\n", "20", "40", "60", "80", "100")

    # get the maximum value to define the axis limits
    max_value <- ifelse(input_params$autoscale_symptoms_axis, max(unlist(patient_df[sapply(patient_df, is.numeric)]), na.rm = TRUE), 100)

    # create the main plot (excluding the "Total" value)
    main <- ggplot() 

    if (input_params$show_density){

        # add the density grayscale for the reference population to the figure
        main <- main + 
            stat_density(
                data = df_long[(df_long$Symptom != "Total"),],
                aes(x = Symptom, y = Value, fill = after_stat(scaled),),
                geom = "raster",
                position = "identity",
                bounds = c(0,100),
                show.legend = FALSE
            ) +
            scale_fill_gradientn(
                colors = custom_palette_gray,
                values = seq(0,1,by = 1/length(custom_palette_gray)),
            ) +
            # scale_fill_gradient2(low = "white", high = "darkgray", limits = c(0., 1)) + 
            new_scale_fill()
    }

    if (input_params$show_median){
        # add the median gray bar for the reference population to the figure
        main <- main +
            geom_errorbar(
                data = summary_df[(summary_df$Symptom != "Total"),],
                aes(x = Symptom, y = NULL, ymin = Median, ymax = Median), 
                width = 1, 
                color = "darkgray", 
                size = 3
            ) 
    }

    # add the patient data to the figure and finish formatting
    main <- main +
        geom_bar(
            data = summary_df[(summary_df$Symptom != "Total"),],
            aes(x = Symptom, y = Value, fill = Color), 
            stat = "identity", 
            color = "black", 
            width = 0.7,
            size = 1
        ) +
        scale_fill_identity() +   
        scale_x_discrete(expand = c(0,0)) +
        scale_y_continuous("", breaks = breaks, labels = labels, limits = c(0, max_value)) +
        facet_grid(Symptom ~ ., 
            scales = "free_y", 
            switch = "both"
        ) +
        coord_flip() +
        guides(fill = FALSE) +
        theme_bw() +
        xlab("") + 
        theme(
            panel.spacing = unit(0, "lines"),
            strip.text = element_text(size = 0, margin = margin(0,0,0,0)),
            plot.margin = margin(0.7, 0.09, 0.85, 0.1, "cm"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
         ) 

    # create the total plot (only including the "Total" value)
    total <- ggplot()

    if (input_params$show_density){
        # add the density grayscale for the reference population to the figure
        total <- total +
            stat_density(
                data = df_long[(df_long$Symptom == "Total"),],
                aes(x = Symptom, y = Value, fill = after_stat(scaled),),
                geom = "raster",
                position = "identity",
                bounds = c(0,100),
                show.legend = FALSE
            ) +
            scale_fill_gradientn(
                colors = custom_palette_gray,
                values = seq(0,1,by = 1/length(custom_palette_gray)),
            ) +
            # scale_fill_gradient2(low = "white", high = "darkgray", limits = c(0., 1)) + 
            new_scale_fill()
    }

    if (input_params$show_median){
        # add the median gray bar for the reference population to the figure
        total <- total +
            geom_errorbar(
                data = summary_df[(summary_df$Symptom == "Total"),],
                aes(x = Symptom, y = NULL, ymin = Median, ymax = Median), 
                width = 1, 
                color = "darkgray", 
                size = 3
            )
    }

    if (input_params$show_total){
        # add the patient data to the figure and finish formatting
        total <- total +
            geom_bar(
                data = summary_df[(summary_df$Symptom == "Total"),],
                aes(x = Symptom, y = Value, fill = Color), 
                stat = "identity", 
                color = "black", 
                width = 0.7,
                size = 1
            ) +
            scale_fill_identity() +   
            scale_x_discrete(breaks = "", labels = "", expand = c(0,0)) +
            scale_y_continuous("Symptom level", breaks = breaks, labels = labels, limits = c(0, max_value)) +
            facet_grid(Symptom ~ ., 
                scales = "free_y", 
                switch = "both"
            ) +
            coord_flip() +
            guides(fill = FALSE) +
            theme_bw() +
            xlab("") + 
            theme(
                strip.text = element_text(size = 0, margin = margin(0,0,0,0)),
                plot.margin = margin(0., 0.09, 0.25, 0.1, "cm"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
            ) 


        # combine the main and total figures and return the final figure to the user
        g <- plot_grid(main, total, 
            labels = c('Symptom', 'Total'), 
            rel_heights = c(length(symptoms) - 1, 1.4),
            label_y = c(1.01, 1.25),
            label_x = c(0.01,0.1),
            ncol = 1,
            align = "v",
            axis = "b"
        )
    } else {
        g <- main + 
            scale_y_continuous("Symptom level", breaks = breaks, labels = labels, limits = c(0, max_value))# +
            # ggtitle("Symptom") +
            # theme(plot.title = element_text(hjust = -0.17, face = "bold"))
    }


    return(g)
}

create_time_series_line_plot <- function(data_all, symptoms, input_params, color_scale = 3.){
    # Produce a faceted line chart showing the symptoms for the selected patient up until the selected week
    # in relation to the reference population.  If input_params$show_median or input_params$show_density are TRUE,
    # then the reference population is also plotted in the figure (either as a median line or a grayscale density 
    # distribution).  color_scale determines how many "sigma" until the end of the colormap (larger numbers mean
    # the patient can have large deviations from the median before getting a bold color)

    # there may be a more streamlined way to do this, but I don't know it!
    # get the median values and quantiles for the reference population 
    median_values <- data_all %>%
        group_by(week_event_number) %>%
        summarize(across(-c(ID, arm, Week), median, na.rm = TRUE))
    median_values_t <- median_values %>%
        pivot_longer(cols = -c(week_event_number),
            names_to = "Symptom",
            values_to = "Median")
    median_values_t$Symptom <- factor(median_values_t$Symptom, levels = symptoms)

    Q16_values <- data_all %>%
        group_by(week_event_number) %>%
        summarize(across(-c(ID, arm, Week), ~ quantile(., 0.16, na.rm = TRUE)))
    Q16_values_t <- Q16_values %>%
        pivot_longer(cols = -c(week_event_number),
            names_to = "Symptom",
            values_to = "Percentile_16")
    Q16_values_t$Symptom <- factor(Q16_values_t$Symptom, levels = symptoms)

    Q84_values <- data_all %>%
        group_by(week_event_number) %>%
        summarize(across(-c(ID, arm, Week), ~ quantile(., 0.84, na.rm = TRUE)))
    Q84_values_t <- Q84_values %>%
        pivot_longer(cols = -c(week_event_number),
            names_to = "Symptom",
            values_to = "Percentile_84")
    Q84_values_t$Symptom <- factor(Q84_values_t$Symptom, levels = symptoms)

    # for the reference population
    data_all_long <- na.omit(pivot_longer(data_all, cols = c(symptoms), names_to = "Symptom", values_to = "Value"))
    data_all_long$Symptom <- factor(data_all_long$Symptom, levels = symptoms)

    # get the patient data for the chart
    p <- select(data_all[data_all$ID == input_params$patient_id, ], -ID)
    patient_df <- p %>%
        pivot_longer(cols = -c(Week, arm, week_event_number),
            names_to = "Symptom",
            values_to = "Value")

    # set the symptoms as a factor and in the correct order
    patient_df$Symptom <- factor(patient_df$Symptom, levels = symptoms)
    patient_df <- patient_df %>%
        mutate(srt = match(Symptom, symptoms)) %>%
        arrange(srt) %>%
        select(-srt)

    # add the patient info to the merged_df
    merged_df <- merge(median_values_t, patient_df, by = c("week_event_number", "Symptom")) %>%
        merge(Q16_values_t, by = c("week_event_number", "Symptom")) %>%
        merge(Q84_values_t, by = c("week_event_number", "Symptom"))
    merged_df$Symptom <- factor(merged_df$Symptom, levels = symptoms)


    # if baseline is selected for the reference population, replace values in merged_df and median_values_t
    if (input_params$reference_population == "baseline"){
        patient_baseline <- data_all[data_all$week_event_number == 0 & data_all$ID == input_params$patient_id, ]
        patient_baseline_long <- pivot_longer(patient_baseline, cols = symptoms, names_to = "Symptom", values_to = "Baseline")
        median_values_t <- merge(median_values_t, patient_baseline_long[,c("Symptom", "Baseline")], by = "Symptom", all.x = TRUE)
        median_values_t$Median <- median_values_t$Baseline
        merged_df <- merge(merged_df, patient_baseline_long[,c("Symptom", "Baseline")], by = "Symptom", all.x = TRUE)
        merged_df$Median <- merged_df$Baseline
        # what should we do for the percentiles in this case??
        # I will try using the percentiles from the full population (but what if the baseline is outside?)
        # merged_df$Percentile_16 <- merged_df$Baseline
        # merged_df$Percentile_84 <- merged_df$Baseline
    }


    # get the colors
    # patient_df$Percentile <- 0
    merged_df$Color <- '#D3D3D3'
    for (row in 1:nrow(merged_df)) {
        val <- merged_df[row, "Value"][[1]]
        sym <- merged_df[row, "Symptom"][[1]]
        wk <- merged_df[row, "week_event_number"][[1]]
        med <- merged_df[row, "Median"][[1]]
        q16 <- merged_df[row, "Percentile_16"][[1]]
        q84 <- merged_df[row, "Percentile_84"][[1]]
        if (!is.nan(val) & !is.na(val)){
            # the precentile that comes from ecdf does not always agree with the quantiles from above
            # so instead of coloring by the percentile, I will color by the distance from the median
            # ref <- data_all[data_all$week_event_number == wk, sym]
            # percentile <- ecdf(ref)
            # pct <- percentile(val)
            # # fix for scenarios where every entry is the same
            # if (diff(range(ref, na.rm = TRUE)) == 0 & pct == 1) pct <- 0.5
            # # fix for scenarios where the Median is at the edge of the distribution
            # if (val == med) pct <- 0.5
            # patient_df[row, 'Percentile'] <- pct
            # patient_df[row, 'Color'] <- get_interpolated_color(pct)
            ifelse(val <= med, wd <- med - q16, wd <- q84 - med)
            cval <- ((val - med)/wd)/color_scale + 0.5
            if (wd == 0) cval <- 0.5
            cval <- pmin(pmax(cval, 0), 1)
            merged_df[row, 'Color'] <- get_interpolated_color(cval)
        }
    }

    # omit the rows with nan values??
    merged_df_clean_limit_week <- na.omit(merged_df[merged_df$week_event_number <= input_params$patient_week, ])

    # for labelling
    breaks <- seq(-4, 24, 4)
    labels <- c("-4","0\n(baseline)", "4", "8", "12", "16", "20", "24")
    labels2 <- c("-4","0", "4", "8", "12", "16", "20", "24")




    # Calculate min and max values for each facet so that I can limit the syn_long_dat_long dataframe 
    # (and therefore the facet's y axis)
    if (input_params$autoscale_symptoms_axis){
        limits <- merged_df_clean_limit_week %>%
            group_by(Symptom) %>%
            # this way the y axis scaling is fixed by the patient data and not the reference population
            # summarize(min_y = min(Value, na.rm = TRUE), max_y = max(Value, na.rm = TRUE))
            # this way, the y axis also takes into account the reference median value (so ensure that is shown)
            summarize(min_y = min(c(Value, Median), na.rm = TRUE), max_y = max(c(Value, Median), na.rm = TRUE))
    } else {
        limits <- data.frame(
            Symptoms = symptoms,
            min_y = rep(0, length(symptoms)),
            max_y = rep(100, length(symptoms))
        )
    }

    for (s in symptoms){
        ind <-  which(data_all_long$Symptom == s & data_all_long$Value > limits[limits$Symptom == s, ]$max_y*1.2)
        if (length(ind) > 0) data_all_long <- data_all_long[-ind, ]
    }

    
    # create the main plot (excluding the "Total" value)
    main <- ggplot()

    if (input_params$show_density){
        # add the density grayscale for the reference population to the figure

        # I want a way to customize the bandwidth used in the density distributions based on the min,max of the data
        custom_h <- function(lims){
            return(c(8, lims$max_y/4.))
        }
        custom_h_values <- list()
        for (s in symptoms){
            if (s != "Total") custom_h_values[[length(custom_h_values) + 1]] = custom_h(limits[limits$Symptom == s, ])
        }

        main <- main + 
            stat_density_2d(
                data = data_all_long[(data_all_long$Symptom != "Total"),],
                aes(x = Week, y = Value, fill = after_stat(ndensity),),
                geom = "raster",
                position = "identity",
                h = unlist(custom_h_values),
                contour = FALSE,
                show.legend = FALSE
            ) +
            scale_fill_gradientn(
                colors = custom_palette_gray,
                values = seq(0,1,by = 1/length(custom_palette_gray)),
            ) +
            # scale_fill_gradient2(low = "white", high = "darkgray", limits = c(0., 1)) + 
            new_scale_fill()
    }

    if (input_params$show_median){
        # add the median gray line for the reference population to the figure
        # using median_values_t here so that I can keep all the weeks (merged_df is limited to be less than the requested patient week)
        main <- main +
            geom_line(
                data = median_values_t[(median_values_t$Symptom != "Total"),],
                aes(x = week_event_number, y = Median),
                color = "darkgray", 
                # linetype = "dashed", 
                size = 1.5
            )
    }

    # add the patient data to the figure and finish formatting
    main <- main +
        geom_line(
            data = merged_df_clean_limit_week[(merged_df_clean_limit_week$Symptom != "Total"),], 
            aes(x = Week, y = Value)
        ) +
        geom_point(
            data = merged_df_clean_limit_week[(merged_df_clean_limit_week$Symptom != "Total"),], 
            aes(x = Week, y = Value, fill = Color), 
            shape = 21, 
            color = "black", 
            size = 4, 
            stroke = 1.5
        ) + 
        scale_fill_identity() + 
        facet_grid(Symptom ~ ., 
            scales = "free_y", 
            switch = "both",
        ) + 
        scale_x_continuous("", breaks = breaks, labels = labels2, limits = c(-1, 24)) + 
        expand_limits(x = c(-1, 26)) + 
        ylab("") + 
        # theme_void() + 
        guides(fill = FALSE) +
        theme_bw() + 
        theme(
            panel.spacing = unit(0, "lines"),
            strip.text = element_text(size = 0, margin = margin(0,0,0,0)),
            plot.margin = margin(0.7, 1.3, 1.2, 0.1, "cm"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.title.y.right = element_text(margin = margin(l = 10))
        ) 


    # create the total plot (including only the "Total" value)
    total <- ggplot()

    if (input_params$show_density){
        # add the density grayscale for the reference population to the figure

        # again, customize the bandwidth
        custom_h_value <- custom_h(limits[limits$Symptom == "Total", ])

        total <- total +
            stat_density_2d(
                data = data_all_long[(data_all_long$Symptom == "Total"),],
                aes(x = Week, y = Value, fill = after_stat(ndensity),),
                geom = "raster",
                h = custom_h_value,
                contour = FALSE,
                show.legend = FALSE
            ) +
            scale_fill_gradientn(
                colors = custom_palette_gray,
                values = seq(0,1,by = 1/length(custom_palette_gray)),
            ) +
            # scale_fill_gradient2(low = "white", high = "darkgray", limits = c(0., 1)) + 
            new_scale_fill()
    }

    if (input_params$show_median){
        # add the median gray line for the reference population to the figure
        # using median_values_t here so that I can keep all the weeks (merged_df is limited to be less than the requested patient week)
        total <- total +    
            geom_line(
                data = median_values_t[(median_values_t$Symptom == "Total"),],
                aes(x = week_event_number, y = Median),
                color = "darkgray", 
                # linetype = "dashed", 
                size = 1.5
            )
    }

    if (input_params$show_total){

        # add the patient data to the figure and finish formatting
        total <- total +
            geom_line(
                data = merged_df_clean_limit_week[(merged_df_clean_limit_week$Symptom == "Total"),], 
                aes(x = Week, y = Value)
            ) +
            geom_point(
                data = merged_df_clean_limit_week[(merged_df_clean_limit_week$Symptom == "Total"),], 
                aes(x = Week, y = Value, fill = Color), 
                shape = 21, 
                color = "black", 
                size = 4, 
                stroke = 1.5
            ) + 
            scale_fill_identity() +   
            facet_grid(Symptom ~ ., 
                scales = "free_y", 
                switch = "both",
            ) + 
            scale_x_continuous("Weeks since procedure", breaks = breaks, labels = labels, limits = c(-1, 24)) + 

            expand_limits(x = c(-1, 26)) + 
            ylab("") + 
            # theme_void() + 
            guides(fill = FALSE) +
            theme_bw() + 
            theme(
                strip.text = element_text(size = 0, margin = margin(0,0,0,0)),
                plot.margin = margin(0., 1.3, 0.25, 0.1, "cm"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                axis.title.y.right = element_text(margin = margin(l = 10))
            ) 
    } 

    if (input_params$autoscale_symptoms_axis){
        # function to calculate custom breaks
        custom_breaks <- function(data, n_breaks = 4, tol = 0.5) {
            # Calculate better breaks based on data range
            # would be nice to be able to use the limits variable below, but I don't know how to send this the symptom
            x <- min(data, na.rm = TRUE)
            min_value <- ifelse(is.finite(x), x, 0)
            x <- max(data, na.rm = TRUE)
            max_value <- ifelse(is.finite(x), x, 100)
            breaks <- pretty(c(min_value, max_value), n = n_breaks)
            diff <- breaks[2] - breaks[1]
            top_diff <- max_value - breaks[length(breaks)]
            if (abs(top_diff/diff) > tol) {
                breaks <- breaks[1:(length(breaks) - 1)]
                if (top_diff < 0){
                    breaks <- breaks[1:(length(breaks) - 1)]
                }
            }
            return(breaks)
        }

        main <- main + scale_y_continuous(
            "Symptom level", 
            expand = expansion(mult = c(0.2, 0.2)),
            breaks = function(x) {
                return(custom_breaks(x))
            },
            position = "right",
        )

        if (input_params$show_total){
            total <- total + scale_y_continuous(
                "Symptom level", 
                expand = expansion(mult = c(0.2, 0.2)),
                breaks = function(x) {
                    return(custom_breaks(x))
                },
                position = "right",
            )
        }
    } else {
        main <- main + scale_y_continuous(
            "Symptom level", 
            expand = expansion(mult = c(0.15, 0.15)), 
            breaks = c(0, 25, 50, 75, 100),
            limits = c(0, 100),
            position = "right"
        )
        if (input_params$show_total){
            total <- total + scale_y_continuous(
                "Symptom level", 
                expand = expansion(mult = c(0.15, 0.15)), 
                breaks = c(0, 25, 50, 75, 100),
                limits = c(0, 100),
                position = "right"
            )
        }
    }

    if (input_params$show_total){

        # combine the main and total panels into one figure and return this to the use
        g <- plot_grid(main, total, 
            rel_heights = c(length(symptoms) - 1, 1.4),
            ncol = 1,
            align = "v",
            axis = "b"
        )
    } else {
        g <- main + 
            scale_x_continuous("Weeks since procedure", breaks = breaks, labels = labels, limits = c(-1, 24))
    }
    
    return(g)

}

create_legend <- function(
    palette = custom_palette,
    breaks = c(0, 1),
    labels = c("Below\nreference\npopulation\n(better)", "Above\nreference\npopulation\n(worse)"),
    title = "",
    fontsize = 12,
    textcolor = "#555555",
    ticklinewidth = 0
){
    # create a plot just for the legend (not sure there's a simpler way to do this!)

    # create some data to plot (won't show it)
    df <- data.frame(
        Week = seq(0, 24, by = 4),
        Value = seq(0, 1, length.out = 7),
        Median = seq(0, 1, length.out = 7)    
    )

    g_legend <- ggplot() +
        geom_point(
            data = df, 
            aes(x = Week, y = Value, fill = Median), 
        ) + 
        scale_fill_gradientn(
            colors = palette,
            values = seq(0,1,by = 1/length(palette)),
            limits = c(0,1),
            breaks = breaks,
            labels = labels,
        ) +   
        guides(
            fill = guide_colorbar(
                title = title,
                title.position = "top",
                title.hjust = 0.5,
                title.vjust = 0,
                label.position = "bottom",
                ticks.colour = 'black', 
                frame.colour = 'black',
                ticks.linewidth = ticklinewidth
            )
        ) + 
        theme(
            legend.position = "bottom",           
            legend.key.width = unit(1, "cm"),
            legend.title.align = 0.5,
            legend.margin = margin(margin(10,0,0,0,"cm")),
            legend.title = element_text(size = 1.2*fontsize, color = textcolor), 
            legend.text = element_text(size = fontsize, color = textcolor),
        )   

    # grab only the legend to return to the user
    legend <- cowplot::get_legend(g_legend)

    return(legend)
}

annotate_plot <- function(plt, acolor){
    p <- plt + 

        # add to the margin to give space for annotations
        theme(plot.margin = margin(0., 2, , 2, "in")) + 
    
        ########################
        # LEFT
        ########################

        # left plot label
        annotate(
            "text",
            label = "Plots on the left show\nyour present symptoms\nin color-filled rectangles.\n(Please see the legend\nat the bottom.)", 
            size = 4, color = acolor, x = -0.2, y = 1, hjust = 0, vjust = 1,
        ) +

        # left plot reference 
        # TODO : link x,y position to actual total reference value + include type of reference
        annotate(
            "text",
            label = sprintf('\u2192'), 
            size = 16, color = acolor, x = 0.19, y = 0.26, hjust = 1, vjust = 1, angle = 45
        ) + 
        annotate(
            "text",
            label = "The reference population is shown in gray.", 
            size = 4, color = acolor, x = -0.2, y = 0.2, hjust = 0, vjust = 1
        ) + 

        # left plot explanation 
        # TODO : link x position to actual patient value + automatic text
        annotate(
            "text",
            label = sprintf('\u2192'), 
            size = 16, color = acolor, x = 0.23, y = 0.32, hjust = 0, vjust = 1, angle = 225
        ) + 
        annotate(
            "text",
            label = "Your value is similar to\nthat of the reference population.", 
            size = 4, color = acolor, x = 0.21, y = 0.39, hjust = 0, vjust = 1
        ) + 


        ########################
        # RIGHT
        ########################

        # right plot label
        annotate(
            "text",
            label = "Plots on the right show\nyour symptoms over time\nin color-filled circles.\n(Please see the legend\nat the bottom.)", 
            size = 4, color = acolor, x = 1.2, y = 1, hjust = 1, vjust = 1,
        ) +

        # right plot reference 
        # TODO : link x,y position to actual total reference value + include type of reference
        annotate(
            "text",
            label = sprintf('\u2192'), 
            size = 16, color = acolor, x = 0.87, y = 0.2, hjust = 0, vjust = 1, angle = 135
        ) + 
        annotate(
            "text",
            label = "The reference population is shown in gray.", 
            size = 4, color = acolor, x = 1.2, y = 0.2, hjust = 1, vjust = 1
        ) + 


        # left plot explanation 
        # TODO : link x position to actual patient value + automatic text
        annotate(
            "text",
            label = sprintf('\u2192'), 
            size = 16, color = acolor, x = 0.855, y = 0.32, hjust = 0, vjust = 1, angle = -90
        ) + 
        annotate(
            "text",
            label = "Your total score has decreased\nsince your last visit.", 
            size = 4, color = acolor, x = 0.8, y = 0.39, hjust = 0, vjust = 1
        ) +

        ########################
        # LEGEND
        ########################
        annotate(
            "text",
            label = "The legend below explains the colors used in all plots above.", 
            size = 4, color = acolor, x = 0.5, y = 0.13, hjust = 0.5, vjust = 0
        ) 

    return(p)

}
