create_full_table <- function(){
    # Code mostly from Jamie Griffith

   # the questions
    q_vec <- c("Loss of bladder Control",
        "Urine leakage",
        "Urine leakage from laughing, coughing, etc.",
        "Urine leakage during activities",
        "Urine leakage from walking",
        "Urine leakage at night",
        "Pain/discomfort with bladder filling",
        "Pain/discomfort with full bladder", 
        "Pain/discomfort during Urination",
        "Pain/discomfort after urination",
        "Need to push for Urination",
        "Delay in urination",
        "Repeated stops in urine flow",
        "Slow/Weak urine flow",
        "Trickle/dribble after urination",
        "Sudden need to urinate",
        "Sudden need to urinate with potential leakage",
        "How difficult was it to wait more than a few minutes?",
        "Times awakened to urinate per night",
        "Frequency of nights awakened to urinate",
        "Number of daytime urinations",
        "Time between daytime urinations",
        "Strength of nighttime urge to urinate",
        "Constant need to urinate",
        "Bladder not empty after urination",
        "Urine dribble after voiding",
        "Splitting/spraying of urine stream",
        "Bothered from urinary symptoms")





    # select the columns that we care about
    si_29_m_nms <- lurn_si_29_names("male")

    # create a table for this particular patient
    this_patient_row <- syn_week[input_params$patient_row, si_29_m_nms]

    # calculate the frequencies
    si29_prelim_freq_table <- t(apply(syn_week[si_29_m_nms], 2,
        function(x) table(factor(x, levels = 0:4), useNA = "always")))

    si29_prelim_prop_table <- prop.table(si29_prelim_freq_table, margin = 1)

    colnames(si29_prelim_prop_table) <- c("0", "1", "2", "3", "4", "Missing")


    si29_prop_table <- cbind(`Question: Short description` = q_vec,
                            as.data.frame(si29_prelim_prop_table))

    si29_item_table <- si29_prop_table %>%
        gt(rownames_to_stub = TRUE) %>%
        tab_stubhead(label = md("**LURN SI-29 Item**")) %>%
        tab_spanner(
            label = "Response options (0-4): Higher numbers mean higher severity",
            columns = c("0", "1", "2", "3", "4", "Missing")) %>%
        tab_options(table.background.color = "white") %>%
        data_color(
            columns = c("0", "1", "2", "3", "4", "Missing"),
            method = "numeric",
            palette = c("Greys"),
            domain = c(0, 1)) %>%
        fmt_number(
            columns = c("0", "1", "2", "3", "4", "Missing"),
            decimals = 0,
            scale_by = 100,
            pattern = "{x}%") %>%
        fmt_number(
            rows = c(19, 21, 23, 28),
            columns = "4",
            pattern = "") %>%
        fmt_number(
            rows = 24,
            columns = c("2", "3", "4"),
            pattern = "") %>%
        cols_width(
            everything() ~ px(80)) %>%
        cols_width(
            1 ~ px(150)) %>%
        cols_width(
            2 ~ px(300)) %>%
        cols_width(
            8 ~ px(100)) %>%
        tab_header(title = md(
            "**Figure 1: LURN SI-29: Percentages of response values**"),
            subtitle = 
            "Synthetic data: Darker gray corresponds to higher percentages") %>%
        tab_footnote(footnote = 
            "For items SI29_Q19, SI29_Q21, SI29_Q23, SI29_Q24, and SI29_Q28, blank cells are not possible response values") %>%
        tab_source_note(source_note = paste0("Data are synthetic for testing and simulation purposes. N = ",  length(syn_week$ID))) %>%
        opt_table_font(font = "Helvetica") %>%
        tab_row_group(label = md("**Section F: Additional symptoms and bother**"),
                        rows = c("SI29_Q21", "SI29_Q22", "SI29_Q23", "SI29_Q24",
                                "SI29_Q25", "SI29_Q26", "SI29_Q27b",
                                "SI29_Q28")) %>%
        tab_row_group(label = md("**Section E: Nocturia**"),
                        rows = c("SI29_Q19", "SI29_Q20")) %>% 
        tab_row_group(label = md("**Section D: Urgency**"),
                        rows = c("SI29_Q16", "SI29_Q17", "SI29_Q18")) %>%
        tab_row_group(label = md("**Section C: Voiding difficulty**"),
                        rows = c("SI29_Q11", "SI29_Q12", "SI29_Q13",
                                "SI29_Q14", "SI29_Q15")) %>%
        tab_row_group(label = md("**Section B: Pain**"),
                        rows = c("SI29_Q7", "SI29_Q8", "SI29_Q9", "SI29_Q10")) %>%
        tab_row_group(label = md("**Section A: Incontinence**"),
                        rows = c("SI29_Q1", "SI29_Q2", "SI29_Q3",
                                "SI29_Q4", "SI29_Q5", "SI29_Q6")) %>%
        cols_label(`Question: Short description` = 
                    md("**Question: Short description**"))

    # highlight the responses from the patient

    # Define a custom color palette
    custom_palette <- c("#2780E3", "black", "#ff7518")
    # Create a color ramp function using the custom palette
    color_ramp <- colorRampPalette(custom_palette)

    # iterate through the questions
    for (cc in si_29_m_nms){
        patient_value <- this_patient_row[, cc]
        if (!is.nan(patient_value) & !is.na(patient_value)){
            median_value <- median(syn_week[, cc], na.rm = TRUE)
            Percentile_16 = quantile(syn_week[, cc], probs = 0.16, na.rm = TRUE)
            Percentile_84 = quantile(syn_week[, cc], probs = 0.84, na.rm = TRUE)
            width <- Percentile_84 - Percentile_16
            # what should I do with width == 0 ??!!
            if (width == 0) width <- 1.

            # Calculate the color value based on (patient_value - median_value) / width
            color_value <- (patient_value - median_value)/width
            color_value <- pmin(pmax(color_value, -1), 1)

            # Map color value to the custom palette
            color <- color_ramp(100)[findInterval(color_value, seq(-1, 1, length.out = 100))]
            si29_item_table <- si29_item_table %>%
                tab_style(
                    style = list(
                        cell_borders(
                            sides = c("top", "bottom", "left", "right"),
                            color = color,
                            weight = px(4)
                        )
                    ),
                    locations = list(
                        cells_body(
                            columns = paste(patient_value),
                            rows = cc
                        )
                    )
                )
        }
    }

    # create the legend for the reference population
    legend_dat <- data.frame(t(seq(0, 1, .2)))
    names(legend_dat) <- paste0(seq(0, 100, 20), "%")
    legend <- gt(legend_dat) %>%
        data_color(method = "numeric",
            palette = c("Greys"),
            domain = c(0, 1)) %>%
        fmt_number(pattern = "") %>%
        tab_header(title = "Population Values Legend") %>%
        opt_table_font(font = "Helvetica")

    # create the legend for the patient
    legend_patient_dat <- data.frame(t(seq(-1, 1, .25)))
    names(legend_patient_dat) <- seq(-1, 1, 0.25)
    legend_patient <- gt(legend_patient_dat) %>%
        data_color(method = "numeric",
            palette = custom_palette,
            domain = c(-1, 1)) %>%
        fmt_number(pattern = "") %>%
        tab_header(title = "Patient Values Legend: (patient_value - median)/width") %>%
        opt_table_font(font = "Helvetica")


    return(
        list(
            "table" = si29_item_table,
            "comparison_legend" = legend,
            "patient_legend" = legend_patient
        )
    )

}
