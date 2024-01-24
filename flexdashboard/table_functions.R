# created to allow more distance between the intense colors at end and black 
# used: https://meyerweb.com/eric/tools/color-blend/
# used the end color and black, set midpoints = 3 and took the step before black
# (same method as un plotting_functions.R but with black in the middle)
custom_palette_t <- c("#2780E3", "#0A2039", "black", "#401D06", "#ff7518") 
custom_palette_gray_t <- c("white", "#404040") 


create_full_table <- function(data_all, input_params, color_scale = 3.){

    data_week <- data_all[data_all$week_event_number == input_params$patient_week, ]
    data_baseline <- data_all[data_all$week_event_number == 0, ]

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
    this_patient_row <- data_week[data_week$study_id == input_params$patient_id, si_29_m_nms]
    this_patient_baseline <- data_baseline[data_baseline$study_id == input_params$patient_id, si_29_m_nms]

    # calculate the frequencies
    si29_prelim_freq_table <- t(apply(data_week[si_29_m_nms], 2,
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
            palette = custom_palette_gray_t,
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
            "Darker gray corresponds to higher percentages") %>%
        tab_footnote(footnote = 
            "For items SI29_Q19, SI29_Q21, SI29_Q23, SI29_Q24, and SI29_Q28, blank cells are not possible response values.") %>%
        tab_source_note(source_note = paste0("N = ",  length(data_week$study_id))) %>%
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

    # iterate through the questions
    for (cc in si_29_m_nms){
        val <- this_patient_row[, cc]
        if (!is.nan(val) & !is.na(val)){
            med <- median(data_week[, cc], na.rm = TRUE)
            q16 = quantile(data_week[, cc], probs = 0.16, na.rm = TRUE)
            q84 = quantile(data_week[, cc], probs = 0.84, na.rm = TRUE)

            # if the reference population is the baseline, use that as "med"
            if (input_params$reference_population == "baseline"){
                med <- this_patient_baseline[, cc]
                # what should we do about the percentiles for the width??
                # for now I will just use the percentiles from the full population
            }

            ifelse(val <= med, wd <- med - q16, wd <- q84 - med)
            cval <- ((val - med)/wd)/color_scale + 0.5
            if (wd == 0) cval <- 0.5
            cval <- pmin(pmax(cval, 0), 1)
            # get_interpolated_color is defined in plotting_functions.R
            color <- get_interpolated_color(cval, custom_palette_t)

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
                            columns = paste(val),
                            rows = cc
                        )
                    )
                )
        }
    }
 
    # save this table as an image so that I can add the legend (is there an easier way!?)
    tmp_html <- tempfile("tbl0", fileext = ".html")
    tmp_png <- tempfile("img0", fileext = ".png")
    foo <- as_raw_html(si29_item_table, inline_css = TRUE)
    fileConn <- file(tmp_html)
    writeLines(foo, fileConn)
    close(fileConn)
    webshot(tmp_html, file = tmp_png)
    tbl_img <- image_trim(image_read(tmp_png))
    tbl_info <- image_info(tbl_img)

    # same for the legends
    # create_legend is in plotting_functions.R
    comparison_legend <- create_legend(
        custom_palette_gray_t,
        seq(0, 1, .2),
        str_pad(paste0(seq(0, 100, 20), "%"), width = 4, side = "right", pad = " "),
        "Population Values Legend",
        6,
        "#555555", 
        0.2
    )
    ggsave(tmp_png, comparison_legend)
    clgnd_img <- image_scale(image_trim(image_read(tmp_png)), tbl_info$width*0.5)
    clgnd_info <- image_info(clgnd_img)

    # moving the labels in so that I can get the same size!
    patient_legend <- create_legend(
        custom_palette_t,
        breaks = c(0, 0.05, 0.95, 1),
        labels = c("", "Below\nreference\n(better)", "Above\nreference\n(worse)",""),
        "Patient Values Legend",
        6,
    )
    ggsave(tmp_png, patient_legend)
    plgnd_img <- image_scale(image_trim(image_read(tmp_png)), tbl_info$width*0.5) 
    plgnd_info <- image_info(plgnd_img)

    # combine into one single image
    p <- image_blank(tbl_info$width, tbl_info$height + clgnd_info$height + 40, color = 'white') %>%
        image_composite(tbl_img) %>%
        image_composite(clgnd_img, gravity = "South")

    pp <- image_blank(tbl_info$width, tbl_info$height + clgnd_info$height + plgnd_info$height + 100, color = 'white') %>%
        image_composite(p) %>%
        image_composite(plgnd_img, gravity = "South")

    return(pp)
}
