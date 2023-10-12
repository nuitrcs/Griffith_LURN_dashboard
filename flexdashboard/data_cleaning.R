
clean_real_data <- function(real_data, study_arm, patient_week_event_number){
    # assumes splom_vars and symptoms are defined outside this function
    
    # Some variables need renaming to work with the lurn R package.
    bph <- rename(real_data,
        SI29_Q1 = si29_q1,
        SI29_Q2 = si29_q2,
        SI29_Q3 = si29_q3,
        SI29_Q4 = si29_q4,
        SI29_Q5 = si29_q5,
        SI29_Q6 = si29_q6,
        SI29_Q7 = si29_q7,
        SI29_Q8 = si29_q8,
        SI29_Q9 = si29_q9,
        SI29_Q10 = si29_q10,
        SI29_Q11 = si29_q11,
        SI29_Q12 = si29_q12,
        SI29_Q13 = si29_q13,
        SI29_Q14 = si29_q14,
        SI29_Q15 = si29_q15,
        SI29_Q16 = si29_q16,
        SI29_Q17 = si29_q17,
        SI29_Q18 = si29_q18,
        SI29_Q19 = si29_q19,
        SI29_Q20 = si29_q20,
        SI29_Q21 = si29_q21,
        SI29_Q22 = si29_q22,
        SI29_Q23 = si29_q23,
        SI29_Q24 = si29_q24,
        SI29_Q25 = si29_q25,
        SI29_Q26 = si29_q26,
        SI29_Q27b = si29_q27b,
        SI29_Q28 = si29_q28
    )

    # Recode sex into SEX so that 2 = MALE
    bph$SEX <- case_match(bph$sex, 2 ~ 1, 1 ~ 2)

    # Add SI29_Q27a to dataframe
    # Note: This is only to enable the scoring algorithm in the LURN data
    bph$SI29_Q27a <- NA_integer_

    # Add SEX to all timepoints for each person
    # This is necessary for scoring the questionnaire using the lurn R package

    ids <- unique(bph$study_id)

    for (i in seq_along(ids)) {
    
        id <- ids[i]

        sex_of_id <- bph[id %in% bph$study_id, "SEX"]

        if (length(sum(!is.na(sex_of_id))) > 1) {
            stop("More then one SEX value found")
        } else {
            if (!all(is.na(sex_of_id)) && any(sex_of_id == "1") && !any(sex_of_id == "2", na.rm = TRUE))
                bph[id %in% bph$study_id, "SEX"] <- 1
            if (!all(is.na(sex_of_id)) && any(sex_of_id == "2") && !any(sex_of_id == "1", na.rm = TRUE))
                bph[id %in% bph$study_id, "SEX"] <- 2
        }
    }

    # there are two "arms" of the study, for those who had surgery and those who did not have surgery
    # add labels for these.  Need to figure out how to deal with these
    bph$arm <- str_sub(bph$redcap_event_name, -5)

    bph$arm <- factor(bph$arm, levels = c("arm_1", "arm_2"))

    # these will need to be updated
    arm1_old_labels <- c(
        "2_weeks_prior_to_b_arm_1",
        "baseline_visit_cli_arm_1",
        "4_weeks_posttx_arm_1",
        "8_weeks_posttx_arm_1",
        "12_weeks_posttx_cl_arm_1",
        "16_weeks_posttx_arm_1",
        "20_weeks_posttx_arm_1",
        "24_weeks_posttx_arm_1"
    )
    arm1_new_labels <- c(-2, 0, 4, 8, 12, 16, 20, 24)
    arm2_old_labels <- c(
        "baseline_visit_cli_arm_2",
        "preop_arm_2",
        "1_week_postop_arm_2",
        "2_weeks_postop_arm_2",
        "4_weeks_postop_cli_arm_2",
        "6_weeks_postop_arm_2",
        "8_weeks_postop_arm_2",
        "12_weeks_postop_arm_2",
        "16_weeks_postop_arm_2",
        "20_weeks_postop_arm_2",
        "24_weeks_postop_arm_2"
    )
    arm2_new_labels <- c(0, -1, 1, 2, 4, 6, 8, 12, 16, 20, 24)
    bph <- bph %>%
        mutate(week_event_number = recode(redcap_event_name, !!!setNames(c(arm1_new_labels,arm2_new_labels), c(arm1_old_labels,arm2_old_labels))))



    # calculate a column to store the weeks since baseline for each patient.  
    bph <- bph %>%
        # Filter rows with "baseline" in redcap_event_name and select relevant columns
        filter(redcap_event_name %in% c("1_week_postop_arm_2", "4_weeks_posttx_arm_1")) %>%
        select(study_id, reference_date = si29_date) %>%

        # Group the data by study_id
        group_by(study_id) %>%

        # Join the baseline_date with the original data frame for each study_id
        right_join(bph, by = "study_id") %>%

        # Calculate the weeks since baseline for each study_id
        mutate(
            si29_date = as.POSIXct(si29_date, format = "%Y-%m-%d"), 
            reference_date = as.POSIXct(reference_date, format = "%Y-%m-%d"), 
            weeks_since_procedure = ifelse(redcap_event_name == "1_week_postop_arm_2", 
                as.numeric(difftime(si29_date, (reference_date - weeks(1)), units = "weeks")), 
                as.numeric(difftime(si29_date, (reference_date - weeks(4)), units = "weeks"))
            ),
            weeks_since_procedure = round(weeks_since_procedure, 2),  # Round to 0.01 (not strictly necessary)
            weeks_since_procedure = ifelse(grepl("baseline", redcap_event_name), 0, weeks_since_procedure)
        ) %>%

        # Ungroup the data
        ungroup()

    # The scoring is accomplished using the lurn package.
    bph <- score_lurn_si_29(bph)

    bph_arm <- bph[bph$arm == study_arm, ] 
    bph_arm_dat <- bph_arm[c("study_id", "weeks_since_procedure", "week_event_number", splom_vars)]
    names(bph_arm_dat)<- append(c("ID", "Week", "week_event_number"), symptoms)

    # Rescale bother to 0-100
    bph_arm_dat$Bother <- bph_arm_dat$Bother/3*100

    # grab the week
    bph_arm_dat_week <- bph_arm_dat[bph_arm_dat$week_event_number == patient_week_event_number, ]


    return(
        list(
            "arm_dat" = bph_arm_dat,
            "arm_dat_week" = bph_arm_dat_week
        )
    )
}