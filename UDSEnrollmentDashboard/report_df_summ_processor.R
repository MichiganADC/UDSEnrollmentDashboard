# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Create dataframe for summary table / stats: 
##   input:  `report_df_procsd`
##   output: `summ_tbl`
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Source `report_df_procsd` and helper functions ----
###

# tryCatch({
#   source("operational_switch.R", local = TRUE)
# }, warning = function(w) {
#   cat("In build/debugging mode. See warning below.\n")
#   print(w)
# }, finally = {
#   source("./UDSEnrollmentDashboard/operational_switch.R", local = TRUE)
# })

if (operational) {  ### OPERATIONAL ###
  source("./report_processor.R", local = TRUE)
  source("./summ_helper_fxns.R", local = TRUE)
} else {            ### DEBUGGING ###
  source("./UDSEnrollmentDashboard/report_processor.R", local = TRUE)
  source("./UDSEnrollmentDashboard/summ_helper_fxns.R", local = TRUE)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Load libraries ----
###
library(dplyr)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Build each mini-table ---
## ... that's part of the larger summary table
###

# Total counts
total_cts <- 
  single_grp_table(report_df_procsd, 
                   group_var = quo(uds_dx))
# UDS Version counts 
uds_vers_cts <-
  double_grp_table(report_df_procsd,
                   group_var_1 = quo(uds_dx),
                   group_var_2 = quo(uds_version))
# Demographic - Sex counts
sex_cts <- 
  double_grp_table(report_df_procsd, 
                   group_var_1 = quo(uds_dx), 
                   group_var_2 = quo(sex_value))
# Demographic - Race counts
race_cts <- 
  double_grp_table(report_df_procsd, 
                   group_var_1 = quo(uds_dx), 
                   group_var_2 = quo(race_value))
# Demographic - Sex + Race counts
sex_race_cts <- 
  triple_grp_table(report_df_procsd, 
                   group_var_1 = quo(uds_dx), 
                   group_var_2 = quo(sex_value), 
                   group_var_3 = quo(race_value))
# Research - Autopsy Consent Yes counts
autopsy_yes_cts <-
  single_grp_filter_table(report_df_procsd, 
                          group_var = quo(uds_dx), 
                          filter_var = quo(consent_to_autopsy), 
                          filter_var_string = "Yes")
# Research - Autopsy Consent Considering counts 
autopsy_consid_cts <- 
  single_grp_filter_table(report_df_procsd,
                          group_var = quo(uds_dx),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Considering")
# Research - MRI Yes counts
mri_yes_cts <- 
  single_grp_filter_table(report_df_procsd,
                          group_var = quo(uds_dx),
                          filter_var = quo(mri_completed),
                          filter_var_string = "1. Yes")
# Research - Blood Drawn Yes counts
blood_yes_cts <- 
  single_grp_filter_table(report_df_procsd,
                          group_var = quo(uds_dx),
                          filter_var = quo(blood_drawn),
                          filter_var_string = "1. Yes")
# UDS Version + Research - Autopsy Yes counts
uds_autopsy_yes_cts <- 
  double_grp_filter_table(report_df_procsd,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(uds_version),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Yes")
# UDS Version + Research - Autopsy Consdiering counts
uds_autopsy_consid_cts <- 
  double_grp_filter_table(report_df_procsd,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(uds_version),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Considering")
# UDS Version + Research - MRI Yes counts
uds_mri_yes_cts <- 
  double_grp_filter_table(report_df_procsd,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(uds_version),
                          filter_var = quo(mri_completed),
                          filter_var_string = "1. Yes")
# UDS Version + Research - Blood Drawn Yes counts
uds_blood_yes_cts <- 
  double_grp_filter_table(report_df_procsd,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(uds_version),
                          filter_var = quo(blood_drawn),
                          filter_var_string = "1. Yes")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Stitch different *_cts dfs together ----
###
uds_vers_tbl <- 
  bind_cols(total_cts, uds_vers_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

# demo_tbl <- 
#   bind_cols(total_cts, sex_cts[, -1], race_cts[, -1], sex_race_cts[, -1]) %>% 
#   arrange(tolower(uds_dx))

sex_tbl <- 
  bind_cols(total_cts, sex_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

race_tbl <- 
  bind_cols(total_cts, race_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

sex_race_tbl <-
  bind_cols(total_cts, sex_race_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

rsrch_tbl <- 
  bind_cols(total_cts, 
            autopsy_yes_cts[, -1], autopsy_consid_cts[, -1], 
            mri_yes_cts[, -1], blood_yes_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

uds_rsrch_tbl <-
  bind_cols(total_cts, 
            uds_autopsy_yes_cts[, -1], uds_autopsy_consid_cts[, -1],
            uds_mri_yes_cts[, -1], uds_blood_yes_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

summ_tbl <-
  bind_cols(total_cts, sex_cts[, -1], race_cts[, -1], sex_race_cts[, -1],
            autopsy_yes_cts[, -1], autopsy_consid_cts[, -1],
            mri_yes_cts[, -1], blood_yes_cts[, -1]) %>%
  arrange(tolower(uds_dx))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Build `totals_row` row and rbind it to each summary table ----
###
sex_tbl <- add_totals_row(sex_tbl)
race_tbl <- add_totals_row(race_tbl)
rsrch_tbl <- add_totals_row(rsrch_tbl)
uds_vers_tbl <- add_totals_row(uds_vers_tbl)
sex_race_tbl <- add_totals_row(sex_race_tbl)
uds_rsrch_tbl <- add_totals_row(uds_rsrch_tbl)
summ_tbl <- add_totals_row(summ_tbl)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Build a `proportions_row` row and rbind it to each summary table ----
###
sex_tbl <- add_proportions_row(sex_tbl)
race_tbl <- add_proportions_row(race_tbl)
rsrch_tbl <- add_proportions_row(rsrch_tbl)
uds_vers_tbl <- add_proportions_row(uds_vers_tbl)
sex_race_tbl <- add_proportions_row(sex_race_tbl)
uds_rsrch_tbl <- add_proportions_row(uds_rsrch_tbl)
summ_tbl <- add_proportions_row(summ_tbl)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Coerce all numeric columns to numeric ----
###
sex_tbl[, -1] <- purrr::map(sex_tbl[, -1], as.numeric)
race_tbl[, -1] <- purrr::map(race_tbl[, -1], as.numeric)
rsrch_tbl[, -1] <- purrr::map(rsrch_tbl[, -1], as.numeric)
uds_vers_tbl[, -1] <- purrr::map(uds_vers_tbl[, -1], as.numeric)
sex_race_tbl[, -1] <- purrr::map(sex_race_tbl[, -1], as.numeric)
uds_rsrch_tbl[, -1] <- purrr::map(uds_rsrch_tbl[, -1], as.numeric)
# summ_tbl[, -1], purrr::map(summ_tbl[, -1], as.numeric)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Rename last four headers (generalize this later) ----
###
summ_tbl <- summ_tbl %>%
  rename(`Autopsy Yes` = Total1, `Autopsy Considering` = Total2,
         `MRI Yes` = Total3, `Blood Drawn Yes` = Total4)
rsrch_tbl <- rsrch_tbl %>% 
  rename(`Autopsy Yes` = Total1, `Autopsy Considering` = Total2,
         `MRI Yes` = Total3, `Blood Drawn Yes` = Total4)
uds_rsrch_tbl <- uds_rsrch_tbl %>% 
  rename(`UDS 2/3 Autopsy Yes` = `UDS 2/3`, `UDS 3 Autopsy Yes` = `UDS 3`,
         `UDS 2/3 Autopsy Consider` = `UDS 2/31`, `UDS 3 Autopsy Consider` = `UDS 31`,
         `UDS 2/3 MRI Yes` = `UDS 2/32`, `UDS 3 MRI Yes` = `UDS 32`,
         `UDS 2/3 Blood Yes` = `UDS 2/33`, `UDS 3 Blood Yes` = `UDS 33`)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Replace NA values with 0 ---- only necessary when using shiny tables (not DT)
###
# summ_tbl[is.na(summ_tbl)] <- 0
# uds_vers_tbl[is.na(uds_vers_tbl)] <- 0
# # demo_tbl[is.na(demo_tbl)] <- 0
# sex_tbl[is.na(sex_tbl)] <- 0
# race_tbl[is.na(race_tbl)] <- 0
# sex_race_tbl[is.na(sex_race_tbl)] <- 0
# rsrch_tbl[is.na(rsrch_tbl)] <- 0





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # #     EXTRA  SPACE    # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 