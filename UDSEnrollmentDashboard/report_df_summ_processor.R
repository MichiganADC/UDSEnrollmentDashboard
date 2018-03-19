################################################################################
## Create dataframe for summary table / stats: 
##   input:  `report_df_procsd`
##   output: `summ_tbl`
################################################################################

################################################################################
## Source `report_df_procsd` and helper functions
#####

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

################################################################################
## Load libraries
#####
library(dplyr)


################################################################################
## Build each mini-table that's part of the larger summary table
#####

# Total counts
total_cts <- 
  single_grp_table(report_df_procsd, 
                   group_var = quo(uds_dx))
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
# Research - Autopsy Consent counts
autopsy_yes_cts <-
  single_grp_filter_table(report_df_procsd, 
                          group_var = quo(uds_dx), 
                          filter_var = quo(consent_to_autopsy), 
                          filter_var_string = "Yes")
# Research - Autopsy Consent counts 
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

################################################################################
## Stitch different *_cts dfs together
#####
total_tbl <- 
  bind_cols(total_cts) %>% 
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
  bind_cols(total_cts, autopsy_yes_cts[, -1], autopsy_consid_cts[, -1], 
            mri_yes_cts[, -1], blood_yes_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

summ_tbl <- 
  bind_cols(total_cts, sex_cts[, -1], race_cts[, -1], sex_race_cts[, -1], 
            autopsy_yes_cts[, -1], autopsy_consid_cts[, -1],
            mri_yes_cts[, -1], blood_yes_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

################################################################################
## Build `totals_row` row and rbind it to each summary table
#####
summ_tbl <- add_totals_row(summ_tbl)
total_tbl <- add_totals_row(total_tbl)
# demo_tbl <- add_totals_row(demo_tbl)
sex_tbl <- add_totals_row(sex_tbl)
race_tbl <- add_totals_row(race_tbl)
sex_race_tbl <- add_totals_row(sex_race_tbl)
rsrch_tbl <- add_totals_row(rsrch_tbl)

################################################################################
## Build a `proportions_row` row and rbind it to each summary table
#####
summ_tbl <- add_proportions_row(summ_tbl)
total_tbl <- add_proportions_row(total_tbl)
# demo_tbl <- add_proportions_row(demo_tbl)
sex_tbl <- add_proportions_row(sex_tbl)
race_tbl <- add_proportions_row(race_tbl)
sex_race_tbl <- add_proportions_row(sex_race_tbl)
rsrch_tbl <- add_proportions_row(rsrch_tbl)

# ## Get total number of participants: `pt_sum`
# pt_sum <- as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Total"])
# get_proportion <- function(x) {
#   round(sum(x, na.rm = TRUE) / pt_sum, 2)
# }
# proportions <- vapply(X = summ_tbl[1:(nrow(summ_tbl)-1), 2:ncol(summ_tbl)], 
#                       FUN = get_proportion, FUN.VALUE = numeric(1))
# proportions_row <- as_data_frame(
#   matrix(c("Proportions", proportions), nrow = 1, byrow = TRUE)
# )
# names(proportions_row) <- names(summ_tbl)
# ## Attach proportions row
# summ_tbl <- rbind(summ_tbl, proportions_row)

################################################################################
## Rename last four headers (generalize this later)
#####
summ_tbl <- summ_tbl %>% 
  rename(Autopsy_Yes = Total1, Autopsy_Consider = Total2,
         MRI_Yes = Total3, Blood_Drawn = Total4)
rsrch_tbl <- rsrch_tbl %>% 
  rename(Autopsy_Yes = Total1, Autopsy_Consider = Total2,
         MRI_Yes = Total3, Blood_Drawn = Total4)

################################################################################
## Replace NA values with 0
#####
summ_tbl[is.na(summ_tbl)] <- 0
total_tbl[is.na(total_tbl)] <- 0
# demo_tbl[is.na(demo_tbl)] <- 0
sex_tbl[is.na(sex_tbl)] <- 0
race_tbl[is.na(race_tbl)] <- 0
sex_race_tbl[is.na(sex_race_tbl)] <- 0
rsrch_tbl[is.na(rsrch_tbl)] <- 0




################################################################################
################################################################################
##############################    EXTRA  SPACE    ##############################
################################################################################
################################################################################