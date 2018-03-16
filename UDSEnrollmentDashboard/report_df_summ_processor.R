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
# Sex counts
sex_cts <- 
  double_grp_table(report_df_procsd, 
                   group_var_1 = quo(uds_dx), 
                   group_var_2 = quo(sex_value))
# Race counts
race_cts <- 
  double_grp_table(report_df_procsd, 
                   group_var_1 = quo(uds_dx), 
                   group_var_2 = quo(race_value))
# Sex + Race counts
sex_race_cts <- 
  triple_grp_table(report_df_procsd, 
                   group_var_1 = quo(uds_dx), 
                   group_var_2 = quo(sex_value), 
                   group_var_3 = quo(race_value))
# Autopsy Consent counts
autopsy_yes_cts <-
  single_grp_filter_table(report_df_procsd, 
                          group_var = quo(uds_dx), 
                          filter_var = quo(consent_to_autopsy), 
                          filter_var_string = "Yes")
# Autopsy Consent counts 
autopsy_consid_cts <- 
  single_grp_filter_table(report_df_procsd,
                          group_var = quo(uds_dx),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Considering")
# MRI Yes counts
mri_yes_cts <- 
  single_grp_filter_table(report_df_procsd,
                          group_var = quo(uds_dx),
                          filter_var = quo(mri_completed),
                          filter_var_string = "1. Yes")
# Blood Drawn Yes counts
blood_yes_cts <- 
  single_grp_filter_table(report_df_procsd,
                          group_var = quo(uds_dx),
                          filter_var = quo(blood_drawn),
                          filter_var_string = "1. Yes")

################################################################################
## Stitch all *_cts dfs together to create initial summ_tbl
#####
summ_tbl <- 
  bind_cols(total_cts, sex_cts[, -1], race_cts[, -1], sex_race_cts[, -1], 
            autopsy_yes_cts[, -1], autopsy_consid_cts[, -1],
            mri_yes_cts[, -1], blood_yes_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

################################################################################
## Build a `totals_row` row and rbind it to `summ_table`
#####
totals <- vapply(X = summ_tbl[, 2:ncol(summ_tbl)], 
                 FUN = sum, na.rm = TRUE, 
                 FUN.VALUE = numeric(1))
totals_row <- as_data_frame(
  matrix(c("Totals", totals), nrow = 1, byrow = TRUE)
  )
names(totals_row) <- names(summ_tbl)
## Attach totals row
summ_tbl <- rbind(summ_tbl, totals_row)
## Coerce integer columns to integers
summ_tbl[2:ncol(summ_tbl)] <- lapply(X = summ_tbl[2:ncol(summ_tbl)], 
                                     FUN = as.integer)

################################################################################
## Build a `proportions_row` row and rbind it to `summ_table`
#####
## Get total number of participants: `pt_sum`
pt_sum <- as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Count"])
get_proportion <- function(x) {
  round(sum(x, na.rm = TRUE) / pt_sum, 2)
}
proportions <- vapply(X = summ_tbl[1:(nrow(summ_tbl)-1), 2:ncol(summ_tbl)], 
                      FUN = get_proportion, FUN.VALUE = numeric(1))
proportions_row <- as_data_frame(
  matrix(c("Proportions", proportions), nrow = 1, byrow = TRUE)
  )
names(proportions_row) <- names(summ_tbl)
## Attach proportions row
summ_tbl <- rbind(summ_tbl, proportions_row)

################################################################################
## Rename last four headers (generalize this later)
#####
summ_tbl <- summ_tbl %>% 
  rename(Autopsy_Yes = Count1, Autopsy_Consider = Count2,
         MRI_Yes = Count3, Blood_Drawn = Count4)

################################################################################
## Replace NA values with 0
#####
summ_tbl[is.na(summ_tbl)] <- 0





################################################################################
################################################################################
##############################    EXTRA  SPACE    ##############################
################################################################################
################################################################################