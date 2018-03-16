################################################################################
## Create dataframe for summary table / stats: 
##   input:  `report_df`
##   output: `summ_tbl``
################################################################################

################################################################################
## Source `report_df` and helper functions
#####

if (operational) {  ### OPERATIONAL ###
  source("./report_processor.R", local = TRUE)
  source("./summ_helper_fxns.R", local = TRUE)
} else {            ### DEBUGGING ###
  source("./UDSEnrollmentDashboard/report_processor.R", local = TRUE)
  source("./UDSEnrollmentDashboard/summ_helper_fxns.R", local = TRUE)
}

################################################################################
## Build each mini-table that's part of the larger summary table
#####

# Total counts
total_cts <- 
  single_grp_table(report_df, 
                   group_var = quo(uds_dx))
# Sex counts
sex_cts <- 
  double_grp_table(report_df, 
                   group_var_1 = quo(uds_dx), 
                   group_var_2 = quo(sex_value))
# Race counts
race_cts <- 
  double_grp_table(report_df, 
                   group_var_1 = quo(uds_dx), 
                   group_var_2 = quo(race_value))
# Sex + Race counts
sex_race_cts <- 
  triple_grp_table(report_df, 
                   group_var_1 = quo(uds_dx), 
                   group_var_2 = quo(sex_value), 
                   group_var_3 = quo(race_value))
# Autopsy Consent counts
autopsy_yes_cts <-
  single_grp_filter_table(report_df, 
                          group_var = quo(uds_dx), 
                          filter_var = quo(consent_to_autopsy), 
                          filter_var_string = "Yes")
# Autopsy Consent counts 
autopsy_consid_cts <- 
  single_grp_filter_table(report_df,
                          group_var = quo(uds_dx),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Considering")
# MRI Yes counts
mri_yes_cts <- 
  single_grp_filter_table(report_df,
                          group_var = quo(uds_dx),
                          filter_var = quo(mri_completed),
                          filter_var_string = "1. Yes")
# Blood Drawn Yes counts
blood_yes_cts <- 
  single_grp_filter_table(report_df,
                          group_var = quo(uds_dx),
                          filter_var = quo(blood_drawn),
                          filter_var_string = "1. Yes")

################################################################################
# Stitch all *_cts dfs together
#####
summ_tbl <- 
  bind_cols(total_cts, sex_cts[, -1], race_cts[, -1], sex_race_cts[, -1], 
            autopsy_yes_cts[, -1], autopsy_consid_cts[, -1],
            mri_yes_cts[, -1], blood_yes_cts[, -1])









################################################################################
################################################################################
##############################    EXTRA  SPACE    ##############################
################################################################################
################################################################################