# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Create dataframe for plots: 
##   input:  `report_df_procsd`
##   output: `report_df_plots`
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
  # source("./plots_helper_fxns.R", local = TRUE)
} else {            ### DEBUGGING ###
  source("./UDSEnrollmentDashboard/report_processor.R", local = TRUE)
  # source("./UDSEnrollmentDashboard/plots_helper_fxns.R", local = TRUE)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Load libraries ----
###
library(dplyr)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Prepare output dataframe ----
###

## Copy report_df_procsd
report_df_plots <- report_df_procsd

## Keep only relevant fields, and sort by `exam_date`
report_df_plots <- report_df_plots %>% 
  select(subject_id, exam_date, uds_dx, race_value, sex_value) %>% 
  arrange(exam_date)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Create CumSum fields ----
###

## Start with a `units` field (to do cumsum counting)
report_df_plots <- bind_cols(report_df_plots,
                             data_frame(units = rep(1, nrow(report_df_plots))))

## Create `total_cum_sum` field
report_df_plots <- report_df_plots %>% 
  mutate(total_cum_sum = cumsum(units))

## Create `dx_cum_sum` field
report_df_plots <- report_df_plots %>% 
  group_by(uds_dx) %>% 
  mutate(dx_cum_sum = cumsum(units))

## Create `race_cum_sum` field
report_df_plots <- report_df_plots %>% 
  group_by(race_value) %>% 
  mutate(race_cum_sum = cumsum(units))

## Create `sex_cum_sum` field
report_df_plots <- report_df_plots %>% 
  group_by(sex_value) %>% 
  mutate(sex_cum_sum = cumsum(units))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Add target diagnosis rows ----
## ... (e.g., "NL target", "MCI target", etc.)
###

# Fxn to abstract away the row-adding work
add_dx_target_rows <- function(df, dx, dx_target, annual_targets) {
  target_df <- data.frame( # empty target data frame
    matrix(rep(NA, ncol(df) * 6), nrow = 6, byrow = TRUE)
  )
  # fill `target_df` with appropriate data
  names(target_df) <- names(df)
  target_df$subject_id <- paste0("UM0000XXX", 0:5)
  target_df$exam_date <- lubridate::as_date(paste0(2017:2022, "-03-01"))
  dx_levels <- c("MCI", "NL", "LBD", "AD", "Impaired, not MCI", 
                 "Pending consensus", "FTD", "Withdrew", 
                 "Amnestic multidomain", 
                 # target diagnoses
                 "MCI target", "NL target", "LBD target",
                 "AD target", "FTD target")
  target_df$uds_dx <- readr::parse_factor(rep(dx_target, 6), levels = dx_levels)
  target_df$dx_cum_sum <- annual_targets
  # return the original `df` with `target_df` attached
  bind_rows(df, target_df)
}

## Add "NL target" rows to `report_df_plots`
report_df_plots <- add_dx_target_rows(report_df_plots, "NL", "NL target", 
                                      c(0, 63, 125, 125, 125, 125))

## Add "MCI target" rows to `report_df_plots`
report_df_plots <- add_dx_target_rows(report_df_plots, "MCI", "MCI target",
                                      c(0, 50, 100, 100, 100, 100))

## Add "AD target" rows to `report_df_plots`
report_df_plots <- add_dx_target_rows(report_df_plots, "AD", "AD target",
                                      c(0, 18, 23, 36, 47, 58))

## Add "LBD target" rows to `report_df_plots`
report_df_plots <- add_dx_target_rows(report_df_plots, "LBD", "LBD target",
                                      c(0, 10, 19, 38, 40, 37))

## Add "FTD target" rows to `report_df_plots`
report_df_plots <- add_dx_target_rows(report_df_plots, "FTD", "FTD target",
                                      c(0, 5, 19, 22, 35, 36))




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # #     EXTRA  SPACE    # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 