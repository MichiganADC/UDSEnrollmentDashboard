# Build data for cumulative enrollment plots

if (operational) {
  source("helper_fxns_plots.R", local = TRUE)
} else {
  source("./UDSEnrollmentDashboard/helper_fxns_plots.R", local = TRUE)
}

build_data_plots <- function(data) {
  
  # # # # # 
  ## Prepare output data ----
  
  ## Keep only relevant fields
  data_plots <- data %>%
    select(subject_id, exam_date, uds_dx, race_value, sex_value) %>% 
    arrange(exam_date)
  
  # # # # # 
  ## Create CumSum fields ----
  
  ## Start with a `units` field (for cumsum counting)
  data_plots <- bind_cols(data_plots,
                          data_frame(units = rep(1, nrow(data_plots))))
  
  ## Create `total_cum_sum` field
  data_plots <- data_plots %>% 
    mutate(total_cum_sum = cumsum(units))
  
  ## Create `dx_cum_sum` field
  data_plots <- data_plots %>% 
    group_by(uds_dx) %>% 
    mutate(dx_cum_sum = cumsum(units))
  
  ## Create `race_cum_sum` field
  data_plots <- data_plots %>% 
    group_by(race_value) %>% 
    mutate(race_cum_sum = cumsum(units))
  
  ## Create `sex_cum_sum` field
  data_plots <- data_plots %>% 
    group_by(sex_value) %>% 
    mutate(sex_cum_sum = cumsum(units))
  
  # # # # # 
  ## Add target diagnosis rows ----
  ## ... (e.g., "NL target", "MCI target", etc.)
  
  ## Add "NL target" rows to `data_plots`
  data_plots <- add_dx_target_rows(data_plots, "NL", "NL target", 
                                   c(0, 63, 125, 125, 125, 125))
  
  ## Add "MCI target" rows to `data_plots`
  data_plots <- add_dx_target_rows(data_plots, "MCI", "MCI target",
                                   c(0, 50, 100, 100, 100, 100))
  
  ## Add "AD target" rows to `data_plots`
  data_plots <- add_dx_target_rows(data_plots, "AD", "AD target",
                                   c(0, 18, 23, 36, 47, 58))
  
  ## Add "LBD target" rows to `data_plots`
  data_plots <- add_dx_target_rows(data_plots, "LBD", "LBD target",
                                   c(0, 10, 19, 38, 40, 37))
  
  ## Add "FTD target" rows to `data_plots`
  data_plots <- add_dx_target_rows(data_plots, "FTD", "FTD target",
                                   c(0, 5, 19, 22, 35, 36))
  
  
  # # # # # 
  ## Return `data_plots`
  return(data_plots)
}




