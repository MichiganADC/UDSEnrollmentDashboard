#!/usr/bin/env RScript

## Script to generate UDS Enrollment Table
## ... Eventually this should be run through the REDCap API 
## ... with a de-identified data report
## ... and then we can eventually push it to a web server
## ... so that it's easily accessible from any device

#####
## Passing downloaded REDCap report csv to script
#####
## To use from *Nix terminal, run this script
## ./UDS_Enrolled_Report_table.R ./input_csv/[UMMAPMindsetRegistryFile].csv

# Get command line arguments
# args = commandArgs(trailingOnly=TRUE)

# Test if there is at least one argument: if not, return an error
# if (length(args) == 0) {
#   stop("At least one argument must be supplied: [UMMAPMindsetRegistryFile].csv", call. = FALSE)
# } 
# Maybe use this later if the user wants to name the output csv file
# else if (length(args) == 1) {
# # default output file
# args[2] = "out.txt"
#}
# # Choose the csv file from the UMMAP Mindset Registry RC report
# report_df_file <- args[1]
# report_df <- readr::read_csv(file = report_df_file, trim_ws = TRUE)


#####
## Using REDCap API
####
## To use, just run the script; all the tables/plots will be generated automatically

# source("config.R") # contains API URL and API token
# library(RCurl)
# library(jsonlite)
# if (!exists(report_df)) {
#   # Project report
#   report_json <- postForm(
#     uri = API_URL,
#     token = API_TOKEN,
#     content = 'report',
#     format = 'json',
#     report_id = REPORT_ID,
#     rawOrLabel = 'label',
#     rawOrLabelHeaders = 'label',
#     exportCheckboxLabel = 'false',
#     returnFormat = 'json',
#     .opts = list(ssl.verifypeer = TRUE, verbose = TRUE)
#   )
#   report_df <- fromJSON(report_json)
#   # print(report_df) # 'report should be the same as 'report_df' after the read_csv below
# }

# source("./redcap_api_call.R", local = TRUE) # Gets remote report_df
# report_df <- read.csv(file = "UDSEnrollmentDashboard/report_df.csv", # debugging
#                       stringsAsFactors = FALSE, 
#                       strip.white = TRUE) # Gets local report_df
report_df <- read.csv(file = "report_df.csv",
                      stringsAsFactors = FALSE,
                      strip.white = TRUE) # Gets local report_df


library(tidyverse)

# names(report_df) <- 
#   gsub(pattern = "[ [:punct:]]", replacement = "_", names(report_df))
# names(report_df)

# Clean out unneeded columns
# report_df <- report_df %>% 
#   select(-Event_Name, -Deceased_, -Exam_Date) 
report_df <- report_df %>% 
  select(-redcap_event_name, -pt_deceased, -exam_date)

##############
# Coerce 'exam_date' column to Date
# report_df$exam_date <- as.Date(report_df$exam_date, format = "%Y-%m-%d")
# Coerce 'Race' column to factor
report_df$race_value <- factor(report_df$race_value, levels = c("Black", "White", "Other"))
# Coerce 'Sex' column to factor
# report_df$sex_value <- factor(report_df$sex_value, levels = c("Female", "Male"))
# Clean up 'uds_dx' column (few factors); Coerce 'uds_dx' column to factor
report_df <- report_df %>% 
  mutate(uds_dx = case_when(
    uds_dx == "Amnestic MCI-memory only" ~ "MCI",
    uds_dx == "Amnestic MCI-memory plus" ~ "MCI",
    uds_dx == "Amnestic MCI, multiple domains" ~ "MCI",
    uds_dx == "Amnestic MCI, single domain" ~ "MCI",
    uds_dx == 
      "Amnestic multidomain dementia syndrome" ~ "Amnestic multidom dem",
    uds_dx == "Dem with Lewy bodies" ~ "LBD",
    # uds_dx == "FTD" ~ "FTD",
    # uds_dx == "Impaired, not MCI" ~ "Impaired, not MCI",
    # uds_dx == "NL" ~ "NL",
    uds_dx == "Non-Amnestic MCI-multiple domains" ~ "MCI",
    uds_dx == "Non-Amnestic MCI-single domain" ~ "MCI",
    uds_dx == "Primary progressive aphasia" ~ "FTD",
    uds_dx == "Probable AD" ~ "AD",
    is.na(uds_dx) & comp_withd == "Y" ~ "Withdrew",
    is.na(uds_dx) & is.na(comp_withd) ~ "Pending consensus dx",
    TRUE ~ uds_dx
  ))
# report_df$uds_dx <-
#   factor(report_df$uds_dx,
#          levels = c("NL", "Impaired, not MCI", "MCI", "AD",
#                     "Amnestic multidom dem", "LBD", "FTD",
#                     "Pending consensus dx", "Withdrew"))
# Coerce 'Deceased_' column to logical
# report_df$pt_deceased <- as.logical(report_df$pt_deceased)
#############

# Source helper fxns
# source("./UDSEnrollmentDashboard/uds_summary_table_fxns.R", local = TRUE) # debugging
source("uds_summary_table_fxns.R", local = TRUE)

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

# Stitch all *_cts dfs together
summ_tbl <- 
  bind_cols(total_cts, sex_cts[, -1], race_cts[, -1], sex_race_cts[, -1], 
            autopsy_yes_cts[, -1], autopsy_consid_cts[, -1],
            mri_yes_cts[, -1], blood_yes_cts[, -1])

# Replace "NA" diagnosis row label with "X_Diagnosis_Blank_X"
# summ_tbl[!(grepl(pattern = ".+", x = summ_tbl$uds_dx)), "uds_dx"] <- "X_Diagnosis_Blank_X"
summ_tbl <- summ_tbl %>% 
  arrange(uds_dx)

# Build totals row
totals <- vapply(X = summ_tbl[, 2:ncol(summ_tbl)], 
                 FUN = sum, na.rm = TRUE, FUN.VALUE = numeric(1))
total_row <- as_data_frame(matrix(c("Totals", totals), nrow = 1, byrow = TRUE))
names(total_row) <- names(summ_tbl)
# total_row # Test printout
# Attach totals row
summ_tbl <- rbind(summ_tbl, total_row)
# Coerce integer columns to integers
summ_tbl[2:ncol(summ_tbl)] <- lapply(X = summ_tbl[2:ncol(summ_tbl)], 
                                   FUN = as.integer)
# summ_tbl # Test printout

# Build proportions row
# summ_tbl$uds_dx == "Totals"
pt_sum <- as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Count"])
get_proportion <- function(x) {
  round(sum(x, na.rm = TRUE) / pt_sum, 2)
}
proportions <- vapply(X = summ_tbl[1:(nrow(summ_tbl)-1), 2:ncol(summ_tbl)], 
                      FUN = get_proportion, FUN.VALUE = numeric(1))
# proportions # Test printout
proportion_row <- as_data_frame(matrix(c("Proportions", proportions), nrow = 1, byrow = TRUE))
# proportion_row # Test printout
names(proportion_row) <- names(summ_tbl)
# proportions_row # Test printout
# Attach proportions row
summ_tbl <- rbind(summ_tbl, proportion_row)
summ_tbl # Test printout


# Rename last four headers (generalize this later)
summ_tbl <- summ_tbl %>% 
  rename(Autopsy_Yes = Count1, Autopsy_Consider = Count2,
         MRI_Yes = Count3, Blood_Drawn = Count4)
summ_tbl[is.na(summ_tbl)] <- 0
# names(summ_tbl)
# print(summ_tbl)


# Write to a csv
# date_time <- substr(Sys.time(), 0, 16) %>% 
#   gsub(pattern = " ", replacement = "_", .) %>% 
#   gsub(pattern = ":", replacement = "-", .)
# export_csv <- file.path("output_csv", paste0("UDS_Enrolled_Table_", date_time, ".csv"))
# export_csv <- file.path("output_csv", paste0("UDS_Enrolled_Table_", Sys.Date(), ".csv"))
# write_csv(summ_tbl, path = export_csv, na = "")


#########################
### NSE dplyr TESTING ###
#########################
# single_grp_table <- function(x, group_var) {
#   distinct_grp_vals <- distinct(x, !!group_var)
#   x %>% 
#     group_by(!!group_var) %>% 
#     summarize(Count = n()) %>%
#     right_join(distinct_grp_vals) %>% 
#     arrange(!!group_var)
# }
# single_grp_table(report_df, group_var = quo(uds_dx))
# distinct(report_df, uds_dx)
# tibble(uds_dx = distinct(report_df, uds_dx))
#########################
### NSE dplyr TESTING ###
#########################

#####################
#####################
#####################
#####################
#### EXTRA SPACE ####
#####################
#####################
#####################
#####################