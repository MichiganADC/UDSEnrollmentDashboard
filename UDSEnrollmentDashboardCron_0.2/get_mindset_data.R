#!/usr/bin/env Rscript

# ETL for Mindset Registry 3/2017 report + UDS 2.0 IDs report

library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyr)

# operational <- TRUE
deployed <- TRUE

# if (operational) {
#   source(paste0(path_to_app, "config.R"), local = TRUE)
# } else {
#   source(paste0(path_to_app, "UDSEnrollmentDashboardCron/config.R"), local = TRUE)
# }

if (deployed) {
  path_to_app <-
    "~/ShinyApps/MADCDashboard/" # Michigan Medicine R Shiny server
} else {
  path_to_app <-
    "~/Documents/GitHub/UDSEnrollmentDashboard/UDSEnrollmentDashboardCron_0.2/" # local
}

source(paste0(path_to_app, "config.R"), local = TRUE)

# get_data_mindset <- function() {

# # # # # 
## Retrieve data from REDCap API ----

## Retrieve MiNDSET Registrty 3/2017 data from R/C API
## ... keep only relevant field(s)
# df_mindset <-  # build / debug
# df_mindset_xfrm <-  # operational
#   jsonlite::fromJSON(
#     RCurl::postForm(
#       uri = API_URL,
#       token = MINDSET_API_TOKEN,
#       content = 'report',
#       format = 'json',
#       report_id = MINDSET_REPORT_ID,
#       rawOrLabel = 'label',
#       rawOrLabelHeaders = 'label',
#       exportCheckboxLabel = 'false',
#       returnFormat = 'json',
#       # .opts = list(ssl.verifypeer = TRUE, verbose = TRUE) # can't use now*
#       .opts = list(ssl.verifypeer = FALSE, verbose = TRUE)
#     )
#   ) %>% 
#   dplyr::select(-pt_deceased, -withdrew_date)
ms_json <- RCurl::postForm(
  uri=API_URL,
  token=MINDSET_API_TOKEN,
  content='record',
  format='json',
  type='flat',
  'fields[0]'='subject_id',
  'fields[1]'='exam_date',
  'fields[2]'='uds_dx',
  'fields[3]'='race_value',
  'fields[4]'='sex_value',
  'fields[5]'='comp_withd',
  'fields[6]'='blood_drawn',
  'fields[7]'='consent_to_autopsy',
  'fields[8]'='mri_completed',
  'fields[9]'='county',
  'fields[10]'='birth_date',
  'fields[11]'='zip_code',
  'fields[12]'='sample_given',
  'fields[13]'='scored',
  'fields[14]'='dbl_scored',
  'fields[15]'='consensus_date',
  'fields[16]'='second_consensus',
  'fields[17]'='fb_date',
  rawOrLabel='label',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json',
  filterLogic='([exam_date] >= "2017-03-28")',
  .opts = list(ssl.verifypeer = FALSE, verbose = TRUE)
)
df_mindset_xfrm <- jsonlite::fromJSON(ms_json)

## Retrieve UDS 2.0 IDs data from R/C API
## ... keep only relevant field(s)
df_uds2_id <- 
  jsonlite::fromJSON(
    RCurl::postForm(
      uri = API_URL,
      token = UDS2_API_TOKEN,
      content = 'report',
      format = 'json',
      report_id = UDS2_REPORT_ID,
      rawOrLabel = 'label',
      rawOrLabelHeaders = 'label',
      exportCheckboxLabel = 'false',
      returnFormat = 'json',
      # .opts = list(ssl.verifypeer = TRUE, verbose = TRUE) # can't use now*
      .opts = list(ssl.verifypeer = FALSE, verbose = TRUE)
    )
  ) %>% 
  dplyr::select(subject_id) # only keep `subject_id` column


## Copy df_mindset
# df_mindset_xfrm <- df_mindset # build / debug

# # # # #  
## Define factor levels ----
##   1. redcap_event_name_levels
##   2. dx_levels
##   3. race_levels
##   4. sex_levels

## 1. redcap_event_name_levels
redcap_event_name_levels <- c("Baseline",
                              paste0("Visit 0", 1:9),
                              paste0("Visit ", 10:15))

## 2. dx_levels
dx_levels <- c("MCI", "NL", "LBD", "AD", "Impaired, not MCI",
               "Pending consensus", "FTD", "Withdrew",
               "Amnestic multidomain", "Depression", "Other",
               # target diagnoses
               "MCI target", "NL target", "LBD target",
               "AD target", "FTD target")

## 3. race_levels
race_levels <- c("Asian", "Black", "Other", "White",
                 "American Indian or Alaska Native",
                 "Native Hawaiian or Other Pacific Islander")

## 4. sex_levels
sex_levels <- c("Female", "Male")

# # # # #
## Filter rows ----
df_mindset_xfrm <- df_mindset_xfrm %>% 
  dplyr::filter(stringr::str_sub(subject_id, 1, 2) == "UM")

# # # # # 
## Mutate fields (if nec.); Coerce fields to appropriate classes ----

## Coerce `redcap_event_name` to factor class
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(redcap_event_name =
                  readr::parse_factor(redcap_event_name,
                                      levels = redcap_event_name_levels))

## Coerce `exam_date` to Date class
## Coerce `scored` to Date class
## Coerce `dbl_scored` to Date class
## Coerce `consensus_date` to Date class
## Coerce `second_consensus` to Date class
## Coerce `fb_date` to Date class
df_mindset_xfrm <- df_mindset_xfrm %>% 
  dplyr::mutate(exam_date        = lubridate::ymd(exam_date),
                scored           = lubridate::ymd(scored),
                dbl_scored       = lubridate::ymd(dbl_scored),
                consensus_date   = lubridate::ymd(consensus_date),
                second_consensus = lubridate::ymd(second_consensus),
                fb_date          = lubridate::ymd(fb_date))

## Mutate `uds_dx` and coerce to factor class
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(uds_dx = dplyr::case_when(
    uds_dx == "Amnestic MCI-memory only"          ~ "MCI",
    uds_dx == "Amnestic MCI-memory plus"          ~ "MCI",
    uds_dx == "Amnestic MCI, multiple domains"    ~ "MCI",
    uds_dx == "Amnestic MCI, single domain"       ~ "MCI",
    uds_dx == "Amnestic multidomain dementia syndrome"
    ~ "Amnestic multidomain",
    uds_dx == "Dem with Lewy bodies"              ~ "LBD",
    uds_dx == "FTD"                               ~ "FTD",
    uds_dx == "Impaired, not MCI"                 ~ "Impaired, not MCI",
    uds_dx == "NL"                                ~ "NL",
    uds_dx == "Non-Amnestic MCI-multiple domains" ~ "MCI",
    uds_dx == "Non-Amnestic MCI-single domain"    ~ "MCI",
    uds_dx == "Other"                             ~ "Other",
    uds_dx == "Patient never came to consensus"   ~ "Withdrew",
    uds_dx == "Per Center Decision-patient did not come to consensus-milestoned out of study"
    ~ "Withdrew",
    uds_dx == "Possible AD"                       ~ "AD",
    uds_dx == "Primary progressive aphasia"       ~ "FTD",
    uds_dx == "Probable AD"                       ~ "AD",
    uds_dx == "Vascular dem"                      ~ "Other",
    uds_dx == "Depression"                        ~ "Depression",
    is.na(uds_dx) & comp_withd == "Y"             ~ "Withdrew",
    uds_dx == "" & comp_withd == "Y"              ~ "Withdrew",
    is.na(uds_dx) & is.na(comp_withd)             ~ "Pending consensus",
    uds_dx == "" & comp_withd == ""               ~ "Pending consensus",
    is.na(uds_dx) & comp_withd == ""              ~ "Pending consensus",
    uds_dx == "" & is.na(comp_withd)              ~ "Pending consensus",
    is.na(uds_dx) | uds_dx == ""                  ~ "Other",
    TRUE ~ uds_dx
  )) %>%
  dplyr::mutate(uds_dx =
                  readr::parse_factor(uds_dx,
                                      levels = dx_levels))

## Mutate `race_value` and coerce to factor class
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(race_value = dplyr::case_when(
    race_value == "Asian"    ~ "Asian",
    race_value == "Black"    ~ "Black",
    race_value == "Other"    ~ "Other",
    race_value == "White"    ~ "White",
    race_value == "Hispanic" ~ "Other",
    race_value == ""         ~ "Other",
    is.na(race_value)        ~ "Other"
  )) %>%
  dplyr::mutate(race_value =
                  readr::parse_factor(race_value,
                                      levels = race_levels))

## Coerce `sex_value` to factor class
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(sex_value =
                  forcats::as_factor(sex_value, levels = sex_levels))

## Coerce `comp_withd` to factor class
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(comp_withd =
                  forcats::as_factor(comp_withd))

## Coerce `blood_drawn` to factor class
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(blood_drawn =
                  forcats::as_factor(blood_drawn))

## Coerce `consent_to_autopsy` to factor class
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(consent_to_autopsy =
                  forcats::as_factor(consent_to_autopsy))

## Coerce `mri_completed` to factor class
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(mri_completed =
                  forcats::as_factor(mri_completed))

## Leave `county` field as character class

## Coerce `birth_date` to Date class
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(birth_date =
                  lubridate::ymd(birth_date))

## Mutate `zip_code` to only include first 5 digits
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::mutate(zip_code = stringr::str_sub(zip_code, 1, 5))

## Derive (mutate) date durations
df_mindset_xfrm <- df_mindset_xfrm %>% 
  dplyr::mutate(exam_scored_dur = 
                  lubridate::interval(exam_date, scored) /
                  lubridate::ddays(1),
                exam_dbl_scored_dur = 
                  lubridate::interval(exam_date, dbl_scored) /
                  lubridate::ddays(1),
                exam_consensus_dur = 
                  lubridate::interval(exam_date, consensus_date) /
                  lubridate::ddays(1),
                final_consensus_fb_dur = 
                  lubridate::interval(second_consensus, fb_date) /
                  lubridate::ddays(1)) %>%
  dplyr::mutate(exam_scored_dur = 
                  dplyr::case_when(
                    exam_scored_dur < 0 ~ NA_real_,
                    TRUE ~ exam_scored_dur),
                exam_dbl_scored_dur =
                  dplyr::case_when(
                    exam_dbl_scored_dur < 0 ~ NA_real_,
                    TRUE ~ exam_dbl_scored_dur),
                exam_consensus_dur =
                  dplyr::case_when(
                    exam_consensus_dur < 0 ~ NA_real_,
                    TRUE ~ exam_consensus_dur),
                final_consensus_fb_dur =
                  dplyr::case_when(
                    final_consensus_fb_dur < 0 ~ NA_real_,
                    TRUE ~ final_consensus_fb_dur))

## Arrange and clean `df_mindset_xfrm` ----

## Arrange data frame by subject_id (1st), then exam_date (2nd)
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::arrange(subject_id, dplyr::desc(exam_date))

## Clean out duplicate visits ... keeping oldest visit
duplicated_subject_ids <- duplicated(df_mindset_xfrm$subject_id)
df_mindset_xfrm <- df_mindset_xfrm %>%
  dplyr::filter(!duplicated_subject_ids)

# # # # # 
## Process `df_uds2_id`` ----
## ... add a simple `uds_version` character column
###
df_uds2_id <- df_uds2_id %>% 
  dplyr::mutate(uds_version = "UDS 2/3")

# # # # # 
## Left join `df_mindset_xfrm`` and `df_uds2_id` ----
###
df_mindset_xfrm <- df_mindset_xfrm %>% 
  dplyr::left_join(df_uds2_id, by = "subject_id") %>% 
  dplyr::mutate(uds_version = 
                  ifelse(is.na(uds_version), "UDS 3", uds_version))

# # # # # 
## Return xformed MiNDSET data ----
# return(df_mindset_xfrm)
# }

saveRDS(df_mindset_xfrm, paste0(path_to_app, "rds/df_mindset_xfrm.Rds"))

# * The REDCap server for some reason isn't able to verify the Shiny server
#   SSL certificate. Andrew Carroll's (MICHR) recommendation is to bypass
#   the SSL certificate verfication... "for the time being".


# df_mindset_xfrm %>% 
#   dplyr::filter(!is.na(exam_scored_dur)) %>% 
#   dplyr::summarize(N = n(), 
#                    Mean = mean(exam_scored_dur), 
#                    SD = sd(exam_scored_dur), 
#                    SEM = (sd(exam_scored_dur) / sqrt(n())))
# df_mindset_xfrm %>% 
#   dplyr::filter(!is.na(exam_dbl_scored_dur)) %>% 
#   dplyr::summarize(N = n(), 
#                    Mean = mean(exam_dbl_scored_dur), 
#                    SD = sd(exam_dbl_scored_dur), 
#                    SEM = (sd(exam_dbl_scored_dur) / sqrt(n())))
# df_mindset_xfrm %>% 
#   dplyr::filter(!is.na(exam_consensus_dur)) %>% 
#   dplyr::summarize(N = n(), 
#                    Mean = mean(exam_consensus_dur), 
#                    SD = sd(exam_consensus_dur), 
#                    SEM = (sd(exam_consensus_dur) / sqrt(n())))
# df_mindset_xfrm %>% 
#   dplyr::filter(!is.na(final_consensus_fb_dur)) %>% 
#   dplyr::summarize(N = n(), 
#                    Mean = mean(final_consensus_fb_dur), 
#                    SD = sd(final_consensus_fb_dur), 
#                    SEM = (sd(final_consensus_fb_dur) / sqrt(n())))
# 
# hist(df_mindset_xfrm$exam_scored_dur)
# hist(df_mindset_xfrm$exam_dbl_scored_dur)
# hist(df_mindset_xfrm$exam_consensus_dur)
# hist(df_mindset_xfrm$final_consensus_fb_dur)




