################################################################################
## Process report_df for three different purposes:
##   1. Summary table / stats      -- report_df_summ
##   2. Plots of enrollment        -- report_df_plots
##   3. Maps of partic. enrollment -- report_df_maps
################################################################################

################################################################################
## Source `report_df`
#####

if (operational) {  ### OPERATIONAL ###
  source("./report_getter.R", local = TRUE) # source `report_df`
} else {            ### DEBUGGING ###
  source("./UDSEnrollmentDashboard/report_getter.R", local = TRUE) 
}

################################################################################
## Load libraries
#####

library(dplyr)
library(lubridate) # tidyverse dates
library(forcats)   # tidyverse factors

################################################################################
## Prepare dataframe
####

## build / dubugging: Copy report_df while building
report_df_procsd <- report_df

## Keep only relevant fields / Discard irrlevant fields
report_df_procsd <- report_df_procsd %>% 
  ## build / debugging
  select(-redcap_event_name, -pt_deceased, -withdrew_date) 
  ## operational
  # select(-subject_id, -redcap_event_name, -pt_deceased, -withdrew_date)

################################################################################
## Mutate each field appropriately and coerce to appropriate class
#####

# names(report_df)

## Coerce `exam_date` to Date class
report_df_procsd$exam_date <- ymd(report_df_procsd$exam_date)
## Mutate `uds_dx` and coerce to factor class
report_df_procsd <- report_df_procsd %>% 
  mutate(uds_dx = case_when(
    uds_dx == "Amnestic MCI-memory only"          ~ "MCI",
    uds_dx == "Amnestic MCI-memory plus"          ~ "MCI",
    uds_dx == "Amnestic MCI, multiple domains"    ~ "MCI",
    uds_dx == "Amnestic MCI, single domain"       ~ "MCI",
    uds_dx == "Amnestic multidomain dementia syndrome" 
                                                  ~ "Amnestic multidomain",
    uds_dx == "Dem with Lewy bodies"              ~ "LBD",
    # uds_dx == "FTD"                               ~ "FTD",
    # uds_dx == "Impaired, not MCI"                 ~ "Impaired, not MCI",
    # uds_dx == "NL"                                ~ "NL",
    uds_dx == "Non-Amnestic MCI-multiple domains" ~ "MCI",
    uds_dx == "Non-Amnestic MCI-single domain"    ~ "MCI",
    uds_dx == "Primary progressive aphasia"       ~ "FTD",
    uds_dx == "Probable AD"                       ~ "AD",
    is.na(uds_dx) & comp_withd == "Y"             ~ "Withdrew",
    uds_dx == "" & comp_withd == "Y"              ~ "Withdrew",
    is.na(uds_dx) & is.na(comp_withd)             ~ "Pending consensus",
    uds_dx == "" & comp_withd == ""               ~ "Pending consensus",
    is.na(uds_dx) | uds_dx == ""                  ~ "",
    TRUE ~ uds_dx
  )) %>% 
  mutate(uds_dx = as_factor(uds_dx))
## Coerce `race_value` to factor class
race_levels = c("Asian", "Black", "Other", "White", 
                "American Indian or Alaska Native", 
                "Native Hawaiian or Other Pacific Islander")
report_df_procsd <- report_df_procsd %>% 
  mutate(race_value = as_factor(race_value, levels = race_levels))
## Coerce `sex_value` to factor class
sex_levels = c("Female", "Male")
report_df_procsd <- report_df_procsd %>% 
  mutate(sex_value = as_factor(sex_value, levels = sex_levels))
## Coerce `comp_withd` to factor class
report_df_procsd <- report_df_procsd %>% 
  mutate(comp_withd = as_factor(comp_withd))
## Coerce `blood_drawn` to factor class
report_df_procsd <- report_df_procsd %>% 
  mutate(blood_drawn = as_factor(blood_drawn))
## Coerce `consent_to_autopsy` to factor class
report_df_procsd <- report_df_procsd %>% 
  mutate(consent_to_autopsy = as_factor(consent_to_autopsy))
## Coerce `mri_completed` to factor class
report_df_procsd <- report_df_procsd %>% 
  mutate(mri_completed = as_factor(mri_completed))
## Leave `county` field as character class
## Coerce `birth_date` to Date class
report_df_procsd <- report_df_procsd %>% 
  mutate(birth_date = ymd(birth_date))
## Check class of each field
# vapply(X = report_df_procsd, FUN = class, FUN.VALUE = character(1))





################################################################################
################################################################################
##############################    EXTRA  SPACE    ##############################
################################################################################
################################################################################