# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Get report data via REDCap API: 
##   input:  `config.R` (variables pointing to REDCap API cred.s + report ID)
##   output: `report_df`
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

if (operational) {  ### OPERATIONAL ###
  source("config.R", local = TRUE) # source REDCap API credentials + report ID
} else {            ### DEBUGGING ###
  source("./UDSEnrollmentDashboard/config.R", local = TRUE) # source REDCap API credentials + report ID
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Load libraries ----
###
library(RCurl)
library(jsonlite)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Get MINDSET Regsitry report data ----
###
if (!exists("report_df")) {
  ## Project report: RCurl post => JSON => data.frame
  report_df <- 
    fromJSON(
      postForm(
        uri = API_URL,
        token = API_TOKEN,
        content = 'report',
        format = 'json',
        report_id = REPORT_ID,
        rawOrLabel = 'label',
        rawOrLabelHeaders = 'label',
        exportCheckboxLabel = 'false',
        returnFormat = 'json',
        .opts = list(ssl.verifypeer = TRUE, verbose = TRUE)
      )
    )
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Get UDS 2.0 IDs report data ----
###
if (!exists("uds2_id_df")) {
  ## Project report: RCurl post => JSON => data.frame
  uds2_id_df <- 
    fromJSON(
      postForm(
        uri = API_URL,
        token = UDS2_API_TOKEN,
        content = 'report',
        format = 'json',
        report_id = UDS2_REPORT_ID,
        rawOrLabel = 'label',
        rawOrLabelHeaders = 'label',
        exportCheckboxLabel = 'false',
        returnFormat = 'json',
        .opts = list(ssl.verifypeer = TRUE, verbose = TRUE)
      )
    ) %>% 
    dplyr::select(subject_id) # only get the `subject_id` column
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Get UDS 3.0 IDs report data ----
###
if (!exists("uds3_id_visit_df")) {
  ## Project report: RCurl post => JSON => data.frame
  uds3_id_visit_df <- 
    fromJSON(
      postForm(
        uri = API_URL,
        token = UDS3_API_TOKEN,
        content = 'report',
        format = 'json',
        report_id = UDS3_REPORT_ID,
        rawOrLabel = 'label',
        rawOrLabelHeaders = 'label',
        exportCheckboxLabel = 'false',
        returnFormat = 'json',
        .opts = list(ssl.verifypeer = TRUE, verbose = TRUE)
      )
    ) %>% 
    dplyr::select(ptid, redcap_event_name) %>% # only get these columns
    dplyr::rename(subject_id = ptid) # rename ptid to subject_id
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # #     EXTRA  SPACE    # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 