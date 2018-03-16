################################################################################
## Get report data via REDCap API: 
##   input:  `config.R` (variables pointing to REDCap API cred.s + report ID)
##   output: `report_df`
################################################################################

tryCatch({
  source("./operational_switch.R", local = TRUE)
}, warning = function(w) {
  cat("In build/debugging mode. See warning below.\n")
  print(w)
}, finally = {
  source("./UDSEnrollmentDashboard/operational_switch.R", local = TRUE)
})

if (operational) {  ### OPERATIONAL ###
  source("./config.R", local = TRUE) # source REDCap API credentials + report ID
} else {            ### DEBUGGING ###
  source("./UDSEnrollmentDashboard/config.R", local = TRUE) # source REDCap API credentials + report ID
}

library(RCurl)
library(jsonlite)

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





################################################################################
################################################################################
##############################    EXTRA  SPACE    ##############################
################################################################################
################################################################################