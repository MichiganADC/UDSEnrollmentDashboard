# Get report data via REDCap API
source("./config.R", local = TRUE)
library(RCurl)
library(jsonlite)

if (!exists("report_df")) {
  # Project report
  report_json <- postForm(
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
  report_df <- fromJSON(report_json)
}

# write.csv(x = report_df, file = "report_df.csv", na = "", row.names = FALSE)