#!/usr/bin/env Rscript

# Build data for conditions (condx) by dx plots

`%>%` <- magrittr::`%>%`
deployed <- TRUE

if (deployed) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard/" 
} else {
  path_to_app <- # local
    "~/Documents/GitHub/UDSEnrollmentDashboard/UDSEnrollmentDashboardCron_0.2/"
}

# **** ----
# GET DATA ----

# _ Load `df_ms_xfrm` as `data` ----
data <- readRDS(paste0(path_to_app, "rds/df_ms_xfrm.Rds"))

# _ Define `condx_vctr` ----
condx_vctr <- condx_vctr <- c(
  'cancer',       # Condx -- Cancer
  'diabet',       # Condx -- Diabetes
  'myoinf',       # Condx -- Myocardial infarction
  'conghrt',      # Condx -- Congestive heart failure
  'hypert',       # Condx -- Hypertension
  'hypchol',      # Condx -- Hypercholesterolemia
  'arth',         # Condx -- Arthritis
  'sleepap',      # Condx -- Sleep apnea
  'remdis',       # Condx -- REM sleep behavior disorder
  'hyposom'       # Condx -- Hyposomnia / insomnia
)
# _ Define `dx_levels` ----
dx_levels <- c("MCI", "NL", "LBD", "AD", "FTD", "Impaired, not MCI",
               "Pending consensus", "Withdrew", "Other")

# **** ----
# FILTER / MUTATE / CLEAN DATA ----

# _ Select only relevant fields ----
data <- data[, c("subject_id", "uds_dx", condx_vctr)]

# _ Coerce condx fields to integer
condx_col_first <- which(names(data) == "cancer")
condx_col_last  <- which(names(data) == "hyposom")
data[, condx_col_first:condx_col_last] <- 
  purrr::map_df(data[, condx_col_first:condx_col_last], as.integer)
# purrr::map_chr(data, class)

# _ Add `nullcond` field ----
data <- data %>% 
  dplyr::mutate(nullcond = NA_integer_) %>% 
  dplyr::select(subject_id, uds_dx, nullcond, dplyr::everything())

condx_col_first <- which(names(data) == "cancer")
condx_col_last  <- which(names(data) == "hyposom")
for (i in 1:nrow(data)) {
  if (!any(as.logical(data[i, condx_col_first:condx_col_last]))) {
    data[i, "nullcond"] <- 1L
  } else {
    data[i, "nullcond"] <- 0L
  }
}
# purrr::map_chr(data, class)

# **** ----
# BUILD OUT nCk CONDX ----

# _ Build list with nCk condx using `combn()` ----
condx_combn <- purrr::map(
  0:length(condx_vctr), 
  function(x) { 
    combn(condx_vctr, x, simplify = TRUE) 
  })

# _ Flatten nCk list to a chr vector ----
condx_combn_vctr <- c()
for (i in 1:length(condx_combn)) {
  for (j in 1:ncol(condx_combn[[i]])) {
    col_name <- c(condx_combn[[i]][, j])
    condx_combn_vctr <- c(condx_combn_vctr, paste(col_name, collapse = "_"))
  }
}
condx_combn_vctr[1] <- "nullcond"

# _ Add nC2 through nCk condx to `data` df ----
# (nC0 [nullcond] and nC1 [cancer, diabet, etc.] already exist)
data[, condx_combn_vctr[(length(condx_vctr)+2):length(condx_combn_vctr)]] <- NA

# _ Populate nC2 through nCk condx fields with 0s/1s ----
for (i in 1:nrow(data)) {
  for (j in (length(condx_vctr)+2):(length(condx_combn_vctr))) {
    col <- condx_combn_vctr[j]
    cols <- strsplit(condx_combn_vctr[j], "_")
    data[i, col] <- as.integer(all(as.logical(data[i, unlist(cols)])))
  }
}

# _ Deduplicate conditions (right-to-left in df) ----
for (i in 1:nrow(data)) {
  for (j in (ncol(data)):(ncol(data)-(length(condx_combn_vctr)-2))) {
    # print(paste(i, j))
    if (data[i, j] == 1) {
      data[i, (j-1):(ncol(data)-(length(condx_combn_vctr)-1))] <- 0L
    }
  }
}

# **** ----
# BUILD CONDX x DX SUM TABLE ----

condx_dx_sums <- data %>% 
  dplyr::group_by(uds_dx) %>% 
  dplyr::summarize_at(.tbl = .,
                      .vars = dplyr::vars(condx_combn_vctr[1:length(condx_combn_vctr)]),
                      .funs = dplyr::funs(sum))


# **** ----
# WRITE TO RDS ----

saveRDS(condx_dx_sums, paste0(path_to_app, "rds/data_condx.Rds"))





