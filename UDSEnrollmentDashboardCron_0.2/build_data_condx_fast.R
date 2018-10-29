#!/usr/bin/env Rscript

# Build data for conditions (condx) by dx plots

`%>%` <- magrittr::`%>%`
deployed <- TRUE
# deployed <- FALSE

if (deployed) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard/" 
} else {
  path_to_app <- # local
    "~/Documents/GitHub/UDSEnrollmentDashboard/UDSEnrollmentDashboardCron_0.2/"
}

source(paste0(path_to_app, "helper_fxns_summ_tbls.R"), local = TRUE)

# **** ----
# GET DATA ----

# _ Load `df_ms_xfrm` as `data` ----
data <- readRDS(paste0(path_to_app, "rds/df_ms_xfrm.Rds"))

# _ Define `condx_vctr` ----
condx_vctr <- condx_vctr <- c(
  'cancer'     # Condx -- Cancer
  , 'diabet'   # Condx -- Diabetes
  , 'myoinf'   # Condx -- Myocardial infarction
  , 'conghrt'  # Condx -- Congestive heart failure
  , 'hypert'   # Condx -- Hypertension
  , 'hypchol'  # Condx -- Hypercholesterolemia
  , 'arth'     # Condx -- Arthritis
  , 'sleepap'  # Condx -- Sleep apnea
  , 'remdis'   # Condx -- REM sleep behavior disorder
  , 'hyposom'  # Condx -- Hyposomnia / insomnia
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
# condx_col_first <- which(names(data) == "diabet")
# condx_col_last  <- which(names(data) == "hypchol")
data[, condx_col_first:condx_col_last] <- 
  purrr::map_df(data[, condx_col_first:condx_col_last], as.integer)
# purrr::map_chr(data, class)

# _ Add `nullcond` field ----
data <- data %>% 
  dplyr::mutate(nullcond = NA_integer_) %>% 
  dplyr::select(subject_id, uds_dx, nullcond, dplyr::everything())

condx_col_first <- which(names(data) == "cancer")
condx_col_last  <- which(names(data) == "hyposom")
# condx_col_first <- which(names(data) == "diabet")
# condx_col_last  <- which(names(data) == "hypchol")
for (i in 1:nrow(data)) {
  if (!any(as.logical(data[i, condx_col_first:condx_col_last]))) {
    data[i, "nullcond"] <- 1L
  } else {
    data[i, "nullcond"] <- 0L
  }
}
# purrr::map_chr(data, class)

# ******** ----
# Binary hash approach ----

# data$condx_combn <- paste0(as.integer(data[, condx_vctr]), collapse = "")

data$condx_combn <- NA_character_
for (i in 1:nrow(data)) {
  data[i, "condx_combn"] <-
    paste0(
      as.character(as.integer(unlist(data[i, condx_vctr]))), 
      collapse = "")
}

data$condx_combn_name <- NA_character_
for (i in 1:nrow(data)) {
  data[i, "condx_combn_name"] <-
    paste0(
      condx_vctr[
        as.logical(as.integer(unlist(strsplit(x = data[i, "condx_combn"], 
                                              split = ""))))
        ],
      collapse = "_"
    )
}

# **** ----
# WRITE TO RDS ----

saveRDS(data, paste0(path_to_app, "rds/data_condx_fast.Rds"))

# # # # # # 
# # Reactive part
# 
# # User selects data to show in pie graph
# user_selex <- list("cancer", "hypert", "arth")
# user_selex <- unlist(user_selex)
# # user_selex_pattern <- paste(unlist(user_selex), collapse = "|")
# # detected <- stringr::str_detect(string = data$condx_combn_name,
# #                                 pattern = user_selex_pattern)
# # data <- data[detected, ]
# 
# # _ Build list with nCk user_selex condx using `combn()` ----
# user_selex_combn <- purrr::map(
#   0:length(user_selex),
#   function(x) {
#     combn(user_selex, x, simplify = TRUE)
#   })
# # _ Flatten nCk list to a chr vector ----
# user_selex_combn_vctr <- c()
# user_selex_combn_vctr_rgx <- c()
# user_selex_combn_vctr_cnt <- c()
# user_selex_combn_vctr_rows <- list()
# 
# for (i in 1:length(user_selex_combn)) {
#   for (j in 1:ncol(user_selex_combn[[i]])) {
#     col_name <- c(user_selex_combn[[i]][, j])
#     user_selex_combn_vctr <-
#       c(user_selex_combn_vctr,
#         paste0(col_name, collapse = " + "))
#     user_selex_combn_vctr_rgx <-
#       c(user_selex_combn_vctr_rgx,
#         paste0("(?=.*", col_name, ")", collapse = ""))
#   }
# }
# user_selex_combn_vctr
# user_selex_combn_vctr_rgx
# user_selex_combn_vctr_cnt
# user_selex_combn_vctr_rows
# 
# data_cp <- data
# 
# for (i in length(user_selex_combn_vctr_rgx):1) {
#   user_selex_combn_vctr_rows[[i]] <-
#     stringr::str_detect(string = data_cp$condx_combn_name,
#                         pattern = user_selex_combn_vctr_rgx[i])
#   user_selex_combn_vctr_cnt[i] <- sum(user_selex_combn_vctr_rows[[i]])
#   data_cp <- data_cp[!user_selex_combn_vctr_rows[[i]], ]
# }
# user_selex_combn_vctr
# user_selex_combn_vctr_rgx
# user_selex_combn_vctr_cnt
# # user_selex_combn_vctr_rows
# sum(user_selex_combn_vctr_cnt)
# round(user_selex_combn_vctr_cnt / sum(user_selex_combn_vctr_cnt), 2)
# 
# pie(x = user_selex_combn_vctr_cnt,
#     labels = user_selex_combn_vctr,
#     col = rainbow(n = length(user_selex_combn_vctr_cnt)),
#     clockwise = TRUE,
#     init.angle = 90)

