# combn.R

`%>%` <- magrittr::`%>%`

## FAKE DATA

# condx <- c("diabet", "hypert", "sleepap")
condx <- c("cancer", "diabet", "myoinf", "conghrt", "hypert",
           "hypchol", "arth", "sleepap", "remdis", "hyposom")
dx <- c("NL", "MCI", "AD", "LBD", "FTD", "Other")

condx_combn <- purrr::map(
  .x = 0:length(condx), 
  .f = function(x) { 
    combn(condx, x, simplify = TRUE) 
  })

# fake_data <- 
#   tibble::tibble(
#     id       = 1:32,
#     dx       = c(
#       c("NL", "MCI", "NL", "NL", "LBD", "NL", "MCI", "NL", "NL", "MCI", "FTD", "AD", "NL", "NL", "MCI", "AD"),
#       c("NL", "MCI", "NL", "AD", "NL", "NL", "MCI", "AD", "NL", "MCI", "NL", "NL", "LBD", "NL", "MCI", "NL")
#     ),
#     nullcond = rep(NA, times = 32),
#     cancer   = rep(0:1, times = 1, each = 16),
#     diabet   = rep(0:1, times = 2, each = 8),
#     hypert   = rep(0:1, times = 4, each = 4),
#     sleepap  = rep(0:1, times = 8, each = 2),
#     remdis   = rep(0:1, times = 16, each = 1)
#   )
n <- 300
fake_data <- 
  tibble::tibble(
    id       = 1:n,
    dx       = sample(x = dx, size = n, replace = TRUE, 
                      prob = c(0.60, 0.15, 0.15, 0.05, 0.025, 0.025)),
    nullcond = rep(NA, times = n),
    cancer   = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.9, 0.1))),
    diabet   = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.7, 0.3))),
    myoinf   = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.8, 0.2))),
    conghrt  = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.95, 0.05))),
    hypert   = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.7, 0.3))),
    hypchol  = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.6, 0.4))),
    arth     = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.5, 0.5))),
    sleepap  = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.9, 0.1))),
    remdis   = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.95, 0.05))),
    hyposom  = round(sample(x = 0:1, size = n, replace = TRUE, 
                            prob = c(0.75, 0.25)))
  )

condx_col_1 <- 4
for (i in 1:nrow(fake_data)) {
  if (!any(as.logical(fake_data[i, condx_col_1:ncol(fake_data)]))) {
    # print(paste(i))
    fake_data[i, "nullcond"] <- 1L
  } else {
    fake_data[i, "nullcond"] <- 0L
  }
}
# purrr::map(fake_data, class)

name_vctr <- c()
for (i in 1:length(condx_combn)) {
  for (j in 1:ncol(condx_combn[[i]])) {
    col_name <- c(condx_combn[[i]][, j])
    name_vctr <- c(name_vctr, paste(col_name, collapse = "_"))
    # print(paste(blah, collapse = "_"))
  }
}
name_vctr[1] <- "nullcond"
fake_data[, name_vctr[(length(condx)+2):length(name_vctr)]] <- NA

# all(1, 1, 1)
# all(as.logical(c(1, 1, 1)))

for (i in 1:nrow(fake_data)) {
  for (j in (length(condx)+2):(length(name_vctr))) {
    col <- name_vctr[j]
    cols <- strsplit(name_vctr[j], "_")
    fake_data[i, col] <- as.integer(all(as.logical(fake_data[i, unlist(cols)])))
  }
}

condx_sums <- 
  dplyr::summarize_at(.tbl = fake_data,
                      .vars = dplyr::vars(name_vctr[2:length(name_vctr)]), 
                      .funs = dplyr::funs(sum))
# condx_sums / nrow(fake_data)

# Deduplicate conditions (right-to-left in df)
for (i in 1:nrow(fake_data)) {
  for (j in (ncol(fake_data)):(ncol(fake_data)-(length(name_vctr)-2))) {
    # print(paste(i, j))
    if (fake_data[i, j] == 1) {
      fake_data[i, (j-1):(ncol(fake_data)-(length(name_vctr)-1))] <- 0L
    }
  }
}

condx_dx_sums <- fake_data %>% 
  dplyr::group_by(dx) %>% 
  dplyr::summarize_at(.tbl = .,
                      .vars = dplyr::vars(name_vctr[1:length(name_vctr)]),
                      .funs = dplyr::funs(sum))
# condx_dx_props <- condx_dx_sums
# condx_dx_rowsums <- rowSums(condx_dx_sums[, 2:ncol(condx_dx_sums)])
# for (i in 1:nrow(condx_dx_props)) {
#   for (j in 2:ncol(condx_dx_props)) {
#     condx_dx_props[i, j] <- condx_dx_props[i, j] / condx_dx_rowsums[i]
#   }
# }

select_condx <- list("cancer", "arth", "hyposom")
select_condx <- as.character(unlist(select_condx))
select_condx_combn <- purrr::map(
  .x = 0:length(select_condx), 
  .f = function(x) { 
    combn(select_condx, x, simplify = TRUE) 
  })

select_name_vctr <- purrr::map(
  select_condx_combn, 
  function(x) {
    x <- as.data.frame(x)
    # return (
      purrr::map_chr(
        x,
        function(y) {
          paste(y, collapse = "_") 
        })
    # )
  })
select_name_vctr[[1]] <- "nullcond"
select_name_vctr <- unlist(select_name_vctr)

select_name_vctr <- c()
for (i in 1:length(select_condx_combn)) {
  for (j in 1:ncol(select_condx_combn[[i]])) {
    col_name <- c(select_condx_combn[[i]][, j])
    select_name_vctr <- c(select_name_vctr, paste(col_name, collapse = "_"))
    # print(paste(blah, collapse = "_"))
  }
}
select_name_vctr[1] <- "nullcond"
# expr <- paste0("^", select_name_vctr, "$")
# expr <- paste(expr, collapse = "|")
# names(condx_dx_sums)[grepl(expr, names(condx_dx_sums))]
select_condx_dx_sums <- condx_dx_sums[, c("dx", select_name_vctr)]


slice_ad  <- 
  as.integer(select_condx_dx_sums[
    which(select_condx_dx_sums$dx == "AD"), 2:ncol(select_condx_dx_sums)])
slice_ftd <- 
  as.integer(select_condx_dx_sums[
    which(select_condx_dx_sums$dx == "FTD"), 2:ncol(select_condx_dx_sums)])
slice_lbd <- 
  as.integer(select_condx_dx_sums[
    which(select_condx_dx_sums$dx == "LBD"), 2:ncol(select_condx_dx_sums)])
slice_mci <- 
  as.integer(select_condx_dx_sums[
    which(select_condx_dx_sums$dx == "MCI"), 2:ncol(select_condx_dx_sums)])
slice_nl  <- 
  as.integer(select_condx_dx_sums[
    which(select_condx_dx_sums$dx == "NL"), 2:ncol(select_condx_dx_sums)])

pie(x = slice_ad[as.logical(slice_ad)], 
    labels = paste(select_name_vctr[as.logical(slice_ad)], 
                   slice_ad[as.logical(slice_ad)]),
    main = "AD", col = rainbow(length(slice_ad[as.logical(slice_ad)])), 
    clockwise = TRUE)
pie(x = slice_ftd[as.logical(slice_ftd)], 
    labels = paste(select_name_vctr[as.logical(slice_ftd)], 
                   slice_ftd[as.logical(slice_ftd)]),
    main = "FTD", col = rainbow(length(slice_ftd[as.logical(slice_ftd)])), 
    clockwise = TRUE)
pie(x = slice_lbd[as.logical(slice_lbd)], 
    labels = paste(select_name_vctr[as.logical(slice_lbd)], 
                   slice_lbd[as.logical(slice_lbd)]),
    main = "LBD", col = rainbow(length(slice_lbd[as.logical(slice_lbd)])), 
    clockwise = TRUE)
pie(x = slice_mci[as.logical(slice_mci)], 
    labels = paste(select_name_vctr[as.logical(slice_mci)], 
                   slice_mci[as.logical(slice_mci)]),
    main = "MCI", col = rainbow(length(slice_mci[as.logical(slice_mci)])), 
    clockwise = TRUE)
pie(x = slice_nl[as.logical(slice_nl)], 
    labels = paste(select_name_vctr[as.logical(slice_nl)], 
                   slice_nl[as.logical(slice_nl)]),
    main = "NL", col = rainbow(length(slice_nl[as.logical(slice_nl)])), 
    clockwise = TRUE)

## REAL DATA

data <- readRDS("./UDSEnrollmentDashboardCron_0.2/rds/df_mindset_xfrm.Rds")

# # unique(data$cancer)
# unique(data$diabet)
# unique(data$hypert)
# unique(data$sleepap)
# 
# # table(data$uds_dx, data$cancer)
# table(data$uds_dx, data$diabet)
# table(data$uds_dx, data$hypert)
# table(data$uds_dx, data$sleepap)







