# combn.R

library(ggplot2)

## FAKE DATA

condx <- c("cancer", "diabet", "hypert", "sleepap")

# condx
# 
# combn(condx, 0, simplify = FALSE)
# combn(condx, 1, simplify = FALSE)
# combn(condx, 2, simplify = FALSE)
# combn(condx, 3, simplify = FALSE)

# for (i in 0:length(condx)) {
#   print(combn(condx, i, simplify = TRUE))
# }
# 
# for (i in 0:3) {
#   print(combn(condx, i, simplify = TRUE))
# }

condx_combn <- purrr::map(.x = 0:length(condx), .f = function(x) { 
  combn(condx, x, simplify = TRUE) 
})

fake_data <- 
  tibble::tibble(
    id      = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
    cancer  = rep(0:1, times = 1, each = 8),
    diabet  = rep(0:1, times = 2, each = 4),
    hypert  = rep(0:1, times = 4, each = 2),
    sleepap = rep(0:1, times = 8, each = 1))

name_vctr <- c()
for (i in 1:length(condx_combn)) {
  for (j in 1:ncol(condx_combn[[i]])) {
    blah <- c(condx_combn[[i]][, j])
    name_vctr <- c(name_vctr, paste(blah, collapse = "_"))
    print(paste(blah, collapse = "_"))
  }
}
fake_data[, name_vctr[(length(condx)+2):length(name_vctr)]] <- NA

# all(1, 1, 1)
# all(as.logical(c(1, 1, 1)))

for (i in 1:nrow(fake_data)) {
  for (j in (length(condx)+2):(length(name_vctr))) {
    # print(name_vctr[j])
    # print(strsplit(name_vctr[j], "_"))
    col <- name_vctr[j]
    cols <- strsplit(name_vctr[j], "_")
    # fake_data[i, col] <- sum(fake_data[i, unlist(cols)])
    # fake_data[i, col] <- as.integer(as.logical(sum(fake_data[i, unlist(cols)])))
    fake_data[i, col] <- as.integer(all(as.logical(fake_data[i, unlist(cols)])))
  }
}

dplyr::summarize(fake_data, N = n(), Mean = mean(cancer), Sum = sum(cancer))

purrr::walk(.x = name_vctr[2:length(name_vctr)],
            .f = function(x) { 
              print(x)
              print(sum(fake_data[, x]))
            })

## REAL DATA

data <- readRDS("./UDSEnrollmentDashboardCron_0.2/rds/df_mindset_xfrm.Rds")

# unique(data$cancer)
unique(data$diabet)
unique(data$hypert)
unique(data$sleepap)

# table(data$uds_dx, data$cancer)
table(data$uds_dx, data$diabet)
table(data$uds_dx, data$hypert)
table(data$uds_dx, data$sleepap)







