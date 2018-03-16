################################################################################
## Helper functions for plots
################################################################################

################################################################################
## Load libraries
#####
library(dplyr)
library(tidyr)
library(ggplot2)

################################################################################
## Define helper functions
#####

min_date <- 
  
  today_col <- "#888888"

## Vertical line on today
today_line <- 
  geom_vline(xintercept = Sys.Date(), color = today_col, linetype = "dashed")
## 
today_scale_x <-
  scale_x_date(name = "Visit Date",
               date_labels = "%b %y",
               date_breaks = "1 month",
               date_minor_breaks = "1 month",
               limits = c(as.Date("2017-03-01"), Sys.Date()))
# limits = as.Date(c("2017-03-01", "2018-04-01")))
## 
scale_y <-
  scale_y_continuous(name = "Cumulative Participants",
                     breaks = seq(0, nrow(report_df) + 10, by = 10))
## Axis theme
custom_theme <- theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Fxn for cumulative plot (no groups)
cum_plot <- function(df, x, y, plot_title) {
  df %>% 
    filter(!stringr::str_detect(uds_dx, "target")) %>% 
    ggplot(data = ., aes_string(x = x, y = y)) +
    geom_line() +
    today_line +
    today_scale_x +
    scale_y + 
    custom_theme +
    ggtitle(label = plot_title)
}

## Fxn for cumulative plot with single group
cum_plot_single_grp <- function(df, x, y, group_var, plot_title) {
  df %>% 
    filter(!stringr::str_detect(uds_dx, "target")) %>% 
    ggplot(data = ., 
           aes_string(x = x, y = y,
                      group = group_var, color = group_var)) +
    geom_line() +
    today_line +
    today_scale_x +
    scale_y + 
    custom_theme +
    ggtitle(label = plot_title)
}





################################################################################
################################################################################
##############################    EXTRA  SPACE    ##############################
################################################################################
################################################################################