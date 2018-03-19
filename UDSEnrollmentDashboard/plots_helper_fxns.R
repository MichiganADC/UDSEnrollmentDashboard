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

cstm_line_size = 1.5
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
## 
custom_scale_x <- function(start_date, end_date) {
  scale_x_date(name = "Visit Date",
               date_labels = "%b %y",
               date_breaks = "1 month",
               date_minor_breaks = "1 month",
               limits = c(start_date, end_date))
}
## 
scale_y <-
  scale_y_continuous(name = "Cumulative Participants",
                     breaks = seq(0, nrow(report_df) + 10, by = 10))
## Axis theme
custom_theme <- theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Fxn for cumulative plot (no groups)
cum_plot <- function(df, x, y, plot_title, start_date, end_date) {
  df %>%
    filter(!stringr::str_detect(uds_dx, "target")) %>%
    ggplot(data = ., aes_string(x = x, y = y)) +
    geom_line(size = cstm_line_size) +
    today_line +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %y",
                 date_breaks = "1 month",
                 date_minor_breaks = "1 month",
                 limits = c(start_date, end_date)) +
    scale_y +
    custom_theme +
    ggtitle(label = plot_title)
}

## Fxn for cumulative plot with single group
cum_plot_single_grp <- function(df, x, y, group_var, plot_title, start_date, end_date) {
  df %>% 
    filter(!stringr::str_detect(uds_dx, "target")) %>% 
    ggplot(data = ., 
           aes_string(x = x, y = y,
                      group = group_var, color = group_var), size = 2) +
    geom_line(size = cstm_line_size) +
    today_line +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %y",
                 date_breaks = "1 month",
                 date_minor_breaks = "1 month",
                 limits = c(start_date, end_date)) +
    scale_y + 
    custom_theme +
    ggtitle(label = plot_title)
}

## Fxn for cumulative plot with dx vs. dx target
cum_plot_dx_target_dx <- function(df, x, y, group_var, dx, dx_target, plot_title, start_date, end_date) {
  df %>% 
    filter(uds_dx == dx | uds_dx == dx_target) %>% 
    ggplot(data = ., 
           aes_string(x = x, y = y, 
                      group = group_var, color = group_var, linetype = group_var)) +
    geom_line(size = cstm_line_size) +
    geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %y",
                 date_breaks = "6 months",
                 date_minor_breaks = "1 month",
                 limits = c(start_date, end_date)) +
    scale_y +
    custom_theme +
    ggtitle(label = plot_title)
}





################################################################################
################################################################################
##############################    EXTRA  SPACE    ##############################
################################################################################
################################################################################