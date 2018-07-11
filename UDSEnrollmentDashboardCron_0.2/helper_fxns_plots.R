## Helper functions for plots

# # # # # 
## Define helper functions ----

cstm_line_size = 1.5
today_col <- "#888888"

## Vertical line on today
today_line <-
  # geom_vline(aes(xintercept = as.integer(as.POSIXct(Sys.Date()))), 
  geom_vline(aes(xintercept = as.integer(as.Date(Sys.Date()))),
             color = today_col, linetype = "dashed")

## Axis theme
custom_theme <- theme(text = element_text(size = 15), 
                      axis.text.x = element_text(angle = 45, hjust = 1))

## Fxn for cumulative plot (no groups)
cum_plot <- function(df, x, y, plot_title, start_date, end_date) {
  df %>%
    filter(!stringr::str_detect(uds_dx, "target")) %>%
    ggplot(data = ., aes_string(x = x, y = y)) +
    geom_line(size = cstm_line_size) +
    today_line +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %Y",
                 date_breaks = "1 month",
                 date_minor_breaks = "1 month",
                 limits = c(start_date, end_date)
                 ) +
    scale_y_continuous(name = "Cumulative Participants",
                       breaks = seq(0, nrow(df) + 10, by = 20)) +
    custom_theme +
    ggtitle(label = plot_title)
}

## Fxn for cumulative plot with single group
cum_plot_single_grp <- function(df, x, y, group_var, 
                                plot_title, start_date, end_date) {
  df %>% 
    filter(!stringr::str_detect(uds_dx, "target")) %>% 
    ggplot(data = ., 
           aes_string(x = x, y = y,
                      group = group_var, color = group_var), size = 2) +
    geom_line(size = cstm_line_size) +
    today_line +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %Y",
                 date_breaks = "1 month",
                 date_minor_breaks = "1 month",
                 limits = c(start_date, end_date)
                 ) +
    scale_y_continuous(name = "Cumulative Participants",
                       breaks = seq(0, nrow(df) + 10, by = 20)) + 
    custom_theme +
    ggtitle(label = plot_title)
}

## Fxn for cumulative plot with dx vs. dx target
cum_plot_dx_target_dx <- function(df, x, y, group_var, 
                                  dx, dx_target, 
                                  plot_title, start_date, end_date) {
  df %>% 
    filter(uds_dx == dx | uds_dx == dx_target) %>% 
    ggplot(data = ., 
           aes_string(x = x, y = y, 
                      group = group_var, 
                      color = group_var, 
                      linetype = group_var)) +
    geom_line(size = cstm_line_size) +
    today_line +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %Y",
                 date_breaks = "6 months",
                 date_minor_breaks = "1 month",
                 limits = c(start_date, end_date)
                 ) +
    scale_y_continuous(name = "Cumulative Participants",
                       breaks = seq(0, nrow(df) + 10, by = 20)) +
    custom_theme +
    ggtitle(label = plot_title)
}

## Fxn to abstract away work of adding diagnosis target rows
add_dx_target_rows <- function(df, dx, dx_target, annual_targets) {
  target_df <- data.frame( # empty target data frame
    matrix(rep(NA, ncol(df) * 6), nrow = 6, byrow = TRUE)
  )
  # fill `target_df` with appropriate data
  names(target_df) <- names(df)
  target_df$subject_id <- paste0("UM0000XXX", 0:5)
  target_df$exam_date <- lubridate::as_date(paste0(2017:2022, "-03-01"))
  dx_levels <- c("MCI", "NL", "LBD", "AD", "FTD", "Impaired, not MCI",
                 "Pending consensus", "Withdrew", "Other",
                 # target diagnoses
                 "MCI target", "NL target", "LBD target",
                 "AD target", "FTD target")
  target_df$uds_dx <- readr::parse_factor(rep(dx_target, 6), levels = dx_levels)
  target_df$dx_cum_sum <- annual_targets
  # return the original `df` with `target_df` attached
  bind_rows(df, target_df)
}




