# uds_plots_fxns.R

# Target diagnoses plot function
plot_target_dx <- function(ms_df, diagnosis, diagnosis_target, yearly_targets) {
  target_df <- data.frame(matrix(rep(NA, ncol(ms_df) * 6), nrow = 6, byrow = TRUE))
  names(target_df) <- names(ms_df)
  target_df$Subject_Id <- paste0("UM0000XXX", 0:5)
  target_df$exam_date <- as.Date(paste0(2017:2022, "-03-01"))
  target_df$uds_dx <- rep(diagnosis_target, 6)
  target_df$DxCumSum <- yearly_targets
  ms_df %>%
    dplyr::filter(uds_dx == diagnosis) %>%
    dplyr::bind_rows(target_df) %>%
    ggplot(aes(x = exam_date, y = DxCumSum, group = uds_dx, color = uds_dx, linetype = uds_dx)) +
    geom_line() +
    geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %y",
                 date_breaks = "6 months",
                 date_minor_breaks = "1 month",
                 limits = as.Date(c("2017-01-01", "2022-03-01"))) +
    scale_y_continuous(name = "Cumulative Participants",
                       breaks = seq(0, nrow(report_df) + 10, by = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(label = paste0("Participants Over Time - ", diagnosis, " vs. ", diagnosis_target))
}