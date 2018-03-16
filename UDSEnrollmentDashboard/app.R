####
# Shiny Dashboard app to summarize useful UDS enrollment data
##
# library(shiny)
library(shinydashboard)
library(ggplot2)

source("./uds_summary_table.R", local = TRUE) # gets summ_tble
source("./uds_plots.R", local = TRUE) # gets various plots
# source("./uds_plots_fxns.R", local = TRUE) # gets plot helper fxns

ui <- dashboardPage(
  # Skin color
  skin = "blue",
  
  # Header ----
  dashboardHeader(title = "Basic dashboard"),
  
  # Sidebar ----
  dashboardSidebar(
    # Sidebar menu ----
    sidebarMenu(
      menuItem(text = "Summary", tabName = "summary", icon = icon("table")),
      menuItem(text = "Graphs", tabName = "graphs", icon = icon("signal")),
      menuItem(text = "Maps", tabName = "maps", icon = icon("map"))
    )
  ), # end dashboardSideBar
  
  # Body
  dashboardBody(
    tags$style(".fa-dashboard {color:#064193}"),
    tags$style(".fa-tint {color:#064193}"),
    tags$style(".fa-magnet {color:#064193}"),
    
    # Tab container ----
    tabItems(
      
      # First tab content
      tabItem(tabName = "summary",
              # Summary tab
              h2("Summary"),
              fluidRow(
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Count"]), 
                         subtitle = "Total Enrolled", icon = icon("dashboard"), color = "navy"),
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Blood_Drawn"]),
                         subtitle = "Blood Drawn", icon = icon("tint"), color = "navy"),
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "MRI_Yes"]),
                         subtitle = "MRI Completed", icon = icon("magnet"), color = "navy")
              ),
              h2("Summary Table"),
              fluidRow(
                tableOutput("summary")
              )
      ), # end 1st tab
      
      # Second tab content
      tabItem(tabName = "graphs",
              h2("Cumulative Enrollment"),
              fluidRow(
                box(width = 6, plotOutput(outputId = "plot_cum_total")),
                box(width = 6, plotOutput(outputId = "plot_cum_dx"))
              ),
              fluidRow(
                box(width = 6, plotOutput(outputId = "plot_cum_sex")),
                box(width = 6, plotOutput(outputId = "plot_cum_race"))
              ),
              h2("Cumulative vs. Target Enrollment by Diagnosis")
      ), # end 2nd tab
      
      # Third tab content
      tabItem(tabName = "maps",
              h1("Maps tab content")
      ) # end 3rd tab
      
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

server <- function(input, output) {
  
  # Summary table output
  output$summary <- renderTable({
    head(summ_tbl, n = nrow(summ_tbl))
  })
  
  # Cumulative participant enrollment plot
  output$plot_cum_total <- renderPlot({
    ggplot(report_df, aes(x = exam_date, y = as.numeric(rownames(report_df)))) +
      geom_line() +
      geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
      scale_x_date(name = "Visit Date",
                   date_labels = "%b %y",
                   date_breaks = "1 month",
                   date_minor_breaks = "1 month",
                   limits = as.Date(c("2017-03-01", Sys.Date()))) +
      scale_y_continuous(name = "Cumulative Participants",
                         breaks = seq(0, nrow(report_df) + 10, by = 10)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(label = "Total Participants Over Time")
  })
  
  # Cumulative participant enrollment by diagnosis plot
  output$plot_cum_dx <- renderPlot({
    ggplot(report_df, aes(x = exam_date, y = DxCumSum, group = uds_dx, color = uds_dx)) +
      geom_line() +
      geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
      scale_x_date(name = "Visit Date",
                   date_labels = "%b %y",
                   date_breaks = "1 month",
                   date_minor_breaks = "1 month",
                   limits = as.Date(c("2017-03-01", Sys.Date()))) +
      scale_y_continuous(name = "Cumulative Participants",
                         breaks = seq(0, nrow(report_df) + 10, by = 10)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(label = "Participants Over Time by Diagnosis")
  })
  
  # Cumulative participant enrollment by sex plot
  output$plot_cum_sex <- renderPlot({
    ggplot(report_df, aes(x = exam_date, y = SexCumSum, group = sex_value, color = sex_value)) + 
      geom_line() +
      geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
      scale_x_date(name = "Visit Date",
                   date_labels = "%b %y",
                   date_breaks = "1 month",
                   date_minor_breaks = "1 month",
                   limits = as.Date(c("2017-03-01", Sys.Date()))) +
      scale_y_continuous(name = "Cumulative Participants",
                         breaks = seq(0, nrow(report_df) + 10, by = 10)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(label = "Participants Over Time by Sex")
  })
  
  # Cumulative participant enrollment by race plot
  output$plot_cum_race <- renderPlot({
    ggplot(report_df, aes(x = exam_date, y = RaceCumSum, group = race_value, color = race_value)) + 
      geom_line() +
      geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
      scale_x_date(name = "Visit Date",
                   date_labels = "%b %y",
                   date_breaks = "1 month",
                   date_minor_breaks = "1 month",
                   limits = as.Date(c("2017-03-01", Sys.Date()))) +
      scale_y_continuous(name = "Cumulative Participants",
                         breaks = seq(0, nrow(report_df) + 10, by = 10)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(label = "Participants Over Time by Race")
  })
  
  # set.seed(122)
  # histdata <- rnorm(500)
  # 
  # output$plot1 <- renderPlot({
  #   data <- histdata[seq_len(input$slider)]
  #   hist(data)
  # })
  
}

shinyApp(ui, server)



