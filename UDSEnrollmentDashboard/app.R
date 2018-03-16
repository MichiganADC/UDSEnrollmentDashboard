################################################################################
## Create dataframe for summary table / stats: 
##   input:  `summ_tbl`, ...
################################################################################

################################################################################
## Load libraries
#####
library(shinydashboard)
library(ggplot2)

################################################################################
## Source `report_df_procsd` and helper functions
#####

operational = TRUE

if (operational) {  ### OPERATIONAL ###
  source("./report_df_summ_processor.R", local = TRUE)
  source("./report_df_plots_processor.R", local = TRUE)
  source("./plots_helper_fxns.R", local = TRUE)
} else {            ### DEBUGGING ###
  source("./UDSEnrollmentDashboard/report_df_summ_processor.R", local = TRUE)
  source("./UDSEnrollmentDashboard/report_df_plots_processor.R", local = TRUE)
  source("./UDSEnrollmentDashboard/plots_helper_fxns.R", local = TRUE)
}

ui <- dashboardPage(
  ## Skin color
  skin = "blue",
  
  ## Header ----
  dashboardHeader(title = "UDS Enrollment Dashboard"),
  
  ## Sidebar ----
  dashboardSidebar(
    # Sidebar menu ----
    sidebarMenu(
      menuItem(text = "Summary", tabName = "summary", icon = icon("table")),
      menuItem(text = "Plots", tabName = "plots", icon = icon("signal")),
      menuItem(text = "Maps", tabName = "maps", icon = icon("map"))
    )
  ), ## end sidebar
  
  ## Body ----
  dashboardBody(
    ## Set colors of font awesome icons
    tags$style(".fa-dashboard {color:#064193}"),
    tags$style(".fa-tint {color:#064193}"),
    tags$style(".fa-magnet {color:#064193}"),
    
    ## Tab container ----
    tabItems(
      ## First tab content ---- Summary
      tabItem(tabName = "summary",
              h2("Summary"),
              fluidRow(
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Count"]),
                         subtitle = "Total Enrolled", icon = icon("dashboard"), color = "navy"),
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Blood_Drawn"]),
                         subtitle = "Blood Drawn", icon = icon("tint"), color = "navy"),
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "MRI_Yes"]),
                         subtitle = "MRI Completed", icon = icon("magnet"), color = "navy")
              ),
              fluidRow(
                tableOutput("summary")
              )
      ), ## end tabItem (first)
      
      ## Second tab content ---- Plots
      tabItem(tabName = "plots",
              h2("Cumulative Enrollment"),
              fluidRow(
                box(width = 6, plotOutput(outputId = "plot_cum_total")),
                box(width = 6, plotOutput(outputId = "plot_cum_dx"))
              ),
              fluidRow(
                box(width = 6, plotOutput(outputId = "plot_cum_sex")),
                box(width = 6, plotOutput(outputId = "plot_cum_race"))
              )
      ) ## end tabItem (second)
      
    ) ## end tabItems
  ) ## end dashboardBody
) ## end dashboardPage

server <- function(input, output) {
  ## Summary table output ----
  output$summary <- renderTable({
    summ_tbl
  })
  
  ## Cumulative enrollment totals ----
  output$plot_cum_total <- renderPlot({
    cum_plot(df = report_df_plots, 
             x = "exam_date", y = "total_cum_sum", 
             plot_title = "Total Participants Over Time")
  })
  
  # blah <- report_df_plots %>% 
  #   filter(!stringr::str_detect(uds_dx, "target"))
  
  ## Cumulative enrollment by diagnosis plot ----
  output$plot_cum_dx <- renderPlot({
    cum_plot_single_grp(df = report_df_plots,
                        x = "exam_date", y = "dx_cum_sum",
                        group_var = "uds_dx",
                        plot_title = "Participants Over Time by Diagnosis")
  })
  
  ## Cumulative enrollment by sex plot ----
  output$plot_cum_sex <- renderPlot({
    cum_plot_single_grp(df = report_df_plots, 
                        x = "exam_date", y = "sex_cum_sum", 
                        group_var = "sex_value",
                        plot_title = "Participants Over Time by Sex")
  })
  
  ## Cumulative enrollment by race plot ----
  output$plot_cum_race <- renderPlot({
    cum_plot_single_grp(df = report_df_plots,
                        x = "exam_date", y = "race_cum_sum",
                        group_var = "race_value",
                        plot_title = "Participants Over Time by Race")
  })
}

shinyApp(ui, server)





################################################################################
################################################################################
##############################    EXTRA  SPACE    ##############################
################################################################################
################################################################################