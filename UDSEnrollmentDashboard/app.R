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

################################################################################
## ui
#####

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
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Total"]),
                         subtitle = "Total Enrolled", icon = icon("dashboard"), color = "navy"),
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Blood_Drawn"]),
                         subtitle = "Blood Drawn", icon = icon("tint"), color = "navy"),
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "MRI_Yes"]),
                         subtitle = "MRI Completed", icon = icon("magnet"), color = "navy")
              ),
              fluidRow(
                box(
                  width = 6,
                  h3("Totals"),
                  tableOutput("totals") ## Totals table
                ),
                box(
                  width = 6,
                  h3("Research Data"),
                  tableOutput("research") ## Research table
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  h3("Sex Data"),
                  tableOutput("sex") ## Sex table
                ),
                box(
                  width = 6,
                  h3("Race Data"),
                  tableOutput("race") # Race table
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  h3("Sex + Race Data"),
                  tableOutput("sex_race") # Sex x Race table
                )
              )
      ), ## end tabItem (first)
      
      ## Second tab content ---- Plots
      tabItem(tabName = "plots",
              h2("Cumulative Enrollment"),
              fluidRow(
                tabBox(
                  title = "Cumulative Enrollment",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", 
                  height = "550px",
                  tabPanel("Total",
                           box(width = 12, 
                               plotOutput(outputId = "plot_cum_total"))),
                  tabPanel("Sex", 
                           box(width = 12, 
                               plotOutput(outputId = "plot_cum_sex"))),
                  tabPanel("Race", 
                           box(width = 12, 
                               plotOutput(outputId = "plot_cum_race"))) #,
                  # tabPanel("Diagnosis", 
                  #          box(width = 12, 
                  #              plotOutput(outputId = "plot_cum_dx")))
                ),
                tabBox(
                  title = "Target Enrollment by Diagnosis",
                  id = "tabset2",
                  height = "550px",
                  side = "right", 
                  tabPanel("NL",
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_nl"))),
                  tabPanel("MCI",
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_mci"))),
                  tabPanel("LBD",
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_lbd"))), 
                  tabPanel("FTD",
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_ftd"))),
                  tabPanel("AD",
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_ad")))
                )
              ), # end fluid row 1
              fluidRow(
                box(
                  width = 6,
                  # Date range input ----
                  dateRangeInput(inputId = "dateRange1",
                                 label = "Date range input: yyyy-mm-dd",
                                 start = as.Date("2017-03-01"), end = Sys.Date()
                  ) #,
                  # verbatimTextOutput("dateRange1Text1"),
                  # verbatimTextOutput("dateRange1Text2")
                ),
                box(
                  width = 6,
                  # Date range input ----
                  dateRangeInput(inputId = "dateRange2",
                                 label = "Date range input: yyyy-mm-dd",
                                 start = as.Date("2017-03-01"), end = as.Date("2022-03-01")
                  ) #,
                  # verbatimTextOutput("dateRange2Text1"),
                  # verbatimTextOutput("dateRange2Text2")
                )
              ) # end fluid row 2
      ) ## end tabItem (second)
      
    ) ## end tabItems
  ) ## end dashboardBody
) ## end dashboardPage

################################################################################
## server
#####

server <- function(input, output) {
  # ## Date testing
  # output$dateRange1Text1 <- renderText({
  #   paste(input$dateRange1[1])
  # })
  # output$dateRange1Text2 <- renderText({
  #   paste(input$dateRange1[2])
  # })
  # output$dateRange2Text1 <- renderText({
  #   paste(input$dateRange2[1])
  # })
  # output$dateRange2Text2 <- renderText({
  #   paste(input$dateRange2[2])
  # })
  
  ################################################################################
  
  ## Summary table output ----
  output$summary <- renderTable({
    summ_tbl
  })
  
  ## Total table output ----
  output$totals <- renderTable({
    total_tbl
  })
  
  # ## Demographic table output ----
  # output$demographic <- renderTable({
  #   demo_tbl
  # })
  ## Sex table output ----
  output$sex <- renderTable({
    sex_tbl
  })
  
  ## Race table output ----
  output$race <- renderTable({
    race_tbl
  })
  
  ## Sex x Race table output ----
  output$sex_race <- renderTable({
    sex_race_tbl
  })
  
  ## Research table output ----
  output$research <- renderTable({
    rsrch_tbl
  })
  
  ################################################################################
  
  ## Cumulative enrollment totals ----
  output$plot_cum_total <- renderPlot({
    cum_plot(df = report_df_plots, 
             x = "exam_date", y = "total_cum_sum", 
             plot_title = "Total Participants Over Time",
             start_date = as.Date(input$dateRange1[1]), 
             end_date = as.Date(input$dateRange1[2]))
  })
  
  # ## Cumulative enrollment by diagnosis plot ----
  # output$plot_cum_dx <- renderPlot({
  #   cum_plot_single_grp(df = report_df_plots,
  #                       x = "exam_date", y = "dx_cum_sum",
  #                       group_var = "uds_dx",
  #                       plot_title = "Participants Over Time by Diagnosis")
  # })
  
  ## Cumulative enrollment by sex plot ----
  output$plot_cum_sex <- renderPlot({
    cum_plot_single_grp(df = report_df_plots, 
                        x = "exam_date", y = "sex_cum_sum", 
                        group_var = "sex_value",
                        plot_title = "Participants Over Time by Sex",
                        start_date = as.Date(input$dateRange1[1]), 
                        end_date = as.Date(input$dateRange1[2]))
  })
  
  ## Cumulative enrollment by race plot ----
  output$plot_cum_race <- renderPlot({
    cum_plot_single_grp(df = report_df_plots,
                        x = "exam_date", y = "race_cum_sum",
                        group_var = "race_value",
                        plot_title = "Participants Over Time by Race",
                        start_date = as.Date(input$dateRange1[1]), 
                        end_date = as.Date(input$dateRange1[2]))
  })
  
  ## Cumulative enrollment target: AD ----
  output$plot_cum_dx_target_ad <- renderPlot({
    cum_plot_dx_target_dx(df = report_df_plots,
                          x = "exam_date", y = "dx_cum_sum",
                          group_var = "uds_dx",
                          dx = "AD", dx_target = "AD target",
                          plot_title = "AD vs. AD Target",
                          start_date = as.Date(input$dateRange2[1]), 
                          end_date = as.Date(input$dateRange2[2]))
  })
  
  ## Cumulative enrollment target: FTD ----
  output$plot_cum_dx_target_ftd <- renderPlot({
    cum_plot_dx_target_dx(df = report_df_plots,
                          x = "exam_date", y = "dx_cum_sum",
                          group_var = "uds_dx",
                          dx = "FTD", dx_target = "FTD target",
                          plot_title = "FTD vs. FTD Target",
                          start_date = as.Date(input$dateRange2[1]), 
                          end_date = as.Date(input$dateRange2[2]))
  })
  
  ## Cumulative enrollment target: LBD ----
  output$plot_cum_dx_target_lbd <- renderPlot({
    cum_plot_dx_target_dx(df = report_df_plots,
                          x = "exam_date", y = "dx_cum_sum",
                          group_var = "uds_dx",
                          dx = "LBD", dx_target = "LBD target",
                          plot_title = "LBD vs. LBD Target",
                          start_date = as.Date(input$dateRange2[1]), 
                          end_date = as.Date(input$dateRange2[2]))
  })
  
  ## Cumulative enrollment target: MCI ----
  output$plot_cum_dx_target_mci <- renderPlot({
    cum_plot_dx_target_dx(df = report_df_plots,
                          x = "exam_date", y = "dx_cum_sum",
                          group_var = "uds_dx",
                          dx = "MCI", dx_target = "MCI target",
                          plot_title = "MCI vs. MCI Target",
                          start_date = as.Date(input$dateRange2[1]), 
                          end_date = as.Date(input$dateRange2[2]))
  })
  
  ## Cumulative enrollment target: NL ----
  output$plot_cum_dx_target_nl <- renderPlot({
    cum_plot_dx_target_dx(df = report_df_plots,
                          x = "exam_date", y = "dx_cum_sum",
                          group_var = "uds_dx",
                          dx = "NL", dx_target = "NL target",
                          plot_title = "NL vs. NL Target",
                          start_date = as.Date(input$dateRange2[1]), 
                          end_date = as.Date(input$dateRange2[2]))
  })
}

################################################################################
## shiny app
#####

shinyApp(ui, server)





################################################################################
################################################################################
##############################    EXTRA  SPACE    ##############################
################################################################################
################################################################################