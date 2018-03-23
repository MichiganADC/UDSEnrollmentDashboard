# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Create dataframe for summary table / stats: 
##   input:  `summ_tbl`, ...
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Load libraries ----
###
library(shinydashboard)
library(DT)
library(ggplot2)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Source `report_df_procsd` and helper functions ----
###

operational <- TRUE
# table_menu_lengths <- c(5, 10, 15)
dt_options <- list(paging = FALSE,
                   searching = FALSE,
                   ordering = FALSE,
                   info = FALSE)

if (operational) {  ### OPERATIONAL ###
  source("./report_df_summ_processor.R", local = TRUE)
  source("./report_df_plots_processor.R", local = TRUE)
  source("./report_df_maps_processor.R", local = TRUE)
  source("./plots_helper_fxns.R", local = TRUE)
} else {            ### DEBUGGING ###
  source("./UDSEnrollmentDashboard/report_df_summ_processor.R", local = TRUE)
  source("./UDSEnrollmentDashboard/report_df_plots_processor.R", local = TRUE)
  source("./UDSEnrollmentDashboard/report_df_maps_processor.R", local = TRUE)
  source("./UDSEnrollmentDashboard/plots_helper_fxns.R", local = TRUE)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## ui ----
###

ui <- dashboardPage(
  ## Skin color
  skin = "blue",
  
  ## Header ----
  dashboardHeader(title = "MADC Dashboard"),
  
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
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "Blood Drawn Yes"]),
                         subtitle = "Blood Drawn", icon = icon("tint"), color = "navy"),
                valueBox(value = as.integer(summ_tbl[summ_tbl$uds_dx == "Totals", "MRI Yes"]),
                         subtitle = "MRI Completed", icon = icon("magnet"), color = "navy")
              ),
              fluidRow(
                box( ## Sex table
                  width = 12,
                  h3("Sex"),
                  # tableOutput("sex")
                  DT::dataTableOutput("sex")
                ),
                box( ## Race table
                  width = 12,
                  h3("Race"),
                  # tableOutput("race")
                  DT::dataTableOutput("race")
                )
              ),
              fluidRow(
                box( ## UDS Version table
                  width = 12,
                  h3("UDS Version"),
                  # tableOutput("uds_vers")
                  DT::dataTableOutput("uds_vers")
                ),
                box( ## Research table
                  width = 12,
                  h3("Research"),
                  # tableOutput("research")
                  DT::dataTableOutput("research")
                )
              ),
              fluidRow(
                box( ## UDS 3 Visit table
                  width = 4,
                  h3("UDS 3.0 Visits*"),
                  DT::dataTableOutput("uds3_visit"),
                  h6("* 19 participants started at Visit 2")
                )
              ),
              fluidRow(
                box( ## Sex x Race table
                  width = 12,
                  h3("Sex + Race"),
                  # tableOutput("sex_race")
                  DT::dataTableOutput("sex_race")
                )
              ),
              fluidRow(
                box( ## UDS Version + Research table
                  width = 12,
                  h3("UDS Version + Research"),
                  # tableOutput("uds_research")
                  DT::dataTableOutput("uds_research")
                )
              )
      ), ## end tabItem (first)
      
      ## Second tab content ---- Plots
      tabItem(tabName = "plots",
              h2("Cumulative Enrollment"),
              fluidRow(
                tabBox(
                  title = "Cumulative Enrollment",
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
                               plotOutput(outputId = "plot_cum_race")))
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
      ), ## end tabItem (second)
      
      tabItem(tabName = "maps",
              h2("Maps"),
              fluidRow(
                tabBox(
                  title = "",
                  id = "map_tabset",
                  height = "700px",
                  # width = 12,
                  side = "left",
                  tabPanel(
                    "County",
                    box(
                      width = 12,
                      height = "625px",
                      plotOutput(outputId = "map_partic_by_county")
                    )
                  ), # end tabPanel 1
                  tabPanel(
                    "ZIP",
                    box(
                      width = 12,
                      height = "625px",
                      plotOutput(outputId = "map_partic_by_zip")
                    )
                  ) # end tabPanel 2
                ) # end tabBox
              ), # end fluid row
              fluidRow(
                # box(
                #   width = 12,
                #   height = "625px",
                #   plotOutput(outputId = "map_partic_by_county")
                # )
              ), # end fluid row 1
              fluidRow(
                # box(
                #   width = 12,
                #   height = "625px",
                #   plotOutput(outputId = "map_partic_by_zip")
                # )
              )
      ) ## end tabItem (third)
      
    ) ## end tabItems
  ) ## end dashboardBody
) ## end dashboardPage

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## server ----
###

server <- function(input, output) {

  # ## NOT USEFUL -- Summary table output ----
  # output$summary <- renderTable({
  #   summ_tbl
  # })
  
  ## Sex table output ----
  # output$sex <- renderTable({ sex_tbl })
  output$sex <- DT::renderDataTable({
    DT::datatable(sex_tbl, options = dt_options)
  })
  
  ## Race table output ----
  # output$race <- renderTable({ race_tbl })
  output$race <- DT::renderDataTable({
    DT::datatable(race_tbl, options = dt_options)
  })
  
  ## UDS Version table output ----
  # output$uds_vers <- renderTable({ uds_vers_tbl })
  output$uds_vers <- DT::renderDataTable({
    DT::datatable(uds_vers_tbl, options = dt_options)
  })
  
  ## Research table output ----
  # output$research <- renderTable({ rsrch_tbl })
  output$research <- DT::renderDataTable({
    DT::datatable(rsrch_tbl, options = dt_options)
  })
  
  ## UDS 3.0 Visit table output
  # output$uds3_visit <- renderTable({ uds3_visit_tbl })
  output$uds3_visit <- DT::renderDataTable({
    DT::datatable(uds3_visit_tbl, options = dt_options)
  })
  
  ## Sex x Race table output ----
  # output$sex_race <- renderTable({ sex_race_tbl })
  output$sex_race <- DT::renderDataTable({
    DT::datatable(sex_race_tbl, options = dt_options)
  })
  
  ## UDS Version + Autopsy output ----
  # output$uds_autopsy <- renderTable({ uds_autopsy_tbl })
  output$uds_research <- DT::renderDataTable({
    DT::datatable(uds_rsrch_tbl, options = dt_options)
  })
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  ## Cumulative enrollment totals ----
  output$plot_cum_total <- renderPlot({
    cum_plot(df = report_df_plots, 
             x = "exam_date", y = "total_cum_sum", 
             plot_title = "Total Participants Over Time",
             start_date = as.Date(input$dateRange1[1]), 
             end_date = as.Date(input$dateRange1[2]))
  })
  
  # ## NOT USEFUL -- Cumulative enrollment by diagnosis plot ----
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  ## Participation by County map
  output$map_partic_by_county <- renderPlot({
    mi_base_map +
      geom_polygon(data = mi_counties_partic_count, aes(fill = Count),
                   color = "black", size = 0.1) +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      nix_lat_long_grid +
      scale_fill_gradient(low = "#FFFFFF", high = "darkblue",
                          breaks = c(1, 20, 40, 60, 80, 100)) +
      ggtitle(label = "Participant Counts by County", 
              subtitle = "March 2017 to Present")
  }, height = 600)

  ## Participation by ZIP map
  zip_max <- max(mi_zips_partic_count$Count, na.rm = T)
  output$map_partic_by_zip <- renderPlot({
    mi_base_map +
      geom_point(data = mi_zips_partic_count,
                 aes(x = longitude, y = latitude, group = zip, 
                     size = Count,
                     fill = Count), 
                 color = "black", pch = 21) +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      nix_lat_long_grid +
      scale_fill_continuous(low = "#eeeeee", high = "royalblue", 
                            breaks = seq(1, zip_max, by = 2)) +
      guides(fill = guide_legend(), size = guide_legend()) +
      scale_size_continuous(limits = c(1,zip_max), 
                            breaks = seq(1, zip_max, by = 2)) +
      ggtitle(label = "Participant Counts by ZIP Code", 
              subtitle = "March 2017 to Present")
  }, height = 600)
  
} # end server function


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## shiny app ----
###

shinyApp(ui, server)





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # #     EXTRA  SPACE    # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 