# app.R

# # # # # 
## Load libraries ---- 

package.list <- c("shiny", "shinydashboard", "DT", "ggplot2", "RCurl", 
                  "jsonlite", "dplyr", "tidyr", "lubridate", "forcats",
                  "ggmap", "maps", "mapdata", "zipcode")
new.package.list <- 
  package.list[!(package.list %in% installed.packages()[, "Package"])]
if (length(new.package.list)) { 
  install.packages(new.package.list,
                   repos = "https://cloud.r-project.org/",
                   verbose = TRUE) 
}

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(ggmap)
library(maps)
library(mapdata)
library(zipcode)

deployed <- FALSE
dt_options <- list(paging = FALSE,
                   searching = FALSE,
                   ordering = FALSE,
                   info = FALSE)

# # # # #
## Source files ---- 

if (deployed) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard/" 
} else {
  path_to_app <- # local
    paste0("~/Documents/GitHub/UDSEnrollmentDashboard/",
           "UDSEnrollmentDashboardCron_0.2/") 
}

source(paste0(path_to_app, "helper_fxns_plots.R"), local = TRUE)


## ui ---- 
ui <- dashboardPage( 
  
  ## Dashboard skin color
  skin = "blue", 
  
  ## _ Header ----
  dashboardHeader(title = "MADC Dashboard"),
  
  ## _ Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Summary", tabName = "summary", 
               icon = icon("table")),
      menuItem(text = "Timelines", tabName = "timelines", 
               icon = icon("clock-o")),
      menuItem(text = "Plots", tabName = "plots", 
               icon = icon("signal")),
      menuItem(text = "Maps", tabName = "maps", 
               icon = icon("map")),
      menuItem(text = "Characterization", tabName = "characterization",
               icon = icon("medkit"))
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  ## _ Body ----
  dashboardBody(
    ## Set colors of font awesome icons
    tags$style(".fa-dashboard {color:#064193}"),
    tags$style(".fa-tint {color:#064193}"),
    tags$style(".fa-magnet {color:#064193}"),
    
    ## _ _ Tab container ----
    tabItems(
      tabItem( # _ _ _ Tab for summary tables ----
        tabName = "summary",
        h2("Summary Tables"),
        # fluidRow( # start fluidRow for valueBoxes
        #   # valueBox 1,2,3 here
        # ), # end fluidRow for valueBoxes
        fluidRow( box(width = 12, h3("UDS Version"), 
                      DT::dataTableOutput("uds_vers")) ), 
        fluidRow( box(width = 12, h3("Sex"), 
                      DT::dataTableOutput("sex")) ), 
        fluidRow( box(width = 12, h3("Race"),
                      DT::dataTableOutput("race")) ), 
        fluidRow( box(width = 12, h3("Research"),
                      DT::dataTableOutput("rsrch")) ), 
        fluidRow( box(width = 12, h3("Sex + Race"),
                      DT::dataTableOutput("sex_race")) ), 
        fluidRow( box(width = 12, h3("UDS Version + Research"),
                      DT::dataTableOutput("uds_rsrch")) ), 
        fluidRow( box(width = 12, h3("Sex + MRI Yes"),
                      DT::dataTableOutput("sex_mri_yes")) ),
        fluidRow( box(width = 12, h3("Race + MRI Yes"),
                      DT::dataTableOutput("race_mri_yes")) )
      ),
      tabItem( # _ _ _ Tab for timeline tables/plots ----
        tabName = "timelines",
        h2("Timelines"),
        fluidRow( 
          box(width = 12, h3("Participant Timelines"),
              DT::dataTableOutput("timeline")) ),
        fluidRow(
          box(width = 6, height = 350, h3("Visit to Scored"),
              plotOutput(outputId = "plot_timeline_exam_scored_hist")),
          box(width = 6, height = 350, h3("Visit to Double Scored"),
              plotOutput(outputId = "plot_timeline_exam_dbl_scored_hist"))
        ),
        fluidRow(
          box(width = 6, height = 350, h3("Visit to First Consensus"),
              plotOutput(outputId = "plot_timeline_exam_consensus_dur_hist")),
          box(width = 6, height = 350, h3("Final Consensus to Feedback"),
              plotOutput(outputId = "plot_timeline_final_consensus_fb_hist"))
        )
      ),
      tabItem( # _ _ _ Tab for plots ----
        tabName = "plots",
        h2("Cumulative Enrollments"),
        fluidRow(
          tabBox( # _ _ _ _ tabBox 1 for cumulative plots ----
            width = 12,
            title = "Cumulative Enrollments",
            id = "tabset_cumenroll",
            height = "550px",
            tabPanel( # _ _ _ _ _ tabPanel 1 -- Total plot ----
              title = "Total",
              box(width = 12,
                  plotOutput(outputId = "plot_cum_total"))
            ),
            tabPanel( # _ _ _ _ _ tabPanel 2 -- Sex plot ----
              title = "Sex",
              box(width = 12,
                  plotOutput(outputId = "plot_cum_sex"))
            ), 
            tabPanel( # _ _ _ _ _ tabPanel 3 -- Race plot ----
              title = "Race",
              box(width = 12,
                  plotOutput(outputId = "plot_cum_race"))
            ) 
          )
        ),
        fluidRow(
          box( # _ _ _ _ box 1 for date range input (cumulative) ----
            width = 12,
            dateRangeInput(inputId = "dateRange1",
                           label = "Date range input: yyyy-mm-dd",
                           start = as.Date("2017-03-01"), end = Sys.Date())
          )
        ),
        fluidRow(
          tabBox( # _ _ _ _ tabBox 2 for target diagnosis plots ----
            width = 12,
            title = "Target Enrollment by Diagnosis",
            id = "tabset_targenroll",
            height = "550px",
            tabPanel("NL", # _ _ _ _ _ tabPanel 1 -- NL ----
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_nl"))),
            tabPanel("MCI", # _ _ _ _ _ tabPanel 1 -- MCI ----
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_mci"))),
            tabPanel("AD", # _ _ _ _ _ tabPanel 1 -- AD ----
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_ad"))),
            tabPanel("LBD", # _ _ _ _ _ tabPanel 1 -- LBD ----
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_lbd"))),
            tabPanel("FTD", # _ _ _ _ _ tabPanel 1 -- FTD ----
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_ftd")))
          ) 
        ),
        fluidRow(
          box( # _ _ _ _ box 2 for date range input (diagnosis) ----
            width = 12,
            dateRangeInput(inputId = "dateRange2",
                           label = "Date range input: yyyy-mm-dd",
                           start = as.Date("2017-03-01"),
                           end = as.Date("2022-03-01")
            )
          )
        ) 
      ), 
      tabItem( # _ _ _ Tab for maps ----
        tabName = "maps",
        h2("Maps"),
        fluidRow(
          tabBox(
            width = 12,
            title = "",
            id = "tabset_map",
            height = "700px",
            side = "left",
            tabPanel( # _ _ _ _ tabPanel 1 -- county map ----
              title = "County",
              box(
                width = 12,
                height = "625px",
                plotOutput(outputId = "map_partic_by_county")
              )
            ), 
            tabPanel( # _ _ _ _ tabPanel 2 -- ZIP map ----
              title = "ZIP",
              box(
                width = 12,
                height = "625px",
                plotOutput(outputId = "map_partic_by_zip")
              )
            ) 
          ) # end tabBox
        ) # end fluidRow
      ), # end tabItem for Maps
      tabItem( # _ _ _ Tab for condition characterization ----
        tabName = "characterization",
        h2("Characterization"),
        fluidRow(
          box(
            checkboxGroupInput(
              inputId = "conditionsCheckboxes",
              label = h3("Conditions"),
              choices = list("Cancer" = "cancer", 
                             "Diabetes" = "diabet", 
                             "Hypertension" = "hypert",
                             "Sleep Apnea" = "sleepap",
                             "REM disorder" = "remdis",
                             "Hyposomnia/Insomnia" = "hyposom")),
            width = 12
          )),
        fluidRow(
          box( 
            verbatimTextOutput(
              outputId = "value"), 
            width = 12
          )
        )
      ) # end tabItem for Characterization
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

server <- function(input, output, session) {
  
  invalidation_time <- 1000 * 60 * 60 * 6 # 6-hour refresh
  #                    ^      ^    ^    ^
  #                    |      |    |    |> 6 hr
  #                    |      |    |> 60 min / hr
  #                    |      |> 60 sec / min
  #                    |> 1000 ms / sec
  # invalidation_time <- 1000 * 60 * 5 # 5-minute refresh (debug)
  
  # # # # # 
  ## Get data ----
  
  ## Raw MiNDSet data
  # data <- readRDS(paste0(path_to_app, "rds/df_mindset_xfrm.Rds"))
  data <- 
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/df_mindset_xfrm.Rds",
                       readFunc = readRDS,
                       session = NULL)
  
  ## List for summary tables
  lst_summ_tbls <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/lst_summ_tbls.Rds",
                       readFunc = readRDS,
                       session = NULL)
  
  ## List for timeline tables
  lst_timeline_tbls <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/lst_timeline_tbls.Rds",
                       readFunc = readRDS,
                       session = NULL)
  
  ## df for plots
  data_plots <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/data_plots.Rds",
                       readFunc = readRDS,
                       session = NULL)
  
  ## List for maps
  lst_map_dfs <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/lst_map_dfs.Rds",
                       readFunc = readRDS,
                       session = NULL)
  
  
  # # # # # 
  ## Render summary tables ----
  
  ## Use `observe` + `lapply` to render all the summary tables
  summ_tbl_names <- c("uds_vers", "sex", "race", "sex_race", 
                      "rsrch", "uds_rsrch", "sex_mri_yes", "race_mri_yes")
  observe({
    lapply(summ_tbl_names, function(tbl_name) {
      output[[tbl_name]] <- DT::renderDataTable({
        DT::datatable( lst_summ_tbls()[[paste0(tbl_name, "_tbl")]], 
                       options = dt_options )
      })
    }) # end `lapply`
  }) # end `observe`
  
  ## Example of a single table render
  # output$data_mindset_tbl <- DT::renderDataTable({ 
  #   DT::datatable( data_mindset_rctv(), options = dt_options ) 
  # })
  
  # # # # #
  ## Render timeline tables ----
  
  ## Use `observe` + `lapply` to render all the summary tables
  timeline_tbl_names <- c("timeline")
  observe({
    lapply(timeline_tbl_names, function(tbl_name) {
      output[[tbl_name]] <- DT::renderDataTable({
        DT::datatable( lst_timeline_tbls()[[paste0(tbl_name, "_tbl")]], 
                       options = dt_options )
      })
    }) # end `lapply`
  }) # end `observe`
  
  # # # # # 
  ## Render enrollment plots ----
  
  ## Timeline - Visit to Score plot
  output$plot_timeline_exam_scored_hist <- renderPlot({
    ggplot(data = data(), aes(x = exam_scored_dur)) +
      geom_histogram(binwidth = 5, center = 2.5, 
                     color = "#000000", fill = "#3885B7",
                     na.rm = TRUE) +
      scale_x_continuous(name = "Days")
  }, height = 275)
  
  ## Timeline - Visit to Double Score plot
  output$plot_timeline_exam_dbl_scored_hist <- renderPlot({
    ggplot(data = data(), aes(x = exam_dbl_scored_dur)) +
      geom_histogram(binwidth = 5, center = 2.5, 
                     color = "#000000", fill = "#3885B7",
                     na.rm = TRUE) +
      scale_x_continuous(name = "Days")
  }, height = 275)
  
  ## Timeline - Visit to First Consensus plot
  output$plot_timeline_exam_consensus_dur_hist <- renderPlot({
    ggplot(data = data(), aes(x = exam_consensus_dur)) +
      geom_histogram(binwidth = 25, center = 12.5, 
                     color = "#000000", fill = "#3885B7",
                     na.rm = TRUE) +
      scale_x_continuous(name = "Days")
  }, height = 275)
  
  ## Timeline - Final Consensus to Feedback plot
  output$plot_timeline_final_consensus_fb_hist <- renderPlot({
    ggplot(data = data(), aes(x = final_consensus_fb_dur)) +
      geom_histogram(binwidth = 25, center = 12.5, 
                     color = "#000000", fill = "#3885B7",
                     na.rm = TRUE) +
      scale_x_continuous(name = "Days")
  }, height = 275)
  
  ## Cumulative enrollment totals plot
  output$plot_cum_total <- renderPlot({
    cum_plot(df = data_plots(),
             x = "exam_date", y = "total_cum_sum",
             plot_title = "Total Participants Over Time",
             start_date = as.Date(input$dateRange1[1]),
             end_date = as.Date(input$dateRange1[2]))
  })
  
  ## Cumulative enrollment by sex plot
  output$plot_cum_sex <- renderPlot({
    cum_plot_single_grp(df = data_plots(),
                        x = "exam_date", y = "sex_cum_sum",
                        group_var = "sex_value",
                        plot_title = "Participants Over Time by Sex",
                        start_date = as.Date(input$dateRange1[1]),
                        end_date = as.Date(input$dateRange1[2]))
  })
  
  ## Cumulative enrollment by race plot
  output$plot_cum_race <- renderPlot({
    cum_plot_single_grp(df = data_plots(),
                        x = "exam_date", y = "race_cum_sum",
                        group_var = "race_value",
                        plot_title = "Participants Over Time by Race",
                        start_date = as.Date(input$dateRange1[1]),
                        end_date = as.Date(input$dateRange1[2]))
  })
  
  ## Cumulative enrollment by diagnosis vs. diagnosis targets
  # Use `observe` + `lapply` to render all the target diagnosis plots
  diagnosis_abbrevs <- c("AD", "FTD", "LBD", "MCI", "NL")
  observe({
    lapply(diagnosis_abbrevs, function(dx_abrv) {
      output[[paste0("plot_cum_dx_target_", tolower(dx_abrv))]] <-
        renderPlot({
          cum_plot_dx_target_dx(df = data_plots(),
                                x = "exam_date", y = "dx_cum_sum",
                                group_var = "uds_dx",
                                dx = dx_abrv, 
                                dx_target = paste0(dx_abrv, " target"),
                                plot_title = paste0(dx_abrv, " vs. ", 
                                                    dx_abrv, " Target"),
                                start_date = as.Date(input$dateRange2[1]),
                                end_date = as.Date(input$dateRange2[2]))
        }) # end renderPlot
    }) # end lapply
  }) # end observe
  
  # # # # # 
  ## Render enrollment maps ----
  
  ## Participation by county map
  county_max <- reactive({
    max(lst_map_dfs()$map_df_partic_ct_mi_county$Count, na.rm = TRUE)
  })
  output$map_partic_by_county <- renderPlot({
    lst_map_dfs()$mi_base_map +
      geom_polygon(data = lst_map_dfs()$map_df_partic_ct_mi_county,
                   aes(fill = Count),
                   color = "black", size = 0.1) +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      ) +
      scale_fill_continuous(low = "#eeeeee", high = "royalblue",
                            breaks = seq(1, county_max(),
                                         by = (county_max()-1)/5)) +
      ggtitle(label = "Participant Counts by County",
              subtitle = "March 2017 to Present")
  }, height = 600)
  
  ## Participation by ZIP map
  zip_max <- reactive({
    max(lst_map_dfs()$map_df_partic_ct_mi_zip$Count, na.rm = TRUE)
  })
  output$map_partic_by_zip <- renderPlot({
    lst_map_dfs()$mi_base_map +
      geom_point(data = lst_map_dfs()$map_df_partic_ct_mi_zip,
                 aes(x = longitude, y = latitude, group = zip,
                     size = Count,
                     fill = Count),
                 color = "black", pch = 21) +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      ) +
      scale_fill_continuous(low = "#eeeeee", high = "royalblue",
                            breaks = seq(1, zip_max(), 
                                         by = round((zip_max()-1)/5))) +
      guides(fill = guide_legend(), size = guide_legend()) +
      scale_size_continuous(limits = c(1, zip_max()),
                            breaks = seq(1, zip_max(),
                                         by = round((zip_max()-1)/5))) +
      ggtitle(label = "Participant Counts by ZIP Code",
              subtitle = "March 2017 to Present")
  }, height = 600)
  
  # # # # #
  ## Render characterization plots ----
  output$value <- renderPrint({ input$conditionsCheckboxes })
  
}

shinyApp(ui, server)




