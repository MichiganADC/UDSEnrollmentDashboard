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

# operational <- TRUE
deployed <- TRUE
# deployed <- FALSE
dt_options <- list(paging = FALSE,
                   searching = FALSE,
                   ordering = FALSE,
                   info = FALSE)

# # # # #
## Source files ---- 

# if (operational) {
#   # df_mindset_xfrm <- readRDS("df_mindset_xfrm.Rds")
#   # source("build_lst_summ_tbls.R", local = TRUE)
#   # source("build_data_plots.R", local = TRUE)
#   # source("build_lst_map_dfs.R", local = TRUE)
#   source("helper_fxns_plots.R", local = TRUE)
# } else {
#   # df_mindset_xfrm <- readRDS("./UDSEnrollmentDashboardCron/df_mindset_xfrm.Rds")
#   # source("./UDSEnrollmentDashboardCron/build_lst_summ_tbls.R", local = TRUE)
#   # source("./UDSEnrollmentDashboardCron/build_data_plots.R", local = TRUE)
#   # source("./UDSEnrollmentDashboardCron/build_lst_map_dfs.R", local = TRUE)
#   source("./UDSEnrollmentDashboardCron/helper_fxns_plots.R", local = TRUE)
# }

if (deployed) {
  path_to_app <-
    "~/ShinyApps/MADCDashboard/" # Michigan Medicine R Shiny server
} else {
  path_to_app <-
    "~/Documents/GitHub/UDSEnrollmentDashboard/UDSEnrollmentDashboardCron/" # local
}

source(paste0(path_to_app, "helper_fxns_plots.R"), local = TRUE)

# ## List for summary tables
# lst_summ_tbls <- build_lst_summ_tbls(df_mindset_xfrm)
# 
# ## df for plots
# data_plots <- build_data_plots(df_mindset_xfrm)
# 
# ## List for maps
# lst_map_dfs <- build_lst_map_dfs(df_mindset_xfrm)

## ui ---- 
ui <- dashboardPage( 
  
  ## Dashboard skin color
  skin = "blue", 
  
  ## Header ----
  dashboardHeader(title = "MADC Dashboard"),
  
  ## Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Summary", tabName = "summary", icon = icon("table")),
      menuItem(text = "Plots", tabName = "plots", icon = icon("signal")),
      menuItem(text = "Maps", tabName = "maps", icon = icon("map"))
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  ## Body ----
  dashboardBody(
    ## Set colors of font awesome icons
    tags$style(".fa-dashboard {color:#064193}"),
    tags$style(".fa-tint {color:#064193}"),
    tags$style(".fa-magnet {color:#064193}"),
    
    ## Tab container ---
    tabItems(
      tabItem( # start tabItem 1 for summary tables
        tabName = "summary",
        h2("Summary Tables"),
        fluidRow( # start fluidRow for valueBoxes
          # valueBox 1,2,3 here
        ), # end fluidRow for valueBoxes
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
      ), # end tabItem 1 for summary tables
      tabItem( # start tabItem 2 for plots
        tabName = "plots",
        h2("Cumulative Enrollments"),
        fluidRow(
          tabBox(
            width = 12,
            title = "Cumulative Enrollments",
            id = "tabset1",
            height = "550px",
            tabPanel(
              title = "Total",
              box(width = 12,
                  plotOutput(outputId = "plot_cum_total"))
            ), # end tabPanel 1 -- Total plot
            tabPanel(
              title = "Sex",
              box(width = 12,
                  plotOutput(outputId = "plot_cum_sex"))
            ), # end tabPanel 2 -- Sex plot
            tabPanel(
              title = "Race",
              box(width = 12,
                  plotOutput(outputId = "plot_cum_race"))
            ) # end tabPanel 3 -- Race plot
          ) # end tabBox 1 for cumulative plots
        ),
        fluidRow(
          box( # start box 1 for date range input (cumulative)
            width = 12,
            dateRangeInput(inputId = "dateRange1",
                           label = "Date range input: yyyy-mm-dd",
                           start = as.Date("2017-03-01"), end = Sys.Date())
          )
        ),
        fluidRow(
          tabBox(
            width = 12,
            title = "Targer Enrollment by Diagnosis",
            id = "tabset2",
            height = "550px",
            tabPanel("NL",
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_nl"))),
            tabPanel("MCI",
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_mci"))),
            tabPanel("AD",
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_ad"))),
            tabPanel("LBD",
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_lbd"))),
            tabPanel("FTD",
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_ftd")))
          ) # end tabBox 2 for target diagnosis plots
        ), # end fluidRow for plots
        fluidRow(
          box( # start box 2 for date range input (diagnosis)
            width = 12,
            dateRangeInput(inputId = "dateRange2",
                           label = "Date range input: yyyy-mm-dd",
                           start = as.Date("2017-03-01"),
                           end = as.Date("2022-03-01")
            )
          )
        ) # end fluidRow for interactive dates
      ), # end tabItem 2 for plots
      tabItem( # start tabItem 3 for maps
        tabName = "maps",
        h2("Maps"),
        fluidRow(
          tabBox(
            width = 12,
            title = "",
            id = "map_tabset",
            height = "700px",
            side = "left",
            tabPanel(
              title = "County",
              box(
                width = 12,
                height = "625px",
                plotOutput(outputId = "map_partic_by_county")
              )
            ), # end tabPanel 1 -- county map
            tabPanel(
              title = "ZIP",
              box(
                width = 12,
                height = "625px",
                plotOutput(outputId = "map_partic_by_zip")
              )
            ) # end tabPanel 2 -- ZIP map
          ) # end tabBox
        ) # end fluidRow
      ) # end tabItem 3 for maps
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
  
  # ## Reactive df for core data
  # data_mindset_rctv <- reactive({
  #   invalidateLater(invalidation_time, session)
  #   get_data_mindset() # source: etl_mindset_uds2.R
  # })
  # 
  # ## List for summary tables
  # lst_summ_tbls <- reactive({
  #   invalidateLater(invalidation_time, session)
  #   build_lst_summ_tbls( data_mindset_rctv() )
  # })
  # 
  # ## df for plots
  # data_plots <- reactive({
  #   invalidateLater(invalidation_time, session)
  #   build_data_plots( data_mindset_rctv() )
  # })
  # 
  # ## List for maps
  # lst_map_dfs <- reactive({
  #   invalidateLater(invalidation_time, session)
  #   build_lst_map_dfs( data_mindset_rctv() )
  # })
  
  ## Read in df for core data
  # df_mindset_xfrm <- 
  #   reactiveFileReader(intervalMillis = invalidation_time,
  #                      filePath = "df_mindset_xfrm.Rds",
  #                      readFunc = readRDS)
  
  ## List for summary tables
  lst_summ_tbls <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/lst_summ_tbls.Rds",
                       readFunc = readRDS)
  
  ## df for plots
  data_plots <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/data_plots.Rds",
                       readFunc = readRDS)
  
  ## List for maps
  lst_map_dfs <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/lst_map_dfs.Rds",
                       readFunc = readRDS)
  
  # ## List for summary tables
  # lst_summ_tbls <- build_lst_summ_tbls(df_mindset_xfrm)
  # 
  # ## df for plots
  # data_plots <- build_data_plots(df_mindset_xfrm)
  # 
  # ## List for maps
  # lst_map_dfs <- build_lst_map_dfs(df_mindset_xfrm)
  
  # # # # # 
  ## Render tables ----
  
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
  ## Render plots ----
  
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
  ## Render maps ----
  
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
  
  # ## Participation by ZIP map
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
  
}

shinyApp(ui, server)




