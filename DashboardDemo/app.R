# library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  # Header ----
  dashboardHeader(title = "Basic dashboard"),
  
  # Sidebar ----
  dashboardSidebar(
    # Sidebar menu ----
    sidebarMenu(
      menuItem(text = "Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem(text = "Widgets", tabName = "widgets", icon = icon("th"))
    )
  ), # end dashboardSideBar
  
  # Body
  dashboardBody(
    
    # Tab container ----
    tabItems(
      
      # First tab content
      tabItem(tabName = "dashboard",
              # Boxes are put in rows or columns
              fluidRow(
                box(plotOutput(outputId = "plot1", height = 250)),
                box(title = "Controls", 
                    sliderInput(inputId = "slider", label = "Number of obs:", 
                                min = 1, max = 100, value = 50))
              )
      ), # end first tab
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      ) # end second tab
      
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)



