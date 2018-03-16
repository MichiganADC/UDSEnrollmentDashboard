library(shiny)
library(shinydashboard)

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Miles Per Gallon"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput(inputId = "variable", 
                  label = "Variable:",
                  choices = c("Cylinders" = "cyl", 
                              "Transmission" = "am",
                              "Gears" = "gear")),
      
      # Input: Checkbox for whether outliers should be inlcuded ---
      checkboxInput(inputId = "outliers", 
                    label = "Show outliers", 
                    value = TRUE)
      
    ),
    
    # Main panel for displaying outputs ---
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
      
    )
  )
)

# Data pre-processing ----
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ---
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # -- only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()), 
            data = mpgData,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}

shinyApp(ui, server)






