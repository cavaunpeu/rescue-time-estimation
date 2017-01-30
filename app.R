source("input.R")
source("model.R")
source("plot.R")

library(shiny)

ui <- fluidPage(

  titlePanel("Rescue Time Estimation"),

  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "rescue.time.report",
        label = "Please upload your Rescue Time report",
        multiple = FALSE,
        accept = c("text/csv, text/comma-separated-values", ".csv")
      ),
      tags$hr()
    ),

    mainPanel(
      plotOutput("plotPanel")
    )
  )
)

server <- function(input, output) {

  output$plotPanel <- renderPlot({
    rescue.time.report <- input$rescue.time.report

    if ( is.null(rescue.time.report) )
      return(NULL)

    report <- readReport(rescue.time.report$datapath)
    model <- buildModel(report)
    predictions <- simulatePredictions(model)
    generatePlot(predictions)
  })

}

shinyApp(ui, server)
