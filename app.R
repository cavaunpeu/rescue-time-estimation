source("input.R")
source("model.R")
source("plot.R")

library(shiny)

ui <- fluidPage(

  titlePanel("RescueTime Estimation"),

  sidebarLayout(
    fixedRow(
      column(
        width = 4,
        h3("Download report"),
        style = "padding-left:31px",
        helpText(
          "Please download your week-level report at the following",
          tags$a(href="https://www.rescuetime.com/browse/productivity/by/week/for/the/year/of/2016-01-01", "link.")
        )
      ),
      column(
        width = 4,
        h3("Upload report"),
        fileInput(
          inputId = "rescue.time.report",
          label = NULL,
          multiple = FALSE,
          accept = c("text/csv, text/comma-separated-values", ".csv")
        )
      ),
      column(
        width = 4,
        h3("Code"),
        style = "padding-right:31px",
        helpText("Voilà the", tags$a(href="http://wp.me/p4zXJT-fm", "code"), "and post for this project")
      )
    ),

    mainPanel(
      plotOutput("plotPanel", width="100%")
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
