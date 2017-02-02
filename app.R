source("input.R")
source("model.R")
source("plot.R")

library(shiny)

ui <- fluidPage(

  titlePanel("RescueTime Estimation"),

  fixedRow(
    column(
      width = 4,
      wellPanel(
        h3("What's this?"),
        helpText("This is an application that ingests week-level RescueTime productivity data then seeks to infer the expected value of a typical
                 week. The result is a probability distribution across the proportion of your week you typically spend on", tags$b("Very Distracting,"), tags$b("Distracting,"), tags$b("Neutral,"),
                 tags$b("Productive"), "and", tags$b("Very Productive"), "activities.")
      ),
      wellPanel(
        h3("Download report"),
        helpText(
          "Please download your week-level report at the following",
          tags$a(href="https://www.rescuetime.com/browse/productivity/by/week/for/the/year/of/2016-01-01", "link."),
          "Should you not have one, you're free to use",
          tags$a(href="https://github.com/cavaunpeu/rescue-time-estimation/blob/publish/data/rescue_time_report.csv", "mine.")
        ),
        tags$hr(),
        h3("Upload report"),
        fileInput(
          inputId = "rescue.time.report",
          label = NULL,
          multiple = FALSE,
          accept = c("text/csv, text/comma-separated-values", ".csv")
        )
      ),
      wellPanel(
        h3("Documentation"),
        helpText(
          "Voilà the",
          tags$a(href="https://github.com/cavaunpeu/rescue-time-estimation", "code"),
          "and",
          tags$a(href="http://wp.me/p4zXJT-fm", "blog post"),
          "accompanying this project."
        )
      )
    ),
    column(
      width = 8,
      plotOutput("plotPanel", height = "1000px", width = "auto")
    )
  )
)

server <- function(input, output) {

  output$plotPanel <- renderPlot({
    rescue.time.report <- input$rescue.time.report

    if ( is.null(rescue.time.report) )
      return(NULL)

    report <- readReport(rescue.time.report$datapath)
    model <- buildModel(report, iter = 4000)
    predictions <- simulatePredictions(model)
    generatePlot(predictions)
  })

}

shinyApp(ui, server)
