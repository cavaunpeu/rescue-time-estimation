source("input.R")

library(shiny)

ui <- fluidPage(
  
  # Application title
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
      # plotOutput("dirichletPlot")
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  output$contents <- renderTable({
    report <- input$rescue.time.report
    
    if ( is.null(report) )
      return(NULL)
    
    readReport(report$datapath)
  })
}

shinyApp(ui, server)