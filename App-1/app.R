library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Demo for the GUI for HIS Project",
  sidebar = sidebar(
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    )
  ),
  page_fixed(
    fileInput("file1", "Choose csv File", accept = c(".csv",".xlsx")),
    verbatimTextOutput("file1_contents")
  ),
  
  plotOutput(outputId = "distPlot")
)

server <- function(input, output) {

  output$file1_contents <- renderPrint({print(input$file1)})
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)


