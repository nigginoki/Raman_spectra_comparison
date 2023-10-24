library(shiny)
library(R.matlab)

# Load data
setwd("~/R_databases")
data <- readMat("data/referencedatabasebiomolecules.mat")

# Define the user interface (UI)
ui <- fluidPage(
  tags$head(tags$style("
    .checkbox-inline {
    
      display: inline-block;
      width: 18%;
    }
  ")),
  checkboxGroupInput("samples", "Select samples to plot:", 
                     choices = names(data), selected = c(names(data)[1]), 
                     inline = TRUE
  ),
  plotOutput("plot"),
  actionButton("reset", "Reset plots")
)

# Define the server logic
server <- function(input, output, session) {
  
  # Render the plot
  output$plot <- renderPlot({
    
    req(input$samples) # Ensure that samples are selected
    
    # Extract X-axis data
    x <- data$Xaxis
    
    tryCatch({
      # Plot the selected samples
      plot(x, data[[input$samples[1]]], type = "l", main = input$samples[1], 
           ylim = range(unlist(data[input$samples])), 
           ylab="I", xlab="Wellenzahl [cm-1]")
      
      for (i in 2:length(input$samples)) {
        lines(x, data[[input$samples[i]]], col = rainbow(length(input$samples))[i])
      }
      
      # Add a legend
      legend("topright", legend = input$samples, col = rainbow(length(input$samples)), lty = 1)
    }, error = function(e) {
      # Display an error message if there is a problem with the selected samples
      message("Error: ", e$message)
    })
    
  })
  
  # Add an observer for the reset button
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "samples", selected = c(names(data)[1]))
    output$plot <- renderPlot(NULL)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
