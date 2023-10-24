library(shiny)
library(R.matlab)
library(ggplot2)

# data: https://www.raman.ugent.be/node/61/download/35893e04a18bbfa0e08700ed46e9ac54
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
                     choices = names(data), selected = c(names(data)[0]), 
                     inline = TRUE
  ),
  plotOutput("plot")
  
)

server <- function(input, output) {
  output$plot <- renderPlot({
    # Extract X-axis data
    x <- data$Xaxis
    
    # Plot the selected samples
    if (length(input$samples) > 0) {
      p <- ggplot() +
        geom_line(aes(x = x, y = data[[input$samples[1]]]), color = "black") +
        ggtitle(input$samples[1]) +
        ylim(range(unlist(data[input$samples]))) +
        ylab("I") +
        xlab("Wellenzahl [cm-1]")
      
      for (i in 2:length(input$samples)) {
        p <- p +
          geom_line(aes(x = x, y = data[[input$samples[i]]]), color = rainbow(length(input$samples))[i])
      }
      
      # Add a legend
      p <- p + theme(legend.position = "topright") +
        labs(color = "Samples") +
        scale_color_manual(values = rainbow(length(input$samples)), labels = input$samples)
      
      print(p)
      
    } else {
      # Display a message if no samples are selected
      message("No samples selected.")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
