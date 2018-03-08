library(shiny)

ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("SOM"),
   
   sidebarLayout(
      sidebarPanel(
        fileInput("dataset", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Header", TRUE),
        actionButton("showDataset", "Show Dataset"),
        numericInput("xNeurons", "Number of Neurons on X dimension", 2),
        numericInput("yNeurons", "Number of Neurons on Y dimension", 1),
        numericInput("groups", "Number of Clusters", 2)
      ),
      
      mainPanel(
        titlePanel("Dataset:\n"),
        tableOutput("dataset"),
        titlePanel('Neurons Dimensions: \n'),
        textOutput("neuronsDimensions"),
        plotOutput("somplot")
      )
   )
))


server <- shinyServer(function(input, output) {
  observeEvent(input$showDataset, {
    inputFile <- input$dataset
    output$dataset <- if (is.null(inputFile)) {
      renderText({
        'Upload Dataset'
      })
    } else {
      renderTable({
        read.csv(inputFile$datapath, header = input$header)
      })
    }
  })
  output$neuronsDimensions <- renderText({
    paste('x = ',input$xNeurons,', y = ',input$yNeurons, ', clusters = ', input$groups)
  })
  
  observe(
    output$somplot <- renderPlot({
      inputFile <- input$dataset
      if (is.null(inputFile)) {
        return(NULL)
      }
      data <- read.csv(inputFile$datapath, header = input$header)
      data = data[, -c(5)]
      sommap <- som(scale(data), grid = somgrid(input$xNeurons, input$yNeurons, "hexagonal"))
      ## use hierarchical clustering to cluster the codebook vectors
      groups <- input$groups
      som.hc <- cutree(hclust(dist(sommap$codes[[1]])), groups)
      
      #plot
      plot(sommap, type="codes", bgcol=rainbow(groups)[som.hc])
      
      #cluster boundaries
      #add.cluster.boundaries(sommap, som.hc)
    })
  )
})

# Run the application 
shinyApp(ui = ui, server = server)

