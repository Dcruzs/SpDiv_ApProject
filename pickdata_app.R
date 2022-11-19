library(shiny)
library(DT)

#### This code applies DT () to customize tables of the dataset chosen #########

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Downloading Dataset of Species Diversity"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Pick a dataset:", 
                                    c("dune", "dune_moistureLevel", "flutter", "vegan.BCI"))
      
      
    ), 
    # Button to download teh data (optional)
    # downloadButton("downloadData", "Download")
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      DTOutput("table")
      
    )
  )
)

server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  myCSV <- reactive({
    switch(input$dataset,
           "dune" = dune,
           "dune_moistureLevel" = dune_moistureLevel,
           "flutter" = flutter,
           "vegan.BCI" = vegan.BCI)
  })
  
  
  # Table of selected dataset ----
      #https://rstudio.github.io/DT/
  output$table = renderDT(myCSV(), options = list(
    pageLength = 10)
  )
  
  # Downloadable csv of selected dataset ----
  #output$downloadData <- downloadHandler(
  # filename = function() {
  # paste(input$dataset, ".csv", sep = "")
  #},
  #content = function(file) {
  # write.csv(datasetInput(), file, row.names = FALSE)
  #}
  #)
  
}


# Run the application 
shinyApp(ui = ui, server = server)
