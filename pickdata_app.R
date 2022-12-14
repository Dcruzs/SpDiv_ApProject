
ui <- fluidPage(
  
  
  titlePanel("Downloading Dataset of Species Diversity"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  c("dune", "mite.data", "vegan.BCI")),
      textOutput("description"), ### add description of the dataset selected into the side panel instead of the mainPanel
      
      br(), ##add vertical space to the next widget 
      
      checkboxGroupInput("indx_outcome", label = "Calculate Indices?", ##choose to display the summary of the index
                         choices = c("Yes" = "Yes"), selected = "Yes")
    ), 
    
    mainPanel(
      dataTableOutput("table"),
      dataTableOutput("Index") #to display the summary of the index
      
      
    )
  )
)

server <- function(input, output) {
  
  desc <- reactive({
    switch(input$dataset, # need to edit the texts
           "dune" = "Vegetation data of the Dutch Dune Meadows, Terschelling, Netherlands. The study sampled 20 sites of 2x 2 square meters each and found 30 species of plants. For more details, check out R - Vegan package (Oksanen et al. 2022).",
           "mite.data" = "The data includes morpho-species of oribatid mites (also known as moss or beetle mites) found in the vicinity of Lac Geai (in Québec, Canada). Mites were collected from Sphagnum moss samples (5 cm in diameter and 7 cm deep). The table includes 70 samples (rows) and 35 mite species (columns). For more details, check out R - Vegan package (Oksanen et al. 2022).",
           "vegan.BCI" = "On the Panamanian island of Barro Colorado (BCI), data from floristic surveys revealed the number of trees in a plot of 1-hectare . The table displays the 225 species found (columns) in 50 plots/samples (rows). For more details, check out R - Vegan package (Oksanen et al. 2022)."
    ) 
  })
  output$description = renderText(desc())
  
  
  myCSV <- reactive({
    switch(input$dataset,
           "dune" = dune,
           "mite.data" = mite.data,
           "vegan.BCI" = vegan.BCI)
  })
  
  ## How to add here the functions to connect the chosen dataset to its specific Index output Table? 
  myindex <- reactive({if ("Yes" %in% input$indx_outcome) {
    switch(input$dataset,
           "dune" = dn_richInd,
           "mite.data" = mt_richInd,
           "vegan.BCI" = bc_richInd)}
    
    
  })
  
  output$table = renderDT(myCSV(), options = list(
    pageLength = 10)
  )
  
  output$Index = renderDT(myindex(), options = list(
    pageLength = 10)
  )
  
}


shinyApp(ui = ui, server = server)
