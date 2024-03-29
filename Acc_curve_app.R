ui <- fluidPage(
  
  sidebarLayout(
    
    
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  c("dune_vg", "mite_vg", "BCI_vg")),
      textOutput("description"),### add description of the dataset selected into the side panel instead of the mainPanel
      
      br(), # add space
      
      checkboxGroupInput("acc_plt", label = "Accumulation Curve:",
                         choices = c("Yes" = "Yes"), selected = "No"),
      
    ), 
    
    mainPanel(
      dataTableOutput("table"),
      
      hr(),
      
      plotOutput("plotacc", width = 1000),
      
      
      fluidRow(
        column(2,
               uiOutput("var_panel")), ## a second panel conditioned to the choices of first panel.
        
        column(10,
               plotOutput("plots", width = 1000, height = 1000))
      )
      
      
    ) 
  )
)
###########################
server <- function(input, output) {
  
  desc <- reactive({
    switch(input$dataset, # need to edit the texts
           "dune_vg" = "Vegetation data of the Dutch Dune Meadows, Terschelling, Netherlands. The study sampled 20 sites of 2x 2 square meters each and found 30 species of plants. For more details, check out R - Vegan package (Oksanen et al. 2022).",
           "mite_vg" = "The data includes morpho-species of oribatid mites (also known as moss or beetle mites) found in the vicinity of Lac Geai (in Québec, Canada). Mites were collected from Sphagnum moss samples (5 cm in diameter and 7 cm deep). The table includes 70 samples (rows) and 35 mite species (columns). For more details, check out R - Vegan package (Oksanen et al. 2022).",
           "BCI_vg" = "Floristic surveys on the Panamanian island of Barro Colorado (BCI) revealed the number of trees in a 1-hectare plot. The table shows the 225 species (columns) in 50 plots/samples (rows). Although the dataset contains nine environmental variables, the Index Table output only displays the columns whose observations varied between samples. See the R - Vegan package for more information (Oksanen et al. 2022)."
    ) 
  })
  
  output$description = renderText(desc())
  
  
  myCSV <- reactive({
    switch(input$dataset,
           "dune_vg" = dune_vg,
           "mite_vg" = mite_vg,
           "BCI_vg" = BCI_vg)
  })
  
  output$table = renderDT(myCSV(), options = list(
    pageLength = 10))
  
  # Selected to see accumulation curve
  my_acc_curve <- reactive({if ("Yes" %in% input$acc_plt) {
    switch (input$dataset, 
            "dune_vg" = plot_acc_dn,           
            "mite_vg" = plot_acc_mt,
            "BCI_vg" = plot_acc_bc)}
  })
  output$plotacc = renderPlot({
    if (!is.null(my_acc_curve())){
      my_acc_curve()}
  })
  
}
shinyApp(ui = ui, server = server)