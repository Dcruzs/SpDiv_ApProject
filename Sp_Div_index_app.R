ui <- fluidPage(
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  c("dune_vg", "mite_vg", "BCI_vg")),
      textOutput("description"), ### add description of the dataset selected into the side panel instead of the mainPanel
      
      br(), ##add vertical space to the next widget 
      
      checkboxGroupInput("indx_outcome", label = "Calculates Indices?", ##choose to display the summary of the index
                         choices = c("Yes" = "Yes"), selected = "No"),
      
      # br(), ##add vertical space to the next widget 
      #  checkboxGroupInput("indx_dune", label = "Plot Diversity based on Environmental characteristics:", ##choose to display the summary of the index by the environmental info
      # choices = c("A1", "Moisture", "Management", "Use", "Manure"))
      ## iS OT BETTER TO USE THE select BUTTOM INSTEA CHECK BOX???
      #### MAYBE ADD DESCRIPTION TO EACH OF THE VARIABLES??? 
      
      
    ), 
    
    
    mainPanel(
      dataTableOutput("table"),
      
      hr(),
      
      
      dataTableOutput("spdiv_Index"),
      hr(),
      
      uiOutput("var_panel"), ## a second panel conditioned to the choices of first panel.
      
      #plotOutput("dune_var")
      
      
    ) 
  )
)
##############################

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
  
  ## How to add here the functions to connect the chosen dataset to its specific Index output Table? 
  my_spdivindx <- reactive({if ("Yes" %in% input$indx_outcome) {
    switch(input$dataset,
           "dune_vg" = dn_spdiv_index,
           "mite_vg" = mt_spdiv_index,
           "BCI_vg" = bc_spdiv_index)}
    
    
  })
  
  
  output$table = renderDT(myCSV(), options = list(
    pageLength = 10)
  )
  
  output$spdiv_Index = renderDT(my_spdivindx(), options = list(
    pageLength = 10)
  )
  
  ### output of the second Panel
  
  output$var_panel <- renderUI({
    if (input$dataset == "dune_vg") {
      checkboxGroupInput("dune_var", label = "Plot Diversity based on Environmental characteristics:", ##choose to display the summary of the index by the environmental info
                         choices = c("A1", "Moisture", "Management", "Use", "Manure"),
                         plotOutput("plot_dune_var"))
    }
    else if (input$dataset == "mite_vg"){
      selectInput("mite_var", "Choose a variable:", 
                  c("SubsDens", "WatrCont", "Substrate", "Shrub", "Topo"),
                  textOutput("description")) #  maybe add description of each variable
      
    }
    else {
      checkboxGroupInput("bci_var", label = "Plot Diversity based on Environmental characteristics:", 
                         choices = c("Age.cat", "Habitat", "Stream", "EnvHet"))
    }
    
    
  })
  
  ## to plot dune variables
  duneplot <- reactive({if ("Yes" %in% input$dune_var) {
    switch(input$dune_var,
           "Moisture" = dn_moist_plot ,
           "Management" = dn_mang_plot,
           "Use" = dn_use_plot,
           "Manure" = dn_manu_plot)}
    
    
  })
  ### output of the plots
  output$plot_dune_var = renderPlot(duneplot)
  
}


shinyApp(ui = ui, server = server)