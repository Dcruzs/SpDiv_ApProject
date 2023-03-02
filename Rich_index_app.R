ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  c("dune_vg", "mite_vg", "BCI_vg")),
      textOutput("description"), ### add description of the dataset selected into the side panel instead of the mainPanel
      
      br(), ##add vertical space to the next widget 
      
      checkboxGroupInput("indx_outcome", label = "Calculate Indices by sample?", ##choose to display the summary of the index
                         choices = c("Yes" = "Yes"), selected = "NUll") # NULL : ensure that none of the choices are pre-selected by default
      
    ), 
    
    mainPanel(
      dataTableOutput("table"),
      hr(),
      dataTableOutput("Index_samp"),
      hr(),
      
      fluidRow(
        column(3, 
               uiOutput("richvar_panel")), ## a second panel conditioned to the choices of first panel.
        
        column(8,
               plotOutput("plots_richvar", width = 1000, height = 1000))
      )
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
  myindex <- reactive({if ("Yes" %in% input$indx_outcome) {
    switch(input$dataset,
           "dune_vg" = dn_richInd,
           "mite_vg" = mt_richInd,
           "BCI_vg" = bc_richInd)}
    
    
  })
  
  
  output$table = renderDT(myCSV(), options = list(
    pageLength = 10)
  )
  
  output$Index_samp = renderDT(myindex(), options = list(
    pageLength = 10)
  )
  ### output of the second Panel
  
  output$richvar_panel <- renderUI({
    if (input$dataset == "dune_vg") {
      checkboxGroupInput("dune_var", label = "Plot Indices by Environmental characteristics:", ##choose to display the summary of the index by the environmental info
                         choices = c("Moisture", "Management", "Use", "Manure"), 
                         selected = "Moisture")
      
    }
    else if (input$dataset == "mite_vg"){
      checkboxGroupInput("mite_var", "Plot Diversity based on Environmental characteristics:", 
                         c("Substrate", "Shrub", "Topo"),
                         selected = "Substrate")
      
    }
    else {
      checkboxGroupInput("bci_var", label = "Plot Diversity based on Environmental characteristics:", 
                         choices = c("Age.cat", "Habitat", "Stream"),
                         selected = "Age.cat")
    }
  }) 
  
  #### PLOTS#############
  
  #### PLOTS DUNE
  moist_dn_rich_plt <- reactive ({
    if("Moisture" %in% input$dune_var){
      wrap_plots(list(dn_moist_plotMg ,dn_moist_plotMn , dn_moist_plotOd), nrow = 1) }
  })
  
  mang_dn_rich_plt <- reactive({
    if("Management" %in% input$dune_var){
      wrap_plots(list(dn_mang_plotMg,dn_mang_plotMn,dn_mang_plotOd), nrow = 1) }
  })
  
  use_dn_rich_plt <- reactive ({
    if("Use" %in% input$dune_var){
      wrap_plots(list(dn_use_plotMg,dn_use_plotMn,dn_use_plotOd), nrow = 1) }
  })
  
  manu_dn_rich_plt <- reactive ({
    if("Manure" %in% input$dune_var){
      wrap_plots(list(dn_manu_plotMg ,dn_manu_plotMn , dn_manu_plotOd), nrow = 1) }
  })
  
  
  ######## list of the bci variables and their indices
  plot_list_dn <- reactive({
    list(moist_dn_rich_plt(), mang_dn_rich_plt(),use_dn_rich_plt(),manu_dn_rich_plt())%>% 
      discard(is.null)
  })
  
  dn_richplots_print<- reactive({
    wrap_plots(plot_list_dn(), ncol = 1)
  })
  
  #### PLOTS MITE
  
  sub_mt_rich_plt <- reactive({
    if("Substrate" %in% input$mite_var){
      wrap_plots(list(mt_subs_plotMg ,mt_subs_plotMn, mt_subs_plotOd), nrow = 1) }
  })
  
  shr_mt_rich_plt <- reactive ({
    if("Shrub" %in% input$mite_var){
      wrap_plots(list(mt_shru_plotMg, mt_shru_plotMn,mt_shru_plotOd ), nrow = 1) }
  })
  
  topo_mt_rich_plt <- reactive ({
    if("Topo" %in% input$mite_var){
      wrap_plots(list(mt_topo_plotMg , mt_topo_plotMn, mt_topo_plotOd), nrow = 1) }
  })
  
  ######## list of the bci variables and their indices
  plot_list_mt <- reactive({
    list(sub_mt_rich_plt(),shr_mt_rich_plt(),topo_mt_rich_plt())%>% 
      discard(is.null)
  })
  
  mt_richplots_print <- reactive({
    wrap_plots(plot_list_mt(), ncol = 1)
  })
  
  #### PLOTS BCI 
  
  age_bci_rich_plt <- reactive({
    if("Age.cat" %in% input$bci_var){
      wrap_plots(list(bc_age_plotMg,bc_age_plotMn, bc_age_plotOd), nrow = 1) }
  })
  
  habt_bci_rich_plt <- reactive ({
    if("Habitat" %in% input$bci_var){
      wrap_plots(list(bc_habt_plotMg,bc_habt_plotMn, bc_habt_plotOd), nrow = 1) }
  })
  
  str_bci_rich_plt <- reactive ({
    if("Stream" %in% input$bci_var){
      wrap_plots(list(bc_strm_plotMg,bc_strm_plotMn, bc_strm_plotOd), nrow = 1) }
  })
  
  ######## list of the bci variables and their indices
  plot_list_bc <- reactive({
    list(age_bci_rich_plt(),habt_bci_rich_plt(),str_bci_rich_plt())%>% 
      discard(is.null)
  })
  
  bci_richplots_print <- reactive({
    wrap_plots(plot_list_bc(), ncol = 1)
  })
  
  
  
  ##### Putput plots based on the dataset chosen
  plot_rich_variables <- reactive({
    if("dune_vg" %in% input$dataset){
      dn_richplots_print()}
    else if("mite_vg" %in% input$dataset){
      mt_richplots_print()}
    else if("BCI_vg" %in% input$dataset){
      bci_richplots_print()}
  })
  output$plots_richvar = renderPlot({
    plot_rich_variables()
  })
  
  
}


shinyApp(ui = ui, server = server)