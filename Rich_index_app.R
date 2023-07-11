ui <- fluidPage(
  
  
  # Does not need a title
  
  
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
        column(2, 
               uiOutput("richvar_panel")), ## a second panel conditioned to the choices of first panel.
        
        column(10,
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
      checkboxGroupInput("dune_var", label = "Plot Diversity based on Environmental characteristics:", ##choose to display the summary of the index by the environmental info
                         choices = c("A1", "Moisture", "Management", "Use", "Manure"), 
                         selected = "Moisture")
      
    }
    else if (input$dataset == "mite_vg"){
      checkboxGroupInput("mite_var", "Plot Diversity based on Environmental characteristics:", 
                         c("SubsDens", "WatrCont", "Substrate", "Shrub", "Topo"),
                         selected = "Substrate")
      
    }
    else {
      checkboxGroupInput("bci_var", label = "Plot Diversity based on Environmental characteristics:", 
                         choices = c("UTM.EW", "UTM.NS", "Age.cat", "Habitat", "Stream", "EnvHet"),
                         selected = "Age.cat")
    }
  }) 
  
  #### PLOTS#############
  
  #### PLOTS DUNE
  a1_dn_rich_plt <- reactive ({
    if("A1" %in% input$dune_var){
      wrap_plots(list(dn_a1_plotS, dn_a1_plotMg,  dn_a1_plotMn), nrow = 1) }
  })
  
  moist_dn_rich_plt <- reactive ({
    if("Moisture" %in% input$dune_var){
      wrap_plots(list(dn_moist_plotS, dn_moist_plotMg, dn_moist_plotMn), nrow = 1) }
  })
  
  mang_dn_rich_plt <- reactive({
    if("Management" %in% input$dune_var){
      wrap_plots(list(dn_mang_plotS, dn_mang_plotMg, dn_mang_plotMn), nrow = 1) }
  })
  
  use_dn_rich_plt <- reactive ({
    if("Use" %in% input$dune_var){
      wrap_plots(list(dn_use_plotS, dn_use_plotMg, dn_use_plotMn), nrow = 1) }
  })
  
  manu_dn_rich_plt <- reactive ({
    if("Manure" %in% input$dune_var){
      wrap_plots(list(dn_manu_plotS, dn_manu_plotMg, dn_manu_plotMn), nrow = 1) }
  })
  
  
  
  ######## list of the bci variables and their indices
  plot_list_dn <- reactive({
    list(a1_dn_rich_plt(),moist_dn_rich_plt(), mang_dn_rich_plt(),use_dn_rich_plt(),manu_dn_rich_plt())%>% 
      discard(is.null)
  })
  
  dn_richplots_print<- reactive({
    wrap_plots(plot_list_dn(), ncol = 1)
  })
  
  #### PLOTS MITE
  
  dens_mt_rich_plt <- reactive({
    if("SubsDens" %in% input$mite_var){
      wrap_plots(list(mt_den_plotS, mt_den_plotMg, mt_den_plotMn), nrow = 1) }
  })
  
  wtr_mt_rich_plt <- reactive({
    if ("WatrCont" %in% input$mite_var){
      wrap_plots(list(mt_wt_plotS, mt_wt_plotMg, mt_wt_plotMn), nrow = 1)}
  })
  
  sub_mt_rich_plt <- reactive({
    if("Substrate" %in% input$mite_var){
      wrap_plots(list(mt_subs_plotS, mt_subs_plotMg ,mt_subs_plotMn), nrow = 1) }
  })
  
  shr_mt_rich_plt <- reactive ({
    if("Shrub" %in% input$mite_var){
      wrap_plots(list(mt_shru_plotS, mt_shru_plotMg, mt_shru_plotMn), nrow = 1) }
  })
  
  topo_mt_rich_plt <- reactive ({
    if("Topo" %in% input$mite_var){
      wrap_plots(list(mt_topo_plotS, mt_topo_plotMg , mt_topo_plotMn), nrow = 1) }
  })
  
  ######## list of the bci variables and their indices
  plot_list_mt <- reactive({
    list(dens_mt_rich_plt(), wtr_mt_rich_plt(), sub_mt_rich_plt(),shr_mt_rich_plt(),topo_mt_rich_plt())%>% 
      discard(is.null)
  })
  
  mt_richplots_print <- reactive({
    wrap_plots(plot_list_mt(), ncol = 1)
  })
  
  #### PLOTS BCI 
  ew_bci_rich_plt <- reactive({
    if("UTM.EW" %in% input$bci_var){
      wrap_plots(list(bc_ew_plotS, bc_ew_plotMg,bc_ew_plotMn), nrow = 1) }
  })
  
  ns_bci_rich_plt <- reactive({
    if("UTM.NS" %in% input$bci_var){
      wrap_plots(list(bc_ns_plotS, bc_ns_plotMg,bc_ns_plotMg), nrow = 1) }
  })
  
  age_bci_rich_plt <- reactive({
    if("Age.cat" %in% input$bci_var){
      wrap_plots(list(bc_age_plotS, bc_age_plotMg,bc_age_plotMn), nrow = 1) }
  })
  
  habt_bci_rich_plt <- reactive ({
    if("Habitat" %in% input$bci_var){
      wrap_plots(list(bc_habt_plotS, bc_habt_plotMg,bc_habt_plotMn), nrow = 1) }
  })
  
  str_bci_rich_plt <- reactive ({
    if("Stream" %in% input$bci_var){
      wrap_plots(list(bc_strm_plotS, bc_strm_plotMg,bc_strm_plotMn), nrow = 1) }
  })
  
  het_bci_rich_plt <- reactive({
    if("EnvHet" %in% input$bci_var){
      wrap_plots(list(bc_het_plotS, bc_het_plotMg, bc_het_plotMn), nrow = 1) }
  })
  ######## list of the bci variables and their indices
  plot_list_bc <- reactive({
    list(ew_bci_rich_plt(), ns_bci_rich_plt(), age_bci_rich_plt(),habt_bci_rich_plt(),str_bci_rich_plt(), het_bci_rich_plt())%>% 
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