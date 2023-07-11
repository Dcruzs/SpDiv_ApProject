ui <- fluidPage(
  
  sidebarLayout(
    
    
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  c("dune_vg", "mite_vg", "BCI_vg")),
      textOutput("description"), ### add description of the dataset selected into the side panel instead of the mainPanel
      
      br(), ##add vertical space to the next widget 
      
      checkboxGroupInput("indx_outcome", label = "Calculates Indices?", ##choose to display the summary of the index
                         choices = c("Yes" = "Yes"), selected = "No")
    ), 
    
    mainPanel(
      dataTableOutput("table"),
      
      hr(),
      
      dataTableOutput("spdiv_Index"),
      hr(),
      
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
                         selected = "Moisture") 
      
    }
    else if (input$dataset == "mite_vg"){
      checkboxGroupInput("mite_var", "Plot Diversity based on Environmental characteristics:", 
                         c("SubsDens","WatrCont", "Substrate", "Shrub", "Topo"),
                         selected = "Substrate")
      
    }
    else {
      checkboxGroupInput("bci_var", label = "Plot Diversity based on Environmental characteristics:", 
                         choices = c("UTM.EW", "UTM.NS", "Age.cat", "Habitat", "Stream", "EnvHet"),
                         selected = "Age.cat")
    }
  })
  
  ## Make plots DUNE variables\
  ### variable A1.dune: 
  a1_dune_plt <- reactive ({
    if("A1" %in% input$dune_var){
      wrap_plots(list(dn_a1_plotH, dn_a1_plotD, dn_a1_plotJ, dn_a1_plotDi), nrow=1) }
  }) 
  ### variable Moisture.dune: 
  moist_dune_plt <- reactive ({
    if("Moisture" %in% input$dune_var){
      wrap_plots(list(dn_moist_plotH,dn_moist_plotD, dn_moist_plotJ, dn_moist_plotDi), nrow=1) }
  }) 
  ### PLOT variable Management.dune
  mang_dune_plt <- reactive ({
    if("Management" %in% input$dune_var){
      wrap_plots(list( dn_mang_plotH, dn_mang_plotD ,dn_mang_plotJ, dn_mang_plotDi), nrow=1) }
  })
  ### variable Use.dune
  use_dune_plt <- reactive ({
    if("Use" %in% input$dune_var){
      wrap_plots(list( dn_use_plotH, dn_use_plotD, dn_use_plotJ, dn_use_plotDi), nrow=1) }
  })
  ## variable Manure.dune: 
  manu_dune_plt <- reactive ({
    if("Manure" %in% input$dune_var){
      wrap_plots(list( dn_manu_plotH, dn_manu_plotD, dn_manu_plotJ, dn_manu_plotDi), nrow=1) }
  })   
  
  # Make a list of plots and print out plots based on which ones were requested 
  dn_plot_list <- reactive({
    list(a1_dune_plt(), moist_dune_plt(),mang_dune_plt (), use_dune_plt(),manu_dune_plt()) %>% 
      discard(is.null)
  })
  #plot_grid() is used to combine multiple plots into one.
  dn_plots_print <- reactive({
    wrap_plots(dn_plot_list(), ncol = 1) 
  })
  
  
  ##########################
  ## Make plots MITE variables
  den_mite_plt <- reactive ({
    if("SubsDens" %in% input$mite_var){
      wrap_plots(list(mt_den_plotH, mt_den_plotD, mt_den_plotJ, mt_den_plotDi), nrow=1) }
  })
  wt_mite_plt <- reactive ({
    if("WatrCont" %in% input$mite_var){
      wrap_plots(list(mt_wt_plotH, mt_wt_plotD, mt_wt_plotJ, mt_wt_plotDi), nrow=1) }
  })
  sub_mite_plt <- reactive ({
    if("Substrate" %in% input$mite_var){
      wrap_plots(list( mt_subs_plotH, mt_subs_plotD, mt_subs_plotJ,mt_subs_plotDi), nrow=1) }
  })
  shr_mite_plt <- reactive ({
    if("Shrub" %in% input$mite_var){
      wrap_plots(list( mt_shru_plotH, mt_shru_plotD, mt_shru_plotJ,mt_shru_plotDi), nrow=1) }
  })   
  topo_mite_plt <- reactive ({
    if("Topo" %in% input$mite_var){
      wrap_plots(list(mt_topo_plotH, mt_topo_plotD, mt_topo_plotJ, mt_topo_plotDi), nrow=1) }
  })  
  
  # Make a list of plots and print out plots based on which ones were requested 
  mt_plot_list <- reactive({
    list(den_mite_plt(), wt_mite_plt(), sub_mite_plt(),shr_mite_plt(), topo_mite_plt()) %>% 
      discard(is.null)
  })
  #plot_grid() is used to combine multiple plots into one.
  mt_plots_print <- reactive({
    wrap_plots(mt_plot_list(), ncol = 1)  
  })
  
  
  ########################## 
  ## Make plots BCI variables
  ew_bci_plt <- reactive ({
    if("UTM.EW" %in% input$bci_var){
      wrap_plots(list(bc_ew_plotH, bc_ew_plotD, bc_ew_plotJ, bc_ew_plotDi), nrow=1) }
  }) 
  ns_bci_plt <- reactive ({
    if("UTM.NS" %in% input$bci_var){
      wrap_plots(list(bc_ns_plotH, bc_ns_plotD, bc_ns_plotJ, bc_ns_plotDi), nrow=1) }
  }) 
  age_bci_plt <- reactive ({
    if("Age.cat" %in% input$bci_var){
      wrap_plots(list(bc_age_plotH, bc_age_plotD, bc_age_plotJ, bc_age_plotDi), nrow=1) }
  }) 
  habt_bci_plt <- reactive ({
    if("Habitat" %in% input$bci_var){
      wrap_plots(list(bc_habt_plotH, bc_habt_plotD, bc_habt_plotJ, bc_habt_plotDi), nrow=1) }
  })
  str_bci_plt <- reactive ({
    if("Stream" %in% input$bci_var){
      wrap_plots(list(bc_strm_plotH, bc_strm_plotD,bc_strm_plotJ, bc_strm_plotDi), nrow=1) }
  })   
  het_bci_plt <- reactive ({
    if("EnvHet" %in% input$bci_var){
      wrap_plots(list(bc_het_plotH, bc_het_plotD, bc_het_plotJ, bc_het_plotDi), nrow=1) }
  })
  
  # Make a list of plots and print out plots based on which ones were requested 
  bci_plot_list <- reactive({
    list(ew_bci_plt(), ns_bci_plt(),age_bci_plt(),habt_bci_plt(), str_bci_plt(), het_bci_plt()) %>% 
      discard(is.null)
  })
  #plot_grid() is used to combine multiple plots into one.
  bci_plots_print <- reactive({
    wrap_plots(bci_plot_list(), ncol = 1)
  })
  
  
  plot_variables <- reactive({
    if("dune_vg" %in% input$dataset){
      dn_plots_print()}
    else if("mite_vg" %in% input$dataset){
      mt_plots_print()}
    else if("BCI_vg" %in% input$dataset){
      bci_plots_print()}
  })
  output$plots = renderPlot({
    plot_variables()
  })
}

shinyApp(ui = ui, server = server)