ui <- fluidPage(
  
  sidebarLayout(
    
    
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  c("dune_vg", "mite_vg", "BCI_vg")),
      textOutput("description"),### add description of the dataset selected into the side panel instead of the mainPanel
      
      br(), # add space
      
     
      # change sample size for rarify plot 
      
      conditionalPanel(
        condition = "input.dataset == 'dune_vg'",
        sliderInput("rarfy_sampl_dune", label = "Sample Size of the Rarefied Plot", 
                    min = 1, max = 50, value = 15)
      ),
      conditionalPanel(
        condition = "input.dataset == 'mite_vg'",
        sliderInput("rarfy_sampl_mite", label = "Sample Size of the Rarefied Plot", 
                    min = 1, max = 300, value = 8)
      ),
      conditionalPanel(
        condition = "input.dataset == 'BCI_vg'",
        sliderInput("rarfy_sampl_BCI", label = "Sample Size of the Rarefied Plot", 
                    min = 1, max = 600, value = 340)
      ),
      
      br(),
      # change sample size for rarecurve plot 
      # sliderInput("rare_steps", label = "Rarefaction Steps", 
      #           min = 1, max = 500, value = 1),
      conditionalPanel(
        condition = "input.dataset == 'dune_vg'",
        sliderInput("rare_steps_dn", label = "Rarefaction Steps", 
                    min = 1, max = 500, value = 1)
      ),
      conditionalPanel(
        condition = "input.dataset == 'mite_vg'",
        sliderInput("rare_steps_mt", label = "Rarefaction Steps", 
                    min = 1, max = 500, value = 1)
      ),
      conditionalPanel(
        condition = "input.dataset == 'BCI_vg'",
        sliderInput("rare_steps_bc", label = "Rarefaction Steps", 
                    min = 1, max = 500, value = 1)
      ),
    ), 
    
    mainPanel(
      dataTableOutput("table"),
      
      hr(),
      plotlyOutput("plot_rarefy", width = "650px"),  # plot rarefy ()
      
      plotlyOutput("plot_rare", width = "650px"), # plot rarecurve()
      
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
  
  output$table = renderDT(myCSV(), options = list(pageLength = 10))
  
  
  # Reactive data frame  and calculate rarefy for plotting
  df_rarefy <- reactive({
    rarfy_sampl <- switch(input$dataset, # it connect with the slide choice
                          "dune_vg" = input$rarfy_sampl_dune, 
                          "mite_vg" = input$rarfy_sampl_mite,
                          "BCI_vg" = input$rarfy_sampl_BCI)
    # Calculate richcness "S" for the selected dataset
    S <- switch (input$dataset,
                 "dune_vg" = rowSums(dune_vg[, -(1:6)] > 0),
                 "mite_vg" = rowSums(mite_vg[, -(1:6)] > 0),
                 "BCI_vg" = rowSums(BCI_vg[ ,-(1:10)] > 0)
    )
    # Calculate rarefied data for the selected dataset and rarfy_sampl
    rarefied_data <- switch(input$dataset,
                            "dune_vg" = rarefy(dune_vg[, -(1:6)], rarfy_sampl),
                            "mite_vg" = rarefy(mite_vg[, -(1:6)], rarfy_sampl),
                            "BCI_vg" = rarefy(BCI_vg[ ,-(1:10)], rarfy_sampl)
    )                      
    data.frame(Richness = S, Rarefied = rarefied_data)%>% 
      mutate(Sample = rownames(.)) %>% # establish the sample id column
      select(Sample, Richness, Rarefied)
  })
  
  # Create the plot
  output$plot_rarefy <- renderPlotly({
    df <- df_rarefy()
    p <- ggplot(df, aes(x = Richness, y = Rarefied, color = Sample)) +
      geom_point(shape = 1, size = 3) +
      theme_bw() +
      labs(x = "Observed Richness (S)", y = "Rarefied No. of Species") +
      geom_abline(intercept = 0, slope = 1) +
      xlim(0, max(df$Richness, na.rm = TRUE)) +
      ylim(0, max(df$Rarefied, na.rm = TRUE)) +
      theme(legend.position = "none") 
    ggplotly(p)
  })
}
shinyApp(ui = ui, server = server)