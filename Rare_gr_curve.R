ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:",
                  c("dune_vg", "mite_vg", "BCI_vg")), 
      textOutput("description"),
      
      br(),
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
      
      checkboxInput("intersect_lines", "Smallest sample (xIntercetp)", value = FALSE),
      
      # choice of environmental variable
      conditionalPanel(
        condition = "input.dataset == 'dune_vg'", 
        selectInput("item_env_dn", 
                    label = HTML("Environmental Variables:<br/>
                                 <span style='font-size: 10px; font-style: italic; font-weight: normal;'> Based on Vegan R package descriptions (Oksanen et al. 2022) </span>"),
                    choices = c("A1", "Moisture", "Management", "Use", "Manure"),
                    selected = "Moisture"),
        textOutput("desc_dunevar")
      ),
      conditionalPanel(
        condition = "input.dataset == 'mite_vg'", 
        selectInput("item_env_mt", 
                    label = HTML("Choose an environmental parameter: <br/>
                                 <span style='font-size: 10px; font-style: italic; font-weight: normal;'> Based on Vegan R package descriptions (Oksanen et al. 2022) </span>"),
                    choices = c("SubsDens", "WatrCont", "Substrate", "Shrub", "Topo"),
                    selected = "Substrate"),
        textOutput("desc_mitevar")
      ),
      conditionalPanel(
        condition = "input.dataset == 'BCI_vg'", 
        selectInput("item_env_bc", 
                    label = HTML("Choose an environmental parameter: <br/>
                                 <span style='font-size: 10px; font-style: italic; font-weight: normal;'> Based on Vegan R package descriptions (Oksanen et al. 2022) </span>"),
                    choices = c("UTM.EW", "UTM.NS", "Age.cat", "Habitat", "Stream", "EnvHet"),
                    selected = "Age.cat"),
        textOutput("desc_bcvar")
      )
    ),
    
    mainPanel(
      dataTableOutput("table"),
      
      hr(),
      
      plotlyOutput("plot_rare", width = "650px", height = "450px"),
      plotlyOutput("plot_rarenv", width = "650px", height = "450px")
    )
  )
)


server <- function(input, output) {
  desc <- reactive({
    switch(input$dataset, # need to edit the texts
           "dune_vg" = "Vegetation data of the Dutch Dune Meadows, Terschelling, Netherlands. The study sampled 20 sites of 2x 2 square meters each and found 30 species of plants. For more details, check out R - Vegan package (Oksanen et al. 2022).",
           "mite_vg" = "The data includes morpho-species of oribatid mites (also known as moss or beetle mites) found in the vicinity of Lac Geai (in Québec, Canada). Mites were collected from Sphagnum moss samples (5 cm in diameter and 7 cm deep). The table includes 70 samples (rows) and 35 mite species (columns). For more details, check out R - Vegan package (Oksanen et al. 2022).",
           "BCI_vg" = "Floristic surveys on the Panamanian island of Barro Colorado (BCI) revealed the number of trees in a 1-hectare plot. The table shows the 225 species (columns) in 50 plots/samples (rows). Although the dataset contains nine environmental variables, the Index Table output only displays the ones that show variation (all, but Precipitation and Elevation). See the R - Vegan package for more information (Oksanen et al. 2022)."
    ) 
  })
  
  output$description = renderText(desc())
  
  output$desc_dunevar <- renderText({
    if (input$dataset == "dune_vg") {
      param <- input$item_env_dn
      switch(
        param,
        "A1" = "Thickness (cm) of the surface soil (A1 horizon), rich in organic matter and biological activity.",
        "Moisture" = "Soil moisture is divided into five levels (1 < 2 < 4 < 5).",
        "Management" = "Four types of grassland management: SF = standard farming, BF = biological farming, HF = hobby-farming, NM = nature conservation management.",
        "Use" = "Three types of land use: Hayfields, Grasslands and Haypastu (intermediate).",
        "Manure" = "Amount of manure ordered from zero to four (0 < 1 < 2 < 3 < 4)."
      )
    }
  })
  
  output$desc_mitevar <- renderText ({
    if (input$dataset == "mite_vg"){
      param <- input$item_env_mt
      switch(
        param, 
        "SubsDens" = "Substrate density (g/L), measured in grams per liter of dry uncompressed matter.",
        "WatrCont" = "Water content of the substrate (g/L).",
        "Substrate" = "Seven types of substrate assigned as: phagn1, Sphagn2, Sphagn3,Sphagn4, Litter, Barepeat and Interface (interface between Sphagnum species).",
        "Shrub" = "Three levels describing density of the shrub cover (1 < 2 < 3).",
        "Topo" = "Two tyoes of substratum morphology: Blanket and Hummock."
      )
    }
  })
  
  output$desc_bcvar <- renderText ({
    if (input$dataset == "BCI_vg"){
      param <- input$item_env_bc
      switch(
        param, 
        "UTM.EW" = "UTM coordinates (zone 17N) East-West.",
        "UTM.NS" = "UTM coordinates (zone 17N) North-South.",
        "Age.cat" = "Forest age category: c2 = mature secondary,  c3 = old growth or primary forest). ",
        "Habitat" = "Five types of habitat described as: Young = ca.100 years forest, OldSlope = old forests on > 7 degree slopes, OldLow = old forests under 152 m elevation, 
    OldHigh = at higher elevation, and Swamp forests.",
        "Stream" = "If there is streamside habitat (Yes or NO).",
        "EnvHet" = "Environmental Heterogeneity assessed as the Simpson diversity of frequencies of Habitat
types in 25 grid cells in the plot."
      )
    }
  })
  
  # displya the datatable:
  myCSV <- reactive({
    switch(input$dataset,
           "dune_vg" = dune_vg,
           "mite_vg" = mite_vg,
           "BCI_vg" = BCI_vg)
  })
  
  output$table = renderDT(myCSV(), options = list(pageLength = 10))
  
  
  # Reactive data frame  and calculate rarecurve for plotting
  df_rare <- reactive({
    rare_steps <- switch(input$dataset, # it connect with the slide choice
                         "dune_vg" = input$rare_steps_dn, 
                         "mite_vg" = input$rare_steps_mt,
                         "BCI_vg" = input$rare_steps_bc)
    # Calculate rarecurve
    rarecurve_data <- switch(input$dataset,
                             "dune_vg" = rarecurve(dune_vg[, -(1:6)], step= rare_steps),
                             "mite_vg" = rarecurve(mite_vg[, -(1:6)], step= rare_steps),
                             "BCI_vg" = rarecurve(BCI_vg[ ,-(1:10)], step= rare_steps) 
    )                      
    #  prepare for data frame
    names(rarecurve_data) <- switch(input$dataset,
                                    "dune_vg" = paste("Sample", 1:20, sep = ""),
                                    "mite_vg" = paste("Sample", 1:70, sep = ""),
                                    "BCI_vg" = paste("Sample", 1:50, sep = "")
    )
    ### for Dune Data frame   
    if (input$dataset == "dune_vg"){
      protox <- mapply(FUN = function(x, y, z, a, b, c, d) {
        pltdf <- as.data.frame(x)
        colnames(pltdf) <- "Richness"
        pltdf$Sample <- as.numeric(gsub("\\D", "", y))  # Extract site number from site name
        pltdf$Subsample <- attr(x, "Subsample")
        pltdf$A1 <- z
        pltdf$Moisture <- a
        pltdf$Management <- b
        pltdf$Use <- c
        pltdf$Manure <- d
        pltdf
      }, x = rarecurve_data, y = as.list(names(rarecurve_data)), z = dune_vg$A1, a = dune_vg$Moisture,
      b=dune_vg$Management, c=dune_vg$Use, d=dune_vg$Manure, SIMPLIFY = FALSE)
    }
    ### for Mite Data frame
    else if(input$dataset == "mite_vg"){
      protox <- mapply(FUN = function(x, y, z, a, b, c, d) {
        pltdf <- as.data.frame(x)
        colnames(pltdf) <- "Richness"
        pltdf$Sample <- as.numeric(gsub("\\D", "", y))  # Extract site number from site name
        pltdf$Subsample <- attr(x, "Subsample")
        pltdf$SubsDens <- z
        pltdf$WatrCont<- a
        pltdf$Substrate <- b
        pltdf$Shrub <- c
        pltdf$Topo <- d
        pltdf
      }, x = rarecurve_data, y = as.list(names(rarecurve_data)), z = mite_vg$SubsDens, a = mite_vg$WatrCont, 
      b=mite_vg$Substrate, c=mite_vg$Shrub, d=mite_vg$Topo, SIMPLIFY = FALSE)
    }
    
    ### for BCI Data frame
    else { 
      protox <- mapply(FUN = function(x, y, z, a, b, c, d, e) {
        pltdf <- as.data.frame(x)
        colnames(pltdf) <- "Richness"
        pltdf$Sample <- as.numeric(gsub("\\D", "", y))  # Extract site number from site name
        pltdf$Subsample <- attr(x, "Subsample")
        pltdf$UTM.EW <- z
        pltdf$UTM.NS <- a
        pltdf$Age.cat <- b
        pltdf$Habitat <- c
        pltdf$Stream <- d
        pltdf$EnvHet <- e
        pltdf
      }, x = rarecurve_data, y = as.list(names(rarecurve_data)), z = BCI_vg$UTM.EW, a = BCI_vg$UTM.NS, 
      b=BCI_vg$Age.cat, c=BCI_vg$Habitat, d=BCI_vg$Stream, e=BCI_vg$EnvHet, SIMPLIFY = FALSE)
    }
    
    xy <- do.call(rbind, protox)
    rownames(xy) <- NULL  # pretty
    return(xy)
  })
  
  # Create the plot
  output$plot_rare <- renderPlotly({
    xy <- df_rare()
    q <- ggplot(xy, aes(x = Subsample, y = Richness, group = Sample)) +
      geom_line(color = "#0570b0") +
      labs(x = "Sample Size", y = "Richness (S)") +
      theme_classic() +
      expand_limits(y = c(0, 15)) 
    
    # Calculate richness "smallest sample size" (rx) for the selected dataset
    if (input$intersect_lines) {
      data_rx <- switch(input$dataset, 
                        "dune_vg" = min(rowSums(dune_vg[, -(1:6)])),
                        "mite_vg" = min(rowSums(mite_vg[, -(1:6)])),
                        "BCI_vg" = min(rowSums(BCI_vg[, -(1:10)]))
      )
      q <- q + 
        geom_vline(xintercept =  data_rx)
    }
    ggplotly(q) 
  })
  #### output plot for the env variables  
  
  # Create a function to generate ggplotly plots for different environmental parameters
  generate_plotly_plot <- function(data, param, is_continuous) {
    p <- ggplot(data, aes(x = Subsample, y = Richness, color = .data[[param]])) +
      geom_line(aes(group = Sample), linewidth = 0.5) +
      labs(x = "Sample Size", y = "Richness (S)", color = param) +
      expand_limits(y = c(0, 15)) +
      theme_classic()
    
    if (is_continuous) {
      p <- p + scale_colour_gradient(low = "darkolivegreen1", high = "darkolivegreen", guide = "colourbar")
    } else {
      p <- p + scale_color_manual(values = col) +
        theme(legend.position = "bottom", legend.background = element_rect(fill = "white", color = "gray"))
    }
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.00, y = -0.25))
  }
  
  # Create reactive ggplot based on selected dataset and variable
  env_grplt <- reactive({
    xy <- df_rare()
    param <- switch(input$dataset,
                    "dune_vg" = input$item_env_dn,
                    "mite_vg" = input$item_env_mt,
                    "BCI_vg" = input$item_env_bc
    )
    
    # Create a list to store the plotly plots for each environmental parameter
    plot_list <- list()
    
    # Generate plotly plots for each environmental parameter and store them in the list
    for (param in env_params) {
      if (param %in% colnames(xy)) {
        is_continuous <- param %in% c("A1", "SubsDens", "WatrCont", "UTM.EW", "UTM.NS", "EnvHet")
        plot_list[[param]] <- generate_plotly_plot(xy, param, is_continuous)
      }
    }
    
    return(plot_list)
  })
  
  output$plot_rarenv <- renderPlotly({
    env_plts <- env_grplt()
    
    # Plot the categorical variables multicolor
    env_plt <- ggplotly(ggplot())  # Create an empty plotly plot by default
    
    if (input$dataset == "dune_vg") {
      if ("Moisture" %in% input$item_env_dn) {
        env_plt <- env_plts$Moisture
      } else if ("Management" %in% input$item_env_dn) {
        env_plt <- env_plts$Management
      } else if ("Use" %in% input$item_env_dn) {
        env_plt <- env_plts$Use
      } else if ("Manure" %in% input$item_env_dn) {
        env_plt <- env_plts$Manure
      } else if ("A1" %in% input$item_env_dn) {
        env_plt <- env_plts$A1
      }
    } else if (input$dataset == "mite_vg") {
      if ("Substrate" %in% input$item_env_mt) {
        env_plt <- env_plts$Substrate
      } else if ("Shrub" %in% input$item_env_mt) {
        env_plt <- env_plts$Shrub
      } else if ("Topo" %in% input$item_env_mt) {
        env_plt <- env_plts$Topo
      } else if ("SubsDens" %in% input$item_env_mt) {
        env_plt <- env_plts$SubsDens
      } else if ("WatrCont" %in% input$item_env_mt) {
        env_plt <- env_plts$WatrCont
      }
      
    } else if (input$dataset == "BCI_vg") {
      if ("Age.cat" %in% input$item_env_bc) {
        env_plt <- env_plts$Age.cat
      } else if ("Habitat" %in% input$item_env_bc) {
        env_plt <- env_plts$Habitat
      } else if ("Stream" %in% input$item_env_bc) {
        env_plt <- env_plts$Stream
      } else if ("UTM.EW" %in% input$item_env_bc) {
        env_plt <- env_plts$UTM.EW
      } else if ("UTM.NS" %in% input$item_env_bc) {
        env_plt <- env_plts$UTM.NS
      } else if ("EnvHet" %in% input$item_env_bc) {
        env_plt <- env_plts$EnvHet
      }
    }
    
    print(env_plt)
  })
}
shinyApp(ui = ui, server = server)

