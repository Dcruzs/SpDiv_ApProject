library(shiny)
library(DT)
library(vegan)

setwd("D:/DataQuest/ShinyApp/Spdiv/SpDiv_ApProject")

#datasets

dune <- read.csv("D:/DataQuest/ShinyApp/Spdiv/SpDiv_ApProject/data/dune.csv")
head(dune)

vegan.BCI <- read.csv("D:/DataQuest/ShinyApp/Spdiv/SpDiv_ApProject/data/vegan.BCI.csv")
head(vegan.BCI)

mite.data <- read.csv("D:/DataQuest/ShinyApp/Spdiv/SpDiv_ApProject/data/mite.csv")
head(mite.data)