library(shiny)
library(DT)
library(vegan)

setwd("D:...Spdiv/SpDiv_ApProject/data")

#datasets

dune <- read.csv("D:...Spdiv/SpDiv_ApProject/data/dune.csv")
head(dune)

vegan.BCI <- read.csv("D:...Spdiv/SpDiv_ApProject/data/vegan.BCI.csv")
head(vegan.BCI)

mite.data <- read.csv("D:...Spdiv/SpDiv_ApProject/data/mite.csv")
head(mite.data)