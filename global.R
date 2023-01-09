library(shiny)
library(DT)
library(vegan) # ecology package
library(kableExtra) #edit and style tables
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

setwd("D:...Spdiv/SpDiv_ApProject/data")

#datasets

dune <- read.csv("D:...Spdiv/SpDiv_ApProject/data/dune.csv")
head(dune)

vegan.BCI <- read.csv("D:...Spdiv/SpDiv_ApProject/data/vegan.BCI.csv")
head(vegan.BCI)

mite.data <- read.csv("D:...Spdiv/SpDiv_ApProject/data/mite.csv")
head(mite.data)