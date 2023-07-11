library(shiny)
library(vegan) # ecology package
library(DT)
library(kableExtra) #edit and style tables
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(patchwork)
library(ggplotify) # to convert ggplot into grob
library(plotly)
library(gghighlight) #  ggplot labels at the end of the line

###########
data("BCI", "BCI.env")
data("dune", "dune.env")
data("mite", "mite.env")
# combine both dataset and make a column "Sample"
dune_vg <- cbind(Sample = rownames(dune.env), dune.env, dune)
BCI_vg  <- cbind(Sample = rownames(BCI.env), BCI.env, BCI)
mite_vg <- cbind(Sample = rownames(mite.env), mite.env, mite)