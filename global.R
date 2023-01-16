library(shiny)
library(DT)
library(vegan) # ecology package
library(kableExtra) #edit and style tables
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

###########
data("dune")
data("dune.env")


denv <- dune.env %>%
  mutate(Sample=1:20) %>% # add a Sample column
  relocate(Sample) # move the sample column to the front

dn <- dune %>%
  mutate(Sample=1:20) %>% # add a Sample column
  relocate(Sample) # move the sample column to the front

#merge two datasets
dune_vg <- full_join(x = denv, y = dn, by ='Sample')


##########
data("mite")
data("mite.env")


menv <- mite.env %>%
  mutate(Sample=1:70) %>%# add a Sample column
  relocate(Sample) # move the sample column to the front

mt <- mite %>%
  mutate(Sample=1:70) %>% # add a Sample column
  relocate(Sample) # move the sample column to the front

#merge two datasets
mite_vg <- full_join(x = menv, y = mt, by ='Sample')


####################################################
data("BCI")
data("BCI.env")

benv <- BCI.env %>% 
  mutate(Sample=1:50) %>% # add a Sample column
  relocate(Sample)# move the sample column to the front

bc <- BCI %>%
  mutate(Sample=1:50) %>% # add a Sample column
  relocate(Sample)# move the sample column to the front

#merge two datasets
BCI_vg <- full_join(x = benv, y = bc, by ='Sample')
