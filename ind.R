
########### Sp Div Functions for the outcome TABLE:####################

library(ecoevoapps)
library(kableExtra) #edit and style tables
library(data.table)
library(DT)
library(vegan)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

##########################
## choice: dune dataset ##
##########################
dataset <- read.csv("D:...Spdiv/SpDiv_ApProject/data/dune.csv")

# N = abundance of each sample;total number of individuals of a sample/site
N <- apply(dataset[ ,-1], 1,sum)# measure the number of indiv. per sites (sample)
data.frame(N)
## CALCULATE REACHNESS: # S is equal to Richness (R), counts the number of species recorded per site
S <- apply(dataset[,-1] >0,1, sum) #sum up the number of non-zero entries per row (1)
data.frame(S)

## Menhinick (Mn)
Menhinick <- function (S,N){
  index <- S/sqrt(N)
  return(index)
}
Mn <- Menhinick(S,N) 
Mn <- round(Mn,2)
Mn

## Margalef (Mg)
Margalef <- function (S,N){
  index <- (S-1)/log(N)
  return(index)
}
Mg <- Margalef(S,N)
Mg <-round(Mg,2)
Mg

#checking mine function for Odum
Odum <- function(S, N){
  index <- S/log10(N)
  return(index)
}
Od <- Odum(S,N)
Od <- round(Od,2)
Od
# OUTPUT TABLE #
########### #Combine all indices #####
Index <- data.frame(dataset$sample, N, S, Mg, Mn, Od)
setnames(Index, old = c("dataset.sample"), new = c("Sample"), skip_absent = TRUE) #change the name of the column "sample"
dn_richInd <- datatable(Index)%>%
  formatStyle(c('Mg','Mn','Od'),  background = '#c6dbef')  # formatting table style


###########################
### choice: Mite dataset ##
###########################
dataset <- read.csv("D:...Spdiv/SpDiv_ApProject/data/mite.csv")
N <- apply(dataset[ ,-1], 1,sum)# measure the number of indiv. per sites (sample)
data.frame(N)
## CALCULATE REACHNESS: # S is equal to Richness (R), counts the number of species recorded per site
S <- apply(dataset[,-1] >0,1, sum) #sum up the number of non-zero entries per row (1)
data.frame(S)

## Menhinick (Mn)
Menhinick <- function (S,N){
  index <- S/sqrt(N)
  return(index)
}
Mn <- Menhinick(S,N) 
Mn <- round(Mn,2)
Mn

## Margalef (Mg)
Margalef <- function (S,N){
  index <- (S-1)/log(N)
  return(index)
}
Mg <- Margalef(S,N)
Mg <-round(Mg,2)
Mg

#checking mine function for Odum
Odum <- function(S, N){
  index <- S/log10(N)
  return(index)
}
Od <- Odum(S,N)
Od <- round(Od,2)
Od
# OUTPUT TABLE #
########### #Combine all indices #####
Index <- data.frame(dataset$sample, N, S, Mg, Mn, Od)
setnames(Index, old = c("dataset.sample"), new = c("Sample"), skip_absent = TRUE) #change the name of the column "sample"
mt_richInd <- datatable(Index)%>%
  formatStyle( c('Mg','Mn','Od'),  background = '#c6dbef') # formatting table style

###########################
### choice: BCI dataset ##
###########################
dataset <- read.csv("D:...Spdiv/SpDiv_ApProject/data/vegan.BCI.csv")
N <- apply(dataset[ ,-1], 1,sum)# measure the number of indiv. per sites (sample)
data.frame(N)
## CALCULATE REACHNESS: # S is equal to Richness (R), counts the number of species recorded per site
S <- apply(dataset[,-1] >0,1, sum) #sum up the number of non-zero entries per row (1)
data.frame(S)

## Menhinick (Mn)
Menhinick <- function (S,N){
  index <- S/sqrt(N)
  return(index)
}
Mn <- Menhinick(S,N) 
Mn <- round(Mn,2)
Mn

## Margalef (Mg)
Margalef <- function (S,N){
  index <- (S-1)/log(N)
  return(index)
}
Mg <- Margalef(S,N)
Mg <-round(Mg,2)
Mg

#checking mine function for Odum
Odum <- function(S, N){
  index <- S/log10(N)
  return(index)
}
Od <- Odum(S,N)
Od <- round(Od,2)
Od
# OUTPUT TABLE #
########### #Combine all indices #####
Index <- data.frame(dataset$sample, N, S, Mg, Mn, Od)
setnames(Index, old = c("dataset.sample"), new = c("Sample"), skip_absent = TRUE) #change the name of the column "sample"
bc_richInd <- datatable(Index) %>%
  formatStyle( c('Mg','Mn','Od'),  background = '#c6dbef') # formatting table style