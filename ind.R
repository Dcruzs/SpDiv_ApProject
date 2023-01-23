
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






################################ Diversity Index###############################


########### Diversity Index Functions for the outcome TABLE:####################
# N = abundance of each sample;total number of individuals of a sample/site

##########################
## choice: dune dataset ##
##########################
dataset <- dune_vg ## call data as dataset to apply the functions
####
N <- apply(dataset[ ,- 1:-6], 1,sum) # sum of non-zero sp columns, except the first 6 variables columns
data.frame(N)

## CALCULATE REACHNESS: # S is equal to Richness (R), counts the number of species recorded per site
S <- apply(dataset[,- 1:-6] >0,1, sum) #sum up the number of non-zero entries per row (1)
data.frame(S)

## Simpson's Index (Ds), using vegan:
D <- diversity(dataset[- 1:-6], index = "simpson")
D <- round(D,2)
D

## Shannon's Index (H'), using vegan::
H <- diversity(dataset[- 1:-6], index = "shannon")
H <- round(H, 2) # round() NOT working here :(
H

#### Species Evenness, correction /J' of Pielou (1975):
flut <- dataset[- 1:-6]# must eliminate the categorical column and let only the numeric values
# using Vegan
E <- H/log(specnumber(flut))
E <- round(E, 2)
E

# OUTPUT TABLE #
########### #Combine all indices (D, H', E)  with the variables #####
DivIndex <- data.frame(dataset[,1:6], N, S, D, H, E)
dn_spdiv_index <- datatable(DivIndex) %>%
  formatStyle( c('D','H', 'E'),  background = "palegreen") # formatting table style
dn_spdiv_index

###########################
### choice: Mite dataset ##
###########################
dataset <- mite_vg ## call data as dataset to apply the functions
####
N <- apply(dataset[ ,- 1:-6], 1,sum) # sum of non-zero sp columns, except the first 6 variables columns
data.frame(N)

## CALCULATE REACHNESS: # S is equal to Richness (R), counts the number of species recorded per site
S <- apply(dataset[,- 1:-6] >0,1, sum) #sum up the number of non-zero entries per row (1)
data.frame(S)

## Simpson's Index (Ds), using vegan:
D <- diversity(dataset[- 1:-6], index = "simpson")
D <- round(D,2)
D

## Shannon's Index (H'), using vegan::
H <- diversity(dataset[- 1:-6], index = "shannon")
H <- round(H, 2) # round() NOT working here :(
H

#### Species Evenness, correction /J' of Pielou (1975):
flut <- dataset[- 1:-6]# must eliminate the categorical column and let only the numeric values
# using Vegan
E <- H/log(specnumber(flut))
E <- round(E, 2)
E

# OUTPUT TABLE #
########### #Combine all indices (D, H', E)  with the variables #####
DivIndex <- data.frame(dataset[,1:6], N, S, D, H, E)
mt_spdiv_index <- datatable(DivIndex) %>%
  formatStyle( c('D','H', 'E'),  background = "palegreen") # formatting table style
mt_spdiv_index


###########################
### choice: BCI dataset ##
###########################
dataset <- BCI_vg[, -c(2,3,4,5,7)]## call data as dataset to apply the functions and DELETE non-variation env.data from the row dataset
####
N <- apply(dataset[ ,- 1:-5], 1,sum) # sum of non-zero sp columns, except the first 6 variables columns
data.frame(N)

## CALCULATE REACHNESS: # S is equal to Richness (R), counts the number of species recorded per site
S <- apply(dataset[,- 1:-5] >0,1, sum) #sum up the number of non-zero entries per row (1)
data.frame(S)

## Simpson's Index (Ds), using vegan:
D <- diversity(dataset[- 1:-5], index = "simpson")
D <- round(D,2)
D

## Shannon's Index (H'), using vegan::
H <- diversity(dataset[- 1:-5], index = "shannon")
H <- round(H, 2) # round() NOT working here :(
H

#### Species Evenness, correction /J' of Pielou (1975):
flut <- dataset[- 1:-5]# must eliminate the categorical column and let only the numeric values
# using Vegan
E <- H/log(specnumber(flut))
E <- round(E, 2)
E

# OUTPUT TABLE #
########### #Combine all indices (D, H', E)  with the variables #####
DivIndex <- data.frame(dataset[,1:5], N, S, D, H, E)
bc_spdiv_index <- datatable(DivIndex) %>%
  formatStyle( c('D','H', 'E'),  background = "palegreen") # formatting table style
bc_spdiv_index

### PLot Sp_div index vs the samples (order according to size)
## order the "DivIndex" with size (N)

