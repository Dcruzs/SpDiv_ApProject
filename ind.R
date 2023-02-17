
########### Sp Richness Functions for TAB 1:####################

##########################
## choice: dune dataset ##
##########################
dataset <- dune_vg

N <- apply(dataset[ ,- 1:-6], 1,sum) # sum of non-zero sp columns, except the first 6 variables columns
data.frame(N)

## CALCULATE REACHNESS: # S is equal to Richness (R), counts the number of species recorded per site
S <- apply(dataset[,- 1:-6] > 0,1, sum) #sum up the number of non-zero entries per row (1)
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
Index_r_dn <- data.frame(dataset[,1:6], N, S, Mg, Mn, Od)

dn_richInd <- datatable(Index_r_dn)%>%
  formatStyle(c('Mg','Mn','Od'),  background = '#c6dbef')# formatting table style


##  PLOTS dune


### variable Moisture.dune: there are 5 levels of moisture
dn_moist_plotMg <- ggplot(Index_r_dn,aes(x= Moisture, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 4), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_moist_plotMn <- ggplot(Index_r_dn,aes(x= Moisture, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_moist_plotOd <- ggplot(Index_r_dn,aes(x=Moisture, y = Od)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,10), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

### PLOT variable Management.dune
dn_mang_plotMg <-  ggplot(Index_r_dn, aes(x= Management, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 4), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_mang_plotMn <-  ggplot(Index_r_dn, aes(x= Management, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_mang_plotOd <-  ggplot(Index_r_dn, aes(x= Management, y = Od)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,10), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


### variable Use.dune
dn_use_plotMg <-  ggplot(Index_r_dn,aes(x= Use, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 4), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_use_plotMn <-  ggplot(Index_r_dn,aes(x= Use, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_use_plotOd <-  ggplot(Index_r_dn,aes(x=Use, y = Od)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,10), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


### variable Manure.dune: 
dn_manu_plotMg <-  ggplot(Index_r_dn,aes(x= Manure, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 4), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_manu_plotMn <-  ggplot(Index_r_dn,aes(x= Manure, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_manu_plotOd <-  ggplot(Index_r_dn,aes(x=Manure, y = Od)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,10), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


###########################
### choice: Mite dataset ##
###########################
dataset <- mite_vg ## call data as dataset to apply the functions
N <- apply(dataset[ ,- 1:-6], 1,sum) # sum of non-zero sp columns, except the first 6 variables columns
data.frame(N)

## CALCULATE REACHNESS: # S is equal to Richness (R), counts the number of species recorded per site
S <- apply(dataset[,- 1:-6] >0,1, sum) #sum up the number of non-zero entries per row (1)
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
Index_r_mt <- data.frame(dataset[,1:6], N, S, Mg, Mn, Od)

mt_richInd <- datatable(Index_r_mt)%>%
  formatStyle( c('Mg','Mn','Od'),  background = '#c6dbef') # formatting table style
mt_richInd 

## PLots MITE

### PLOT variable Substrate. Mite
mt_subs_plotMg <- ggplot(Index_r_mt, aes(x= Substrate, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,6), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))  

mt_subs_plotMn <-  ggplot(Index_r_mt, aes(x= Substrate, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,4), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))  

mt_subs_plotOd <- ggplot(Index_r_mt, aes(x= Substrate, y = Od)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 12), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))  

### variable Shrub.mite
mt_shru_plotMg <- ggplot(Index_r_mt, aes(x= Shrub, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,6), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

mt_shru_plotMn <- ggplot(Index_r_mt, aes(x= Shrub, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,4), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


mt_shru_plotOd <- ggplot(Index_r_mt, aes(x=Shrub, y = Od)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 12), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


### variable Topo.mite: 
mt_topo_plotMg <-  ggplot(Index_r_mt, aes(x= Topo, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,6), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

mt_topo_plotMn <- ggplot(Index_r_mt, aes(x= Topo, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,4), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

mt_topo_plotOd <-  ggplot(Index_r_mt, aes(x=Topo, y = Od)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 12), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

###########################
### choice: BCI dataset ##
###########################
dataset <- BCI_vg[, -c(2,3,4,5,7)] ## call data as dataset to apply the functions and DELETE non-variation env.data from the row dataset

N <- apply(dataset[ ,- 1:-5], 1,sum) # sum of non-zero sp columns, except the first 6 variables columns
data.frame(N)

## CALCULATE REACHNESS: # S is equal to Richness (R), counts the number of species recorded per site
S <- apply(dataset[,- 1:-5] >0,1, sum) #sum up the number of non-zero entries per row (1)
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
Index <- data.frame(dataset[,1:5], N, S, Mg, Mn, Od)

bc_richInd <- datatable(Index) %>%
  formatStyle( c('Mg','Mn','Od'),  background = '#c6dbef') # formatting table style
bc_richInd

## PLot AGE.CAT BCI
bc_age_plotMg <- ggplot(Index, aes(x= Age.cat, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(12,18), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_age_plotMn <- ggplot(Index, aes(x= Age.cat, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,6), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_age_plotOd <- ggplot(Index, aes(x= Age.cat, y = Od)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(30, 40), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

## PLot Habitat BCI
bc_habt_plotMg <- ggplot(Index, aes(x= Habitat, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(12,18), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_habt_plotMn <- ggplot(Index, aes(x= Habitat, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,6), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_habt_plotOd <- ggplot(Index, aes(x= Habitat, y = Od)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(30, 40), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

## PLot STREAM BCI
bc_strm_plotMg <- ggplot(Index, aes(x= Stream, y = Mg)) + 
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(12,18), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_strm_plotMn <- ggplot(Index, aes(x= Stream, y = Mn)) + 
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,6), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_strm_plotOd <- ggplot(Index, aes(x= Stream, y = Od)) + 
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(30, 40), name="Odum (Od)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  





################################ Diversity Index ##############################################################################


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
dnDivIndex <- data.frame(dataset[,1:6], N, S, D, H, E)
dn_spdiv_index <- datatable(dnDivIndex) %>%
  formatStyle( c('D','H', 'E'),  background = "palegreen") # formatting table style
dn_spdiv_index


##  PLOTS dune

### variable Moisture.dune: there are 5 levels of moisture
dn_moist_plotD <- ggplot(dnDivIndex,aes(x= Moisture, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.65, 1.0), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_moist_plotH <- ggplot(dnDivIndex,aes(x= Moisture, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(1.30, 2.8), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_moist_plotE <- ggplot(dnDivIndex,aes(x=Moisture, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.8, 1.10), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


### PLOT variable Management.dune
dn_mang_plotD <- ggplot(dnDivIndex, aes(x= Management, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.65, 1.0), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_mang_plotH <-  ggplot(dnDivIndex, aes(x= Management, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(1.30, 2.8), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_mang_plotE <-  ggplot(dnDivIndex, aes(x= Management, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.8, 1.10), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


### variable Use.dune
dn_use_plotD <- ggplot(dnDivIndex,aes(x= Use, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.65, 1.0), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_use_plotH <- ggplot(dnDivIndex,aes(x= Use, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(1.30, 2.8), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_use_plotE <- ggplot(dnDivIndex,aes(x=Use, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.8, 1.10), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  



### variable Manure.dune: 
dn_manu_plotD <-  ggplot(dnDivIndex,aes(x= Manure, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.65, 1.0), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_manu_plotH <-  ggplot(dnDivIndex,aes(x= Manure, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(1.30, 2.8), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_manu_plotE <-  ggplot(dnDivIndex,aes(x=Manure, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.8, 1.10), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


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
mtDivIndex <- data.frame(dataset[,1:6], N, S, D, H, E)
mt_spdiv_index <- datatable(mtDivIndex) %>%
  formatStyle( c('D','H', 'E'),  background = "palegreen") # formatting table style
mt_spdiv_index

##########################

### PLOT variable Substrate. Mite
mt_subs_plotD <-  ggplot(mtDivIndex, aes(x= Substrate, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))  


mt_subs_plotH <-  ggplot(mtDivIndex,aes(x= Substrate, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))  
mt_subs_plotH

mt_subs_plotE <-  ggplot(mtDivIndex,aes(x= Substrate, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))  



### variable Shrub.mite
mt_shru_plotD <- ggplot(mtDivIndex,aes(x= Shrub, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


mt_shru_plotH <- ggplot(mtDivIndex,aes(x= Shrub, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


mt_shru_plotE <- ggplot(mtDivIndex,aes(x=Shrub, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


### variable Topo.mite: 
mt_topo_plotD <-  ggplot(mtDivIndex,aes(x= Topo, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


mt_topo_plotH <-  ggplot(mtDivIndex,aes(x= Topo, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


mt_topo_plotE <-  ggplot(mtDivIndex,aes(x=Topo, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  



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
bcDivIndex <- data.frame(dataset[,1:5], N, S, D, H, E)
bc_spdiv_index <- datatable(bcDivIndex) %>%
  formatStyle( c('D','H', 'E'),  background = "palegreen") # formatting table style
bc_spdiv_index

### PLot Sp_div index vs the samples (order according to size)
## order the "DivIndex" with size (N)

### PLOT variable Age.cat.bci
bc_age_plotD <-  ggplot(bcDivIndex, aes(x= Age.cat, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.6,1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


bc_age_plotH <-  ggplot(bcDivIndex,aes(x= Age.cat, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


bc_age_plotE <-  ggplot(bcDivIndex,aes(x= Age.cat, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  



### variable Habitat.bci
bc_habt_plotD <- ggplot(bcDivIndex,aes(x= Habitat, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


bc_habt_plotH <- ggplot(bcDivIndex,aes(x= Habitat, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,5), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


bc_habt_plotE <- ggplot(bcDivIndex,aes(x=Habitat, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1.2), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


### variable Stream.bci
bc_strm_plotD <-  ggplot(bcDivIndex,aes(x= Stream, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


bc_strm_plotH <-  ggplot(bcDivIndex,aes(x= Stream, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,5), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


bc_strm_plotE <-  ggplot(bcDivIndex,aes(x=Stream, y = E)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Evenness (E)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

