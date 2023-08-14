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


########### Sp Div Functions for the outcome TABLE:####################
###########
data("dune", "dune.env")
data("mite", "mite.env")
data("BCI", "BCI.env")
# combine both dataset and make a column "Sample"
dune_vg <- cbind(Sample = rownames(dune.env), dune.env, dune)
mite_vg <- cbind(Sample = rownames(mite.env), mite.env, mite)
BCI_vg  <- cbind(Sample = rownames(BCI.env), BCI.env, BCI)


##########################
## choice: dune dataset ##
##########################
# N = abundance; total number of individuals in the sample 
N <- rowSums(dune_vg[, -(1:6)]) # sum of individuals sp columns, except the first 6 variables columns
# Indices
## RICHNESS: # S is equal to Richness (R), counts the number of species recorded in the sample
S <- rowSums(dune_vg[, -(1:6)] > 0)  #sum up the number of non-zero entries per row 

## Margalef (Mg)
Margalef <- function (S,N){
  index <- (S-1)/log(N)
  return(index)
}
Mg <- Margalef(S,N)
Mg <-round(Mg,2)
Mg

## Menhinick (Mn)
Menhinick <- function (S,N){
  index <- S/sqrt(N)
  return(index)
}
Mn <- Menhinick(S,N) 
Mn <- round(Mn,2)
Mn

# OUTPUT TABLE #
########### #Combine all indices #####
Index_r_dn <- data.frame(dune_vg[,(1:6)], N, S, Mg, Mn)
Index_r_dn

dn_richInd <- datatable(Index_r_dn)%>%
  formatStyle(c('S', 'Mg','Mn'),  background = '#c6dbef')# formatting table style
dn_richInd

##  PLOTS dune

### variable Moisture.dune: there are 4 levels of moisture (1,2,4,5)
dn_moist_plotS <- ggplot(Index_r_dn,aes(x=Moisture, y = S)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,20), name="Richness (S)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

dn_moist_plotMg <- ggplot(Index_r_dn,aes(x= Moisture, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 4), name="Margalef (Mg)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_moist_plotMn <- ggplot(Index_r_dn,aes(x= Moisture, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  



### PLOT variable Management.dune
dn_mang_plotS <-  ggplot(Index_r_dn, aes(x= Management, y = S)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,20), name="Richness (S)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

dn_mang_plotMg <-  ggplot(Index_r_dn, aes(x= Management, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 4), name="Margalef (Mg)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_mang_plotMn <-  ggplot(Index_r_dn, aes(x= Management, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))


### variable Use.dune
dn_use_plotS <-  ggplot(Index_r_dn,aes(x=Use, y = S)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,20), name="Richness (S)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

dn_use_plotMg <-  ggplot(Index_r_dn,aes(x= Use, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 4), name="Margalef (Mg)") +  # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_use_plotMn <-  ggplot(Index_r_dn,aes(x= Use, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))    

### variable Manure.dune: 
dn_manu_plotS <-  ggplot(Index_r_dn,aes(x=Manure, y = S)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,20), name="Richness (S)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 


dn_manu_plotMg <-  ggplot(Index_r_dn,aes(x= Manure, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 4), name="Margalef (Mg)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


dn_manu_plotMn <-  ggplot(Index_r_dn,aes(x= Manure, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") + # changing scale
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  


# PLot the numerical variables

# A1. dune
dn_a1_plotS <- ggplot(data = Index_r_dn, aes(x= A1, y= S)) + 
  labs(x = "Thickness of soil A1 horizon", y = "Richness") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 


dn_a1_plotMg  <- ggplot(data = Index_r_dn, aes(x= A1, y= Mg)) + 
  labs(x = "Thickness of soil A1 horizon", y = "Margalef (Mg)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 


dn_a1_plotMn <-  ggplot(data = Index_r_dn, aes(x= A1, y= Mn)) + 
  labs(x = "Thickness of soil A1 horizon", y = "Menhinick (Mn)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

###########################
### choice: Mite dataset ##
###########################

# N = abundance; total number of individuals in the sample 
N <-rowSums(mite_vg[ ,-(1:6)])# sum of individuals sp columns, except the first 6 variables columns
# Indices
## RICHNESS: # S is equal to Richness (R), counts the number of species recorded in the sample
S <- rowSums(mite_vg[, -(1:6)] > 0)  #sum up the number of non-zero entries per row 

## Margalef (Mg)
Margalef <- function (S,N){
  index <- (S-1)/log(N)
  return(index)
}
Mg <- Margalef(S,N)
Mg <-round(Mg,2)
Mg

## Menhinick (Mn)
Menhinick <- function (S,N){
  index <- S/sqrt(N)
  return(index)
}
Mn <- Menhinick(S,N) 
Mn <- round(Mn,2)
Mn

# OUTPUT TABLE #
########### #Combine all indices #####
Index_r_mt <- data.frame(mite_vg[,(1:6)], N, S, Mg, Mn)
Index_r_mt

mt_richInd <- datatable(Index_r_mt)%>%
  formatStyle(c('S', 'Mg','Mn'),  background = '#c6dbef')# formatting table style
mt_richInd


## PLots MITE

### PLOT variable Substrate. Mite
mt_subs_plotS <- ggplot(Index_r_mt, aes(x= Substrate, y = S)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30), name="Richness (S)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))  


mt_subs_plotMg <- ggplot(Index_r_mt, aes(x= Substrate, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,6), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))  

mt_subs_plotMn <-  ggplot(Index_r_mt, aes(x= Substrate, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))   

### variable Shrub.mite
mt_shru_plotS <- ggplot(Index_r_mt, aes(x= Shrub, y = S)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30), name="Richness (S)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

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
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))    


### variable Topo.mite: 
mt_topo_plotS <-  ggplot(Index_r_mt, aes(x=Topo, y = S)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30), name="Richness (S)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

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
  scale_y_continuous(limits = c(0,3), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

# PLot the numerical variables

# SubsDens. Mite
mt_den_plotS <- ggplot(data = Index_r_mt, aes(x= SubsDens, y= S)) + 
  labs(x = "Substrate density (g/L)", y = "Richness") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_den_plotMg  <- ggplot(data = Index_r_mt, aes(x= SubsDens, y= Mg)) + 
  labs(x = "Substrate density (g/L)", y = "Margalef (Mg)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_den_plotMn <-  ggplot(data = Index_r_mt, aes(x= SubsDens, y= Mn)) + 
  labs(x = "Substrate density (g/L)", y = "Menhinick (Mn)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

# WatrCont. Mite
mt_wt_plotS <- ggplot(data = Index_r_mt, aes(x= WatrCont, y= S)) + 
  labs(x = "Water content (g/L)", y = "Richness") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_wt_plotMg <- ggplot(data = Index_r_mt, aes(x= WatrCont, y= Mg)) + 
  labs(x = "Water content (g/L)", y = "Margalef (Mg)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_wt_plotMn <-  ggplot(data = Index_r_mt, aes(x= WatrCont, y= Mn)) + 
  labs(x = "Water content (g/L)", y = "Menhinick (Mn)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

###########################
### choice: BCI dataset ##
###########################
# N = abundance; total number of individuals in the sample 
N <-rowSums(BCI_vg[ ,-(1:10)])# sum of individuals sp columns, except the first 10 variables columns
# Indices
## RICHNESS: # S is equal to Richness (R), counts the number of species recorded in the sample
S <- rowSums(BCI_vg[ ,-(1:10)] > 0)  #sum up the number of non-zero entries per row 

## Margalef (Mg)
Margalef <- function (S,N){
  index <- (S-1)/log(N)
  return(index)
}
Mg <- Margalef(S,N)
Mg <-round(Mg,2)
Mg


## Menhinick (Mn)
Menhinick <- function (S,N){
  index <- S/sqrt(N)
  return(index)
}
Mn <- Menhinick(S,N) 
Mn <- round(Mn,2)
Mn

# OUTPUT TABLE #
########### #Combine all indices #####
Index_r_bc <- data.frame(BCI_vg[ ,(1:10)], N, S, Mg, Mn)

bc_richInd <- datatable(Index_r_bc) %>%
  formatStyle( c('S','Mg','Mn'),  background = '#c6dbef') # formatting table style
bc_richInd


## PLot AGE.CAT BCI
bc_age_plotS <- ggplot(Index_r_bc, aes(x= Age.cat, y = S)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(60, 120), name="Richness (S)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_age_plotMg <- ggplot(Index_r_bc, aes(x= Age.cat, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(12,18), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_age_plotMn <- ggplot(Index_r_bc, aes(x= Age.cat, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,6), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))    

## PLot Habitat BCI
bc_habt_plotS <- ggplot(Index_r_bc, aes(x= Habitat, y = S)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(60, 120), name="Richness (S)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_habt_plotMg <- ggplot(Index_r_bc, aes(x= Habitat, y = Mg)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(12,18), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_habt_plotMn <- ggplot(Index_r_bc, aes(x= Habitat, y = Mn)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,6), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

## PLot STREAM BCI
bc_strm_plotS <- ggplot(Index_r_bc, aes(x= Stream, y = S)) + 
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(60, 120), name="Richness (S)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_strm_plotMg <- ggplot(Index_r_bc, aes(x= Stream, y = Mg)) + 
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(12,18), name="Margalef (Mg)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_strm_plotMn <- ggplot(Index_r_bc, aes(x= Stream, y = Mn)) + 
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,6), name="Menhinick (Mn)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))    

# PLot the numerical variables
# No plot  made for Precipitation, Elevation and Geology because there is not variation.  

summary(Index_r_bc$UTM.EW)
hist(Index_r_bc$UTM.EW)
boxplot(Index_r_bc$UTM.EW)

# UTM.EW. BCI
bc_ew_plotS <- ggplot(data = Index_r_bc, aes(x= UTM.EW, y= S)) + 
  labs(x = "East-West coordinates", y = "Richness") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ew_plotMg <- ggplot(data = Index_r_bc, aes(x= UTM.EW, y= Mg)) + 
  labs(x = "East-West coordinates", y = "Margalef (Mg)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ew_plotMn <-  ggplot(data = Index_r_bc, aes(x= UTM.EW, y= Mn)) + 
  labs(x = "East-West coordinates", y = "Menhinick (Mn)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 


# UTM.NS. BCI
bc_ns_plotS <- ggplot(data = Index_r_bc, aes(x= UTM.NS, y= S)) + 
  labs(x = "North-South coordinates", y = "Richness") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ns_plotMg <- ggplot(data = Index_r_bc, aes(x= UTM.NS, y= Mg)) + 
  labs(x = "North-South coordinates", y = "Margalef (Mg)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ns_plotMg <-  ggplot(data = Index_r_bc, aes(x= UTM.NS, y= Mn)) + 
  labs(x = "North-South coordinates", y = "Menhinick (Mn)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

## EnvHet = Environmental Heterogeneity assessed as the Simpson diversity of frequencies of Habitat types in 25 grid cells in the plot.

# EnvHet. BCI
bc_het_plotS <- ggplot(data = Index_r_bc, aes(x= EnvHet, y= S)) + 
  labs(x = "Habitat heterogeneity (Simpson's Index)", y = "Richness") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_het_plotMg <- ggplot(data = Index_r_bc, aes(x= EnvHet, y= Mg)) + 
  labs(x = "Habitat heterogeneity (Simpson's Index)", y = "Margalef (Mg)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_het_plotMn <-  ggplot(data = Index_r_bc, aes(x= EnvHet, y= Mn)) + 
  labs(x = "Habitat heterogeneity (Simpson's Index)", y = "Menhinick (Mn)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 
################################ Diversity Index ##############################################################################



##########################
## choice: dune dataset ##
##########################
N <- rowSums(dune_vg[, -(1:6)]) # sum of individuals sp columns, except the first 6 variables columns
## RICHNESS: # S is equal to Richness (R), counts the number of species recorded in the sample
S <- rowSums(dune_vg[, -(1:6)] > 0)  #sum up the number of non-zero entries per row 
# Div Index: 
## Shannon Index (H), using vegan::
H <- diversity(dune_vg[, -(1:6)], index = "shannon") %>% round(.,2) 
## Simpson's Index (D), using vegan:
D <- diversity(dune_vg[, -(1:6)], index = "simpson") %>% round(.,2)
#### Species Evenness, correction /J' of Pielou (1975): using Vegan
J <- round(H/log(S), 2) 
#### Inverse Simpson from vegan,equivalent to Hill number of order 2 
Di <- diversity(dune_vg[, -(1:6)], index = "invsimpson") %>% round(.,2)

# OUTPUT TABLE #
########### #Combine all indices (D, H', E)  with the variables #####
dnDivIndex <- data.frame(dune_vg[,1:6], N, S, H, D, J, Di)
dn_spdiv_index <- datatable(dnDivIndex) %>%
  formatStyle( c('H', 'D', 'J', 'Di'),  background = "palegreen") # formatting table style
dn_spdiv_index

##  PLOTS dune

### variable Moisture.dune: 
dn_moist_plotH <- ggplot(dnDivIndex,aes(x= Moisture, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(1.30, 2.8), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_moist_plotD <- ggplot(dnDivIndex,aes(x= Moisture, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.65, 1.0), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_moist_plotJ <- ggplot(dnDivIndex,aes(x=Moisture, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.8, 1.10), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_moist_plotDi <- ggplot(dnDivIndex,aes(x= Moisture, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))    

### PLOT variable Management.dune
dn_mang_plotH <-  ggplot(dnDivIndex, aes(x= Management, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(1.30, 2.8), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_mang_plotD <- ggplot(dnDivIndex, aes(x= Management, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.65, 1.0), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_mang_plotJ <-  ggplot(dnDivIndex, aes(x= Management, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.8, 1.10), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_mang_plotDi <- ggplot(dnDivIndex,aes(x= Management, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))

### variable Use.dune
dn_use_plotH <- ggplot(dnDivIndex,aes(x= Use, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(1.30, 2.8), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

dn_use_plotD <- ggplot(dnDivIndex,aes(x= Use, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.65, 1.0), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_use_plotJ <- ggplot(dnDivIndex,aes(x=Use, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.8, 1.10), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

dn_use_plotDi <- ggplot(dnDivIndex,aes(x= Use, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

### variable Manure.dune: 
dn_manu_plotH <-  ggplot(dnDivIndex,aes(x= Manure, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(1.30, 2.8), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_manu_plotD <-  ggplot(dnDivIndex,aes(x= Manure, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.65, 1.0), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_manu_plotJ <-  ggplot(dnDivIndex,aes(x=Manure, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.8, 1.10), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

dn_manu_plotDi <- ggplot(dnDivIndex,aes(x= Manure, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))

###  Numerical variables
# A1. dune
dn_a1_plotH <-  ggplot(data = dnDivIndex,aes(x= A1, y = H)) + # changing the Index
  labs(x = "Thickness of soil A1 horizon", y = "Shannon's Index (H)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))

dn_a1_plotD <-  ggplot(data = dnDivIndex,aes(x= A1, y = D)) + # changing the Index
  labs(x = "Thickness of soil A1 horizon", y = "Simpson's Index (D)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))

dn_a1_plotJ <-  ggplot(data = dnDivIndex,aes(x= A1, y = J)) + # changing the Index
  labs(x = "Thickness of soil A1 horizon", y = "Pielou Index (J)") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))

dn_a1_plotDi <- ggplot(data = dnDivIndex,aes(x= A1, y = Di)) + # changing the Index
  labs(x = "Thickness of soil A1 horizon", y = "Inverse Simpson Index (Di)") +
  geom_point(size = 4, shape = 21, fill = "#fd8d3c", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))


###########################
### choice: Mite dataset ##
###########################

# N = abundance; total number of individuals in the sample 
N <-rowSums(mite_vg[ ,-(1:6)])# sum of individuals sp columns, except the first 6 variables columns
## RICHNESS: # S is equal to Richness (R), counts the number of species recorded in the sample
S <- rowSums(mite_vg[, -(1:6)] > 0)  #sum up the number of non-zero entries per row 
# Div Index:
## Shannon Index (H), using vegan::
H <- diversity(mite_vg[ ,-(1:6)], index = "shannon") %>% round(.,2) 
## Simpson's Index (D), using vegan:
D <- diversity(mite_vg[ ,-(1:6)], index = "simpson") %>% round(.,2)
#### Species Evenness, correction /J' of Pielou (1975): using Vegan
J <- round(H/log(S), 2) 
#### Inverse Simpson from vegan,equivalent to Hill number of order 2 
Di <- diversity(mite_vg[ ,-(1:6)], index = "invsimpson") %>% round(.,2)

# OUTPUT TABLE #
########### #Combine all indices (D, H', E)  with the variables #####
mtDivIndex <- data.frame(mite_vg[,1:6], N, S, H, D, J, Di)
mt_spdiv_index <- datatable(mtDivIndex) %>%
  formatStyle( c('H', 'D', 'J', 'Di'),  background = "palegreen") # formatting table style
mt_spdiv_index

##########################

### PLOT variable Substrate. Mite
mt_subs_plotH <-  ggplot(mtDivIndex,aes(x= Substrate, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

mt_subs_plotD <-  ggplot(mtDivIndex, aes(x= Substrate, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

mt_subs_plotJ <-  ggplot(mtDivIndex,aes(x= Substrate, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))
mt_subs_plotDi <- ggplot(mtDivIndex,aes(x= Substrate, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 12), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

### variable Shrub.mite
mt_shru_plotH <- ggplot(mtDivIndex,aes(x= Shrub, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_shru_plotD <- ggplot(mtDivIndex,aes(x= Shrub, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

mt_shru_plotJ <- ggplot(mtDivIndex,aes(x=Shrub, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_shru_plotDi <- ggplot(mtDivIndex,aes(x=Shrub, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))


## variable Topo.mite: 
mt_topo_plotH <-  ggplot(mtDivIndex,aes(x= Topo, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,3), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

mt_topo_plotD <-  ggplot(mtDivIndex,aes(x= Topo, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

mt_topo_plotJ <-  ggplot(mtDivIndex,aes(x=Topo, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))

mt_topo_plotDi <- ggplot(mtDivIndex,aes(x=Topo, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

###  Numerical variables
# SubsDens. mite
mt_den_plotH <-  ggplot(data = mtDivIndex,aes(x= SubsDens, y = H)) + # changing the Index
  labs(x = "Substrate density (g/L)", y = "Shannon's Index (H)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_den_plotD <-  ggplot(data = mtDivIndex,aes(x= SubsDens, y = D)) + # changing the Index
  labs(x = "Substrate density (g/L)", y = "Simpson's Index (D)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_den_plotJ <-  ggplot(data = mtDivIndex,aes(x= SubsDens, y = J)) + # changing the Index
  labs(x = "Substrate density (g/L)", y = "Pielou Index (J)") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_den_plotDi <- ggplot(data = mtDivIndex,aes(x= SubsDens, y = Di)) + # changing the Index
  labs(x = "Substrate density (g/L)", y = "Inverse Simpson Index (Di)") +
  geom_point(size = 4, shape = 21, fill = "#fd8d3c", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

# WatrCont. mite
mt_wt_plotH <-  ggplot(data = mtDivIndex,aes(x= WatrCont, y = H)) + # changing the Index
  labs(x = "Water content (g/L)", y = "Shannon's Index (H)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_wt_plotD <-  ggplot(data = mtDivIndex,aes(x= WatrCont, y = D)) + # changing the Index
  labs(x = "Water content (g/L)", y = "Simpson's Index (D)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_wt_plotJ <-  ggplot(data = mtDivIndex,aes(x= WatrCont, y = J)) + # changing the Index
  labs(x = "Water content (g/L)", y = "Pielou Index (J)") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

mt_wt_plotDi <- ggplot(data = mtDivIndex,aes(x= WatrCont, y = Di)) + # changing the Index
  labs(x = "Water content (g/L)", y = "Inverse Simpson Index (Di)") +
  geom_point(size = 4, shape = 21, fill = "#fd8d3c", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

###########################
### choice: BCI dataset ##
###########################
# N = abundance; total number of individuals in the sample 
N <-rowSums(BCI_vg[ ,-(1:10)])# sum of individuals sp columns, except the first 10 variables columns
## RICHNESS: # S is equal to Richness (R), counts the number of species recorded in the sample
S <- rowSums(BCI_vg[ ,-(1:10)] > 0)  #sum up the number of non-zero entries per row 
# Div Index: 
## Shannon Index (H), using vegan::
H <- diversity(BCI_vg[ ,-(1:10)], index = "shannon") %>% round(.,2) 
## Simpson's Index (D), using vegan:
D <- diversity(BCI_vg[ ,-(1:10)], index = "simpson") %>% round(.,2)
#### Species Evenness, correction /J' of Pielou (1975): using Vegan
J <- round(H/log(S), 2) 
#### Inverse Simpson from vegan,equivalent to Hill number of order 2 
Di <- diversity(BCI_vg[ ,-(1:10)], index = "invsimpson") %>% round(.,2)

# OUTPUT TABLE #
########### #Combine all indices (D, H', E)  with the variables #####
bcDivIndex <- data.frame(BCI_vg[ ,(1:10)], N, S, H, D, J, Di)
bc_spdiv_index <- datatable(bcDivIndex) %>%
  formatStyle( c('H', 'D', 'J', 'Di'),  background = "palegreen") # formatting table style
bc_spdiv_index


### PLOT variable Age.cat.bci
bc_age_plotH <-  ggplot(bcDivIndex,aes(x= Age.cat, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,5), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_age_plotD <-  ggplot(bcDivIndex, aes(x= Age.cat, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0.6,1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_age_plotJ <-  ggplot(bcDivIndex,aes(x= Age.cat, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_age_plotDi <- ggplot(bcDivIndex,aes(x=Age.cat, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 45), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 


### variable Habitat.bci
bc_habt_plotH <- ggplot(bcDivIndex,aes(x= Habitat, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,5), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

bc_habt_plotD <- ggplot(bcDivIndex,aes(x= Habitat, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))  

bc_habt_plotJ <- ggplot(bcDivIndex,aes(x=Habitat, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1.2), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

bc_habt_plotDi <- ggplot(bcDivIndex,aes(x=Habitat, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 45), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))  

## variable Stream.bci
bc_strm_plotH <-  ggplot(bcDivIndex,aes(x= Stream, y = H)) + # changing the Index
  geom_boxplot(fill = "#ffeda0", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(2,5), name="Shannon's Index (H)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_strm_plotD <-  ggplot(bcDivIndex,aes(x= Stream, y = D)) + # changing the Index
  geom_boxplot(fill = "#a1d99b", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Simpson's Index (D)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))  

bc_strm_plotJ <-  ggplot(bcDivIndex,aes(x=Stream, y = J)) + # changing the Index
  geom_boxplot(fill = "#c6dbef", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1.2), name="Pielou Index (J)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_strm_plotDi <- ggplot(bcDivIndex,aes(x=Stream, y = Di)) + # changing the Index
  geom_boxplot(fill = "#fd8d3c", alpha=0.8) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 45), name="Inverse Simpson Index (Di)") +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12))


# PLot the numerical variables
# No plot  made for Precipitation, Elevation and Geology because there is not variation.  
# UTM.EW. BCI
bc_ew_plotH <- ggplot(data = bcDivIndex, aes(x= UTM.EW, y= H)) + 
  labs(x = "East-West coordinates", y = "Shannon's Index (H)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ew_plotD <- ggplot(data = bcDivIndex, aes(x= UTM.EW, y= D)) + 
  labs(x = "East-West coordinates", y = "Simpson's Index (D)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ew_plotJ <- ggplot(data = bcDivIndex, aes(x= UTM.EW, y= J)) + 
  labs(x = "East-West coordinates", y = "Pielou Index (J)") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ew_plotDi <- ggplot(data = bcDivIndex, aes(x= UTM.EW, y= Di)) + 
  labs(x = "East-West coordinates", y = "Inverse Simpson Index (Di)") +
  geom_point(size = 4, shape = 21, fill = "#fd8d3c", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

# UTM.NS. BCI
bc_ns_plotH <- ggplot(data = bcDivIndex, aes(x= UTM.NS, y= H)) + 
  labs(x = "North-South coordinates", y = "Shannon's Index (H)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ns_plotD <- ggplot(data = bcDivIndex, aes(x= UTM.NS, y= D)) + 
  labs(x = "North-South coordinates", y = "Simpson's Index (D)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ns_plotJ <- ggplot(data = bcDivIndex, aes(x= UTM.NS, y= J)) + 
  labs(x = "North-South coordinates", y = "Pielou Index (J)") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_ns_plotDi <- ggplot(data = bcDivIndex, aes(x= UTM.NS, y= Di)) + 
  labs(x = "North-South coordinates", y = "Inverse Simpson Index (Di)") +
  geom_point(size = 4, shape = 21, fill = "#fd8d3c", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

# EnvHet. BCI
bc_het_plotH <- ggplot(data = bcDivIndex, aes(x= EnvHet, y= H)) + 
  labs(x = "Habitat heterogeneity (Simpson's Index)", y = "Shannon's Index (H)") +
  geom_point(size = 4, shape = 21, fill = "#ffeda0", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_het_plotD <- ggplot(data = bcDivIndex, aes(x= EnvHet, y= D)) + 
  labs(x = "Habitat heterogeneity (Simpson's Index)", y = "Simpson's Index (D)") +
  geom_point(size = 4, shape = 21, fill = "#a1d99b", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_het_plotJ <- ggplot(data = bcDivIndex, aes(x= EnvHet, y= J)) + 
  labs(x = "Habitat heterogeneity (Simpson's Index)", y = "Pielou Index (J)") +
  geom_point(size = 4, shape = 21, fill = "#c6dbef", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

bc_het_plotDi <- ggplot(data = bcDivIndex, aes(x= EnvHet, y= Di)) + 
  labs(x = "Habitat heterogeneity (Simpson's Index)", y = "Inverse Simpson Index (Di)") +
  geom_point(size = 4, shape = 21, fill = "#fd8d3c", alpha = 0.7) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 


#########################################################################################################
# Accumulation Curve
########################################################################################################
# vegan sp. accumulation in vegan uses "exact" method ( Kindt’s exact method) as default
dn_acc <- specaccum(dune_vg[, -(1:6)])
mt_acc <- specaccum(mite_vg[ ,-(1:6)])
bc_acc <- specaccum(BCI_vg[ ,-(1:10)])

# extract the varibles to ggplot the acc curve
dn_acc_dt <- data.frame(dn_acc$sites, dn_acc$richness , dn_acc$sd)
mt_acc_dt <- data.frame(mt_acc$sites, mt_acc$richness , mt_acc$sd)
bc_acc_dt <- data.frame(bc_acc$sites, bc_acc$richness , bc_acc$sd)

# Acc curve dune
plot_acc_dn <- ggplot(data = dn_acc_dt, aes(x = dn_acc.sites, y = dn_acc.richness)) +
  geom_line(colour = "black", size = 1) +
  geom_ribbon(aes(ymin = dn_acc.richness - dn_acc.sd, ymax = dn_acc.richness + dn_acc.sd), 
              fill = "#2b8cbe", alpha = 0.2) + ylim(0, 35) +
  labs(x = "Sample", y = "Number of expected species") +
  theme_bw()
# Acc curve mite
plot_acc_mt <- ggplot(data = mt_acc_dt, aes(x = mt_acc.sites, y = mt_acc.richness)) +
  geom_line(colour = "black", size = 1) +
  geom_ribbon(aes(ymin = mt_acc.richness - mt_acc.sd, ymax = mt_acc.richness + mt_acc.sd), 
              fill = "#2b8cbe", alpha = 0.2) + ylim(0, 40) +
  labs(x = "Sample", y = "Number of expected species") +
  theme_bw()
# Acc curve bci
plot_acc_bc <- ggplot(data = bc_acc_dt, aes(x = bc_acc.sites, y = bc_acc.richness)) +
  geom_line(colour = "black", size = 1) +
  geom_ribbon(aes(ymin = bc_acc.richness - bc_acc.sd, ymax = bc_acc.richness + bc_acc.sd),
              fill = "#2b8cbe", alpha = 0.2) + ylim(0,250) +
  labs(x = "Sample", y = "Number of expected species") + 
  theme_bw()


#########################################################################################################
                                      # RAREFACTION #
#########################################################################################################
# using vegan 
# Function rarefy() gives the expected species richness in random subsamples of size sample from the community. 

## ## Richness Rarefaction: Rarefy() PLot
## the smallest samples into the database


### Richness Rarecurve( ): Make interactive to choose the steps 1 - 200
# Custom color palette (replace col with your desired colors)
col <- c("red", "blue", "green", "purple", "orange", "#525252", "#f768a1")

# Create a list of environmental parameters
env_params <- c(
  "A1", "Moisture", "Management", "Use", "Manure", 
  "SubsDens", "WatrCont", "Substrate", "Shrub", "Topo", 
  "UTM.EW", "UTM.NS", "Age.cat", "Habitat", "Stream", "EnvHet")


