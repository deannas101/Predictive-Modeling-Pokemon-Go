library(AppliedPredictiveModeling)
library(caret)
library(e1071)
library(tidyverse)

#PreProcessing the Pokemon Data
#Pokemon is the name of the original dataset

####Selecting Columns####
colnames(Pokemon)
keptPokemon <- Pokemon[,c(1, 13, 16, 20, 21, 24:29, 38:56)]
colnames(keptPokemon)
numPokemon <- keptPokemon[,c(1, 7:10, 12, 17, 24)]

####Checking Skewness####
pokemonId <- numPokemon[,1]
skewCheck <- numPokemon[,2:8]
skewCheck <- skewCheck %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    as.data.frame() %>%
    drop_na() #drops observations with missing values to calculate skewness
apply(skewCheck, 2, skewness)

