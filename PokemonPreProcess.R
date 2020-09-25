library(AppliedPredictiveModeling)
library(caret)
library(e1071)
library(tidyverse)

#PreProcessing the Pokemon Data
#Pokemon is the name of the original dataset

####Selecting Columns####
colnames(Pokemon)
keptPokemon <- Pokemon[,c(1, 13, 16, 20, 21, 24:29, 38:56)]
keptPokemon <- keptPokemon %>%
  drop_na()
colnames(keptPokemon)
numPokemon <- keptPokemon[,c(1, 7:10, 12, 17, 24)]

####Checking Skewness####
skewCheck <- dummyVar_Pokemon[,2:8]
skewCheck <- skewCheck %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    as.data.frame() %>%
    drop_na() #drops observations with missing values to calculate skewness
apply(skewCheck, 2, skewness)

#skewed values: windSpeed, population_density, gymDistanceKm, pokestopDistanceKm

#---------------------Dummy Variables--------------------------#

dmy <- dummyVars("~.", data = keptPokemon)
dummyVar_Pokemon <- data.frame(predict(dmy, newdata = keptPokemon))
View(dummyVar_Pokemon)

#------------------BoxCoxTrans-------------------------#
numPokemonTrans_windSpeed <- BoxCoxTrans(dummyVar_Pokemon$windSpeed) 

predict(numPokemonTrans_windSpeed, head(dummyVar_Pokemon$temperature))
windSpeedTrans = predict(numPokemonTrans_windSpeed, dummyVar_Pokemon$windSpeed)
skewness(windSpeedTrans)
  #1.522345 (used to be 1.5215)

numPokemonTrans_populationDensity <- BoxCoxTrans(dummyVar_Pokemon$population_density) 

predict(numPokemonTrans_populationDensity, head(dummyVar_Pokemon$population_density))
populationDensityTrans = predict(numPokemonTrans_populationDensity, dummyVar_Pokemon$population_density)
skewness(populationDensityTrans)
  #2.708712 (used to be 2.7093953)
  
numPokemonTrans_gymDistanceKm <- BoxCoxTrans(dummyVar_Pokemon$gymDistanceKm) 

predict(numPokemonTrans_gymDistanceKm, head(dummyVar_Pokemon$gymDistanceKm))
gymDistanceKmTrans = predict(numPokemonTrans_gymDistanceKm, dummyVar_Pokemon$gymDistanceKm)
skewness(gymDistanceKmTrans)
  #1.042624 (used to be 43.9522978)

numPokemonTrans_pokestopDistanceKm <- BoxCoxTrans(dummyVar_Pokemon$pokestopDistanceKm) 
  #error: missing value where TRUE/FALSE needed?? If you understand this error, feel free to fix it!
predict(numPokemonTrans_pokestopDistanceKm, head(dummyVar_Pokemon$pokestopDistanceKm))
pokestopDistanceKmTrans = predict(numPokemonTrans_pokestopDistanceKm, dummyVar_Pokemon$pokestopDistanceKm)
skewness(pokestopDistanceKmTrans)


#------------------Boxplots to Look for Outliers----------------#
boxplot(dummyVar_Pokemon)

boxplot_temperature <- dummyVar_Pokemon %>% ggplot() +
  geom_boxplot(aes(temperature)) +
  coord_flip()

boxplot_windSpeed <- dummyVar_Pokemon %>% ggplot() +
  geom_boxplot(aes(windSpeed)) +
  coord_flip()

boxplot_windBearing <- dummyVar_Pokemon %>% ggplot() +
  geom_boxplot(aes(windBearing)) +
  coord_flip()

boxplot_pressure <- dummyVar_Pokemon %>% ggplot() +
  geom_boxplot(aes(pressure)) +
  coord_flip()

boxplot_populationDensity <- dummyVar_Pokemon %>% ggplot() +
  geom_boxplot(aes(population_density)) +
  coord_flip()

boxplot_gymDistanceKm <- dummyVar_Pokemon %>% ggplot() +
  geom_boxplot(aes(gymDistanceKm)) +
  coord_flip()

boxplot_pokestopDistanceKm <- dummyVar_Pokemon %>% ggplot() +
  geom_boxplot(aes(pokestopDistanceKm)) +
  coord_flip()

grid.arrange(boxplot_temeperature, boxplot_windSpeed,boxplot_windBearing, boxplot_pressure, boxplot_populationDensity,boxplot_gymDistanceKm, boxplot_pokestopDistanceKm )


#----------------Spatial Sign to remove outliers---------------------#
spatialSign_numPokemon <- spatialSign(dummyVar_Pokemon)
spatialSign_numPokemon <- data.frame(spatialSign_numPokemon)

#boxplots to see if outliers were removed
boxplotSS_temperature <- spatialSign_numPokemon %>% 
  ggplot() +
  geom_boxplot(aes(temperature)) +
  coord_flip()

boxplotSS_windSpeed <- spatialSign_numPokemon %>% 
  ggplot() +
  geom_boxplot(aes(windSpeed)) +
  coord_flip()

boxplotSS_windBearing <- spatialSign_numPokemon %>% 
  ggplot() +
  geom_boxplot(aes(windBearing)) +
  coord_flip()

boxplotSS_pressure <- spatialSign_numPokemon %>% 
  ggplot() +
  geom_boxplot(aes(pressure)) +
  coord_flip()

boxplotSS_populationDensity <- spatialSign_numPokemon %>% 
  ggplot() +
  geom_boxplot(aes(population_density)) +
  coord_flip()

boxplotSS_gymDistanceKm <- spatialSign_numPokemon %>% 
  ggplot() +
  geom_boxplot(aes(gymDistanceKm)) +
  coord_flip()

boxplotSS_pokestopDistanceKm <- spatialSign_numPokemon %>% 
  ggplot() +
  geom_boxplot(aes(pokestopDistanceKm)) +
  coord_flip()

grid.arrange(boxplotSS_temperature, boxplotSS_windSpeed,boxplotSS_windBearing, boxplotSS_pressure, boxplotSS_populationDensity,boxplotSS_gymDistanceKm, boxplotSS_pokestopDistanceKm )

#outliers were minimized from pressure, population_density, and gymDistanceKm
#we still have a lot of outliers for windspeed, gymDistanceKm and pokestopDistanceKm