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

#-----------------Lydia's Code---------------------#
    #I added BoxCox trans just to see if any of the skewness was minimized, and it brought down the skewness for 
    # gymDistance quite a bit. There was an error on the box cox trans for the pokestopDistanceKm, so if you understand
    # the error, feel free to fix it!
    # I also did the box plots for the original numeric data, spatial sign transformation, and then the boxplots
    # for after the spatial sign. Let me know if I did anything wrong! I could try to scale and center the data
    # before doing spatial sign, so if you want me to do that, let me know! The only other transformation we could
    # do could be PCA.

#skewed values: windSpeed, population_density, gymDistanceKm, pokestopDistanceKm

#------------------BoxCoxTrans-------------------------#
numPokemonTrans_windSpeed <- BoxCoxTrans(numPokemon$windSpeed) 

predict(numPokemonTrans_windSpeed, head(numPokemon$temperature))
windSpeedTrans = predict(numPokemonTrans_windSpeed, numPokemon$windSpeed)
skewness(windSpeedTrans)
  #1.522345 (used to be 1.5215)

numPokemonTrans_populationDensity <- BoxCoxTrans(numPokemon$population_density) 

predict(numPokemonTrans_populationDensity, head(numPokemon$population_density))
populationDensityTrans = predict(numPokemonTrans_populationDensity, numPokemon$population_density)
skewness(populationDensityTrans)
  #2.708712 (used to be 2.7093953)
  
numPokemonTrans_gymDistanceKm <- BoxCoxTrans(numPokemon$gymDistanceKm) 

predict(numPokemonTrans_gymDistanceKm, head(numPokemon$gymDistanceKm))
gymDistanceKmTrans = predict(numPokemonTrans_gymDistanceKm, numPokemon$gymDistanceKm)
skewness(gymDistanceKmTrans)
  #1.042624 (used to be 43.9522978)

numPokemonTrans_pokestopDistanceKm <- BoxCoxTrans(numPokemon$pokestopDistanceKm) 
  #error: missing value where TRUE/FALSE needed?? If you understand this error, feel free to fix it!
predict(numPokemonTrans_pokestopDistanceKm, head(numPokemon$pokestopDistanceKm))
pokestopDistanceKmTrans = predict(numPokemonTrans_pokestopDistanceKm, numPokemon$pokestopDistanceKm)
skewness(pokestopDistanceKmTrans)


#------------------Boxplots to Look for Outliers----------------#
boxplot(numPokemon)

boxplot_temperature <- numPokemon %>% ggplot() +
  geom_boxplot(aes(temperature)) +
  coord_flip()

boxplot_windSpeed <- numPokemon %>% ggplot() +
  geom_boxplot(aes(windSpeed)) +
  coord_flip()

boxplot_windBearing <- numPokemon %>% ggplot() +
  geom_boxplot(aes(windBearing)) +
  coord_flip()

boxplot_pressure <- numPokemon %>% ggplot() +
  geom_boxplot(aes(pressure)) +
  coord_flip()

boxplot_populationDensity <- numPokemon %>% ggplot() +
  geom_boxplot(aes(population_density)) +
  coord_flip()

boxplot_gymDistanceKm <- numPokemon %>% ggplot() +
  geom_boxplot(aes(gymDistanceKm)) +
  coord_flip()

boxplot_pokestopDistanceKm <- numPokemon %>% ggplot() +
  geom_boxplot(aes(pokestopDistanceKm)) +
  coord_flip()

grid.arrange(boxplot_temeperature, boxplot_windSpeed,boxplot_windBearing, boxplot_pressure, boxplot_populationDensity,boxplot_gymDistanceKm, boxplot_pokestopDistanceKm )


#----------------Spatial Sign to remove outliers---------------------#
spatialSign_numPokemon <- spatialSign(numPokemon)
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

#---------------------Dummy Variables--------------------------#

catPokemon <- keptPokemon %>% select(appearedTimeOfDay,appearedDayOfWeek,closeToWater, weather, weatherIcon, urban, suburban, midurban, rural, gymIn100m, gymIn250m, gymIn500m, gymIn1000m, gymIn2500m, gymIn5000m, pokestopDistanceKm, pokestopIn100m, pokestopIn250m, pokestopIn500m, pokestopIn1000m, pokestopIn2500m, pokestopIn5000m)
View(catPokemon)

catPokemon %>% mutate_at(vars(1:22), dummyVars())

dmy <- dummyVars(~., data = keptPokemon)
dummyVar_Pokemon <- data.frame(predict(dmy, newdata = keptPokemon))
View(dummyVar_Pokemon)

