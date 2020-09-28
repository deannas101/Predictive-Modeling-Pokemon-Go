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
  drop_na() %>%
  mutate_at(vars(terrainType), as.character)
colnames(keptPokemon)
numPokemon <- keptPokemon[,c(1, 7:10, 12, 17, 24)]


####Checking Skewness####
skewCheck <- numPokemon[,2:8]
skewCheck <- skewCheck %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    as.data.frame() %>%
    drop_na() #drops observations with missing values to calculate skewness
apply(skewCheck, 2, skewness)

#skewed values: windSpeed, population_density, gymDistanceKm, pokestopDistanceKm

####Histograms####
histogram_temperature <- numPokemon %>%
  ggplot(aes(temperature)) +
  geom_histogram() +
  ylab("Frequency")

histogram_windSpeed <- numPokemon %>%
  ggplot(aes(windSpeed)) +
  geom_histogram() +
  ylab("Frequency")

histogram_windBearing <- numPokemon %>%
  ggplot(aes(windBearing)) +
  geom_histogram() +
  ylab("Frequency")

histogram_pressure <- numPokemon %>%
  ggplot(aes(pressure)) +
  geom_histogram() +
  ylab("Frequency")

histogram_population_density <- numPokemon %>%
  ggplot(aes(population_density)) +
  geom_histogram() +
  ylab("Frequency")

histogram_gymDistanceKm <- numPokemon %>%
  ggplot(aes(gymDistanceKm)) +
  geom_histogram() +
  ylab("Frequency")

histogram_pokestopDistanceKm <- numPokemon %>%
  ggplot(aes(pokestopDistanceKm)) +
  geom_histogram() +
  ylab("Frequency")

grid.arrange(histogram_temperature, histogram_windSpeed, histogram_windBearing, histogram_pressure, histogram_population_density, histogram_gymDistanceKm, histogram_pokestopDistanceKm)

####Dummy Variables####

dmy <- dummyVars("~.", data = keptPokemon)
dummyVar_Pokemon <- data.frame(predict(dmy, newdata = keptPokemon))
View(dummyVar_Pokemon)

####BoxCoxTrans####
numPokemonTrans_windSpeed <- BoxCoxTrans(dummyVar_Pokemon$windSpeed) 

predict(numPokemonTrans_windSpeed, head(dummyVar_Pokemon$temperature))
windSpeedTrans = predict(numPokemonTrans_windSpeed, dummyVar_Pokemon$windSpeed)
skewness(windSpeedTrans)
  #1.522345 (used to be 1.5215)

#histogram
windSpeed_BoxCox <- data.frame(windSpeedTrans)
View(windSpeed_BoxCox)
windSpeed_boxcox <- windSpeed_BoxCox %>% 
  ggplot(aes(windSpeedTrans)) +
  geom_histogram() +
  ylab("Frequency")

numPokemonTrans_populationDensity <- BoxCoxTrans(dummyVar_Pokemon$population_density) 

predict(numPokemonTrans_populationDensity, head(dummyVar_Pokemon$population_density))
populationDensityTrans = predict(numPokemonTrans_populationDensity, dummyVar_Pokemon$population_density)
skewness(populationDensityTrans)
  #2.708712 (used to be 2.7093953)

#histogram
populationDensity_BoxCox <- data.frame(populationDensityTrans)
View(populationDensity_BoxCox)
populationDensity_boxcox <- populationDensity_BoxCox %>% 
  ggplot(aes(populationDensityTrans)) +
  geom_histogram() +
  ylab("Frequency")

numPokemonTrans_gymDistanceKm <- BoxCoxTrans(dummyVar_Pokemon$gymDistanceKm) 

predict(numPokemonTrans_gymDistanceKm, head(dummyVar_Pokemon$gymDistanceKm))
gymDistanceKmTrans = predict(numPokemonTrans_gymDistanceKm, dummyVar_Pokemon$gymDistanceKm)
skewness(gymDistanceKmTrans)
  #1.042624 (used to be 43.9522978)

#histogram
gymDistanceKm_BoxCox <- data.frame(gymDistanceKmTrans)
View(gymDistanceKm_BoxCox)
gymDistanceKm_boxcox <- gymDistanceKm_BoxCox %>% 
  ggplot(aes(gymDistanceKmTrans)) +
  geom_histogram() +
  ylab("Frequency")

numPokemonTrans_pokestopDistanceKm <- BoxCoxTrans(dummyVar_Pokemon$pokestopDistanceKm) 
  #error: missing value where TRUE/FALSE needed?? If you understand this error, feel free to fix it!
predict(numPokemonTrans_pokestopDistanceKm, head(dummyVar_Pokemon$pokestopDistanceKm))
pokestopDistanceKmTrans = predict(numPokemonTrans_pokestopDistanceKm, dummyVar_Pokemon$pokestopDistanceKm)
skewness(pokestopDistanceKmTrans)

#histogram
pokestopDistanceKm_BoxCox <- data.frame(pokestopDistanceKmTrans)
View(gymDistanceKm_BoxCox)
pokestopDistanceKm_boxcox <- pokestopDistanceKm_BoxCox %>% 
  ggplot(aes(pokestopDistanceKmTrans)) +
  geom_histogram() +
  ylab("Frequency")

grid.arrange(windSpeed_boxcox, populationDensity_boxcox, gymDistanceKm_boxcox, pokestopDistanceKm_boxcox)

####Center and Scale####
scaled.centered_Pokemon <- preProcess(numPokemon, method = c("center", "scale"))
scaled.centered_Pokemon <- predict(scaled.centered_Pokemon, numPokemon)
View(scaled.centered_Pokemon)


####Boxplots to Look for Outliers####
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


####Spatial Sign to remove outliers####
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


####PCA####
pca_Pokemon <- preProcess(numPokemon, method = c("scale", "center", "pca")) #default value is C = 95%
pca_Pokemon <- predict(pca_Pokemon,numPokemon)
dim(pca_Pokemon)
dim(numPokemon)
summary(pca_Pokemon)
pca_Pokemon
#we will not use PCA since it did not reduce the number of our numeric variables

####Near Zero Variance####
remove <- nearZeroVar(dummyPokemon)
dummyPokemonUpdate <- dummyPokemon[,-remove] #reduced to 54 columns

####Check Result Variable####
#make a histogram of the pokemonID and figure out if its unbalanced
#I couldn't find any specific code that figures it out from ch3 or 4

####Data Resampling####

#data splitting into training dataset
trainingRows <- createDataPartition(dummyPokemon$pokemonId, p = 0.8, list = FALSE)
training <- dummyPokemon[trainingRows,]


#Using the 10 fold cross validation for resampling the large dataset
#replace preData with the final preprocessed dataset

folds <- createFolds(training$pokemonId, returnTrain = TRUE)
str(folds)

splitUpPokemon <- lapply(folds, function(ind, dat) dat[ind,], dat = training)
