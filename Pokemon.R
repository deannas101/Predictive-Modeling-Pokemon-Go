library(AppliedPredictiveModeling)
library(caret)
library(e1071)
library(tidyverse)

# PreProcessing the Pokemon Data ----------------------------------------------
# Pokemon is the name of the original dataset
# start with 208 columns

# Selecting Columns and Remove NAs
kept_pokemon <- Pokemon[, c(1, 13, 16, 20, 21, 24:29, 38:56)]
kept_pokemon <- kept_pokemon %>%
  drop_na() %>%
  mutate_at(vars(terrainType), as.character)
colnames(kept_pokemon)
response_pokemon <- kept_pokemon[, 1]
response_pokemon <- as.data.frame(response_pokemon)
dirty_pokemon <- kept_pokemon[, -1]
# columns: 30

# Dummy Variables
dmy <- dummyVars("~.", data = dirty_pokemon)
dummy_pokemon <- data.frame(predict(dmy, newdata = dirty_pokemon))
# columns: 100

# Near Zero Variance
zed <- nearZeroVar(dummy_pokemon)
non_zed_pokemon <- dummy_pokemon[, -zed]
# columns: 57

### not sure if we need boxcox or not, have both options
### currently running without boxocx

# Center, Scale, PCA
scaled_centered <- preProcess(non_zed_pokemon, method = c("center", "scale", "pca"))
scaled_centered_pokemon <- predict(scaled_centered, non_zed_pokemon)
# columns: 31

# Center, Scale, BoxCox, PCA
scaled_centered_transformed <- preProcess(non_zed_pokemon, method = c("center", "scale", "BoxCox", "pca"))
scaled_centered_transformed_pokemon <- predict(scaled_centered_transformed, non_zed_pokemon)
# columns: ?

# Spatial Sign
spatial_sign <- spatialSign(scaled_centered_pokemon)
prepared_pokemon <- data.frame(spatial_sign)
# columns:31

# Data Splitting ---------------------------------------------------------------

# Check Response Balance
ggplot(data = response_pokemon) +
  geom_bar(aes(pokemonId)) +
  labs(x = "Pokemon ID", y = "Frequency", title = "Distribution of Pokemon ID")

# Data Splitting using Stratified Random Sampling
set.seed(1234)

training_rows <- createDataPartition(response_pokemon$pokemonId, p = .80, list = FALSE)

training_predictors <- prepared_pokemon[training_rows, ] # obs: 236786 columns: 31
training_response <- response_pokemon[training_rows, ] # obs: 23676 columns: 1

testing_predictors <- prepared_pokemon[-training_rows, ] # obs: 59196 columns: 31
testing_response <- response_pokemon[-training_rows, ] # obs: 59196 columns: 1

training_response <- as.factor(training_response)
testing_response <- as.factor(testing_response)

# Linear Classification Models -------------------------------------------------

## Modeling## *need to fix variable names - getting an error when running models stating invalid variable name?

# Logistic Regression
ctrl.pokemon <- trainControl(
  method = "LGOCV",
  summaryFunction = defaultSummary,
  classProbs = TRUE,
  savePredictions = TRUE
)
set.seed(1234)
logisticReg.pokemon <- train(trainPredictors.pokemon,
  y = trainResponse.pokemon,
  method = "glm",
  metric = "Kappa",
  trControl = ctrl.pokemon,
  tuneLength = 1,
  trace = FALSE
)
logisticReg.pokemon

confusionMatrix(
  data = logisticReg.pokemon$pred$pred,
  reference = logisticReg.pokemon$pred$obs
)

# Linear Discriminant Analysis
ctrl.pokemon <- trainControl(
  method = "LGOCV",
  summaryFunction = defaultSummary,
  classProbs = TRUE,
  savePredictions = TRUE
)
set.seed(1234)
LDAFull.pokemon <- train(trainPredictors.pokemon,
  y = trainResponse.pokemon,
  method = "lda",
  metric = "Kappa",
  trControl = ctrl.pokemon
)
LDAFull.pokemon

# confusion matrix
confusionMatrix(
  data = LDAFull.pokemon$pred$pred,
  reference = LDAFull.pokemon$pred$obs
)

# Partial Least Squares Discriminant Analysis
ctrl <- trainControl(
  summaryFunction = defaultSummary,
  classProbs = TRUE
)
set.seed(1234)
plsFit.pokemon <- train(
  x = trainPredictors.pokemon,
  y = trainResponse.pokemon,
  method = "pls",
  tuneGrid = expand.grid(.ncomp = 1:20),
  preProc = c("center", "scale"),
  metric = "Kappa",
  trControl = ctrl,
  maxit = 100
)

plsFit.pokemon

plot(plsFit.pokemon, main = "Plot of PLS Discriminant Analysis")

confusionMatrix(
  data = plsFit.pokemon, # might need to change to predicted
  reference = testResponse.pokemon
)

# Penalized Model
ctrl.penalized <- trainControl(
  method = "LGOCV",
  summaryFunction = defaultSummary,
  classProbs = TRUE,
  ## index = list(simulatedTest[,1:4]),
  savePredictions = TRUE
)

glmnGrid <- expand.grid(
  .alpha = c(0, .1, .2, .4, .6, .8, 1),
  .lambda = seq(.01, .2, length = 10)
)
set.seed(123)
glmnTuned.pokemon <- train(
  x = trainPredictors.pokemon,
  y = trainResponse.pokemon,
  method = "glmnet",
  tuneGrid = glmnGrid,
  preProc = c("center", "scale"),
  metric = "Kappa",
  trControl = ctrl.penalized
)
glmnTuned.pokemon

confusionMatrix(
  data = glmnTuned.pokemonc$pred$pred,
  reference = glmnTuned.pokemon$pred$obs
)
