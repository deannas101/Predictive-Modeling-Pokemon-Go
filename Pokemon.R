library(AppliedPredictiveModeling)
library(caret)
library(earth)
library(e1071)
library(kernlab)
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

# Center, Scale, PCA
scaled_centered <- preProcess(non_zed_pokemon, method = c("center", "scale", "pca"))
scaled_centered_pokemon <- predict(scaled_centered, non_zed_pokemon)
# columns: 31

# Spatial Sign
spatial_sign <- spatialSign(scaled_centered_pokemon)
prepared_pokemon <- data.frame(spatial_sign)
# columns:31

# Turning pokemon ID into pokemon names ---------------------------------------

response_pokemon[response_pokemon == 1] <- "Bulbasaur"
response_pokemon[response_pokemon == 2] <- "Ivysaur"
response_pokemon[response_pokemon == 3] <- "Venusaur"
response_pokemon[response_pokemon == 4] <- "Charmander"
response_pokemon[response_pokemon == 5] <- "Charmeleon"
response_pokemon[response_pokemon == 6] <- "Charizard"
response_pokemon[response_pokemon == 7] <- "Squirtle"
response_pokemon[response_pokemon == 8] <- "Wartortle"
response_pokemon[response_pokemon == 9] <- "Blastoise"
response_pokemon[response_pokemon == 10] <- "Caterpie"
response_pokemon[response_pokemon == 11] <- "Metapod"
response_pokemon[response_pokemon == 12] <- "Butterfree"
response_pokemon[response_pokemon == 13] <- "Weedle"
response_pokemon[response_pokemon == 14] <- "Kakuna"
response_pokemon[response_pokemon == 15] <- "Beedrill"
response_pokemon[response_pokemon == 16] <- "Pidgey"
response_pokemon[response_pokemon == 17] <- "Pidgeotto"
response_pokemon[response_pokemon == 18] <- "Pidgeot"
response_pokemon[response_pokemon == 19] <- "Rattata"
response_pokemon[response_pokemon == 20] <- "Raticate"
response_pokemon[response_pokemon == 21] <- "Spearow"
response_pokemon[response_pokemon == 22] <- "Fearow"
response_pokemon[response_pokemon == 23] <- "Ekans"
response_pokemon[response_pokemon == 24] <- "Arbok"
response_pokemon[response_pokemon == 25] <- "Pikachu"
response_pokemon[response_pokemon == 26] <- "Raichu"
response_pokemon[response_pokemon == 27] <- "Sandshrew"
response_pokemon[response_pokemon == 28] <- "Sandslash"
response_pokemon[response_pokemon == 29] <- "NidoranF"
response_pokemon[response_pokemon == 30] <- "Nidorina"
response_pokemon[response_pokemon == 31] <- "Nidoqueen"
response_pokemon[response_pokemon == 32] <- "NidoranM"
response_pokemon[response_pokemon == 33] <- "Nidorino"
response_pokemon[response_pokemon == 34] <- "Nidoking"
response_pokemon[response_pokemon == 35] <- "Clefairy"
response_pokemon[response_pokemon == 36] <- "Clefable"
response_pokemon[response_pokemon == 37] <- "Vulpix"
response_pokemon[response_pokemon == 38] <- "Ninetales"
response_pokemon[response_pokemon == 39] <- "Jigglypuff"
response_pokemon[response_pokemon == 40] <- "Wigglytuff"
response_pokemon[response_pokemon == 41] <- "Zubat"
response_pokemon[response_pokemon == 42] <- "Golbat"
response_pokemon[response_pokemon == 43] <- "Oddish"
response_pokemon[response_pokemon == 44] <- "Gloom"
response_pokemon[response_pokemon == 45] <- "Vileplume"
response_pokemon[response_pokemon == 46] <- "Paras"
response_pokemon[response_pokemon == 47] <- "Parasect"
response_pokemon[response_pokemon == 48] <- "Venonat"
response_pokemon[response_pokemon == 49] <- "Venomoth"
response_pokemon[response_pokemon == 50] <- "Diglett"
response_pokemon[response_pokemon == 51] <- "Dugtrio"
response_pokemon[response_pokemon == 52] <- "Meowth"
response_pokemon[response_pokemon == 53] <- "Persian"
response_pokemon[response_pokemon == 54] <- "Psyduck"
response_pokemon[response_pokemon == 55] <- "Golduck"
response_pokemon[response_pokemon == 56] <- "Mankey"
response_pokemon[response_pokemon == 57] <- "Primeape"
response_pokemon[response_pokemon == 58] <- "Growlithe"
response_pokemon[response_pokemon == 59] <- "Arcanine"
response_pokemon[response_pokemon == 60] <- "Poliwag"
response_pokemon[response_pokemon == 61] <- "Poliwhirl"
response_pokemon[response_pokemon == 62] <- "Poliwrath"
response_pokemon[response_pokemon == 63] <- "Abra"
response_pokemon[response_pokemon == 64] <- "Kadabra"
response_pokemon[response_pokemon == 65] <- "Alakazam"
response_pokemon[response_pokemon == 66] <- "Machop"
response_pokemon[response_pokemon == 67] <- "Machoke"
response_pokemon[response_pokemon == 68] <- "Machamp"
response_pokemon[response_pokemon == 69] <- "Bellsprout"
response_pokemon[response_pokemon == 70] <- "Weepinbell"
response_pokemon[response_pokemon == 71] <- "Victreebel"
response_pokemon[response_pokemon == 72] <- "Tentacool"
response_pokemon[response_pokemon == 73] <- "Tantacruel"
response_pokemon[response_pokemon == 74] <- "Geodude"
response_pokemon[response_pokemon == 75] <- "Graveler"
response_pokemon[response_pokemon == 76] <- "Golem"
response_pokemon[response_pokemon == 77] <- "Ponyta"
response_pokemon[response_pokemon == 78] <- "Rapidash"
response_pokemon[response_pokemon == 79] <- "Slowpoke"
response_pokemon[response_pokemon == 80] <- "Slowbro"
response_pokemon[response_pokemon == 81] <- "Magnemite"
response_pokemon[response_pokemon == 82] <- "Magnetron"
response_pokemon[response_pokemon == 83] <- "Farfetch'd"
response_pokemon[response_pokemon == 84] <- "Doduo"
response_pokemon[response_pokemon == 85] <- "Dodio"
response_pokemon[response_pokemon == 86] <- "Seel"
response_pokemon[response_pokemon == 87] <- "Dewgong"
response_pokemon[response_pokemon == 88] <- "Grimer"
response_pokemon[response_pokemon == 89] <- "Muk"
response_pokemon[response_pokemon == 90] <- "Shellder"
response_pokemon[response_pokemon == 91] <- "Cloyster"
response_pokemon[response_pokemon == 92] <- "Gastly"
response_pokemon[response_pokemon == 93] <- "Haunter"
response_pokemon[response_pokemon == 94] <- "Gengar"
response_pokemon[response_pokemon == 95] <- "Onix"
response_pokemon[response_pokemon == 96] <- "Drowzee"
response_pokemon[response_pokemon == 97] <- "Hypno"
response_pokemon[response_pokemon == 98] <- "Krabby"
response_pokemon[response_pokemon == 99] <- "Kingler"
response_pokemon[response_pokemon == 100] <- "Voltorb"
response_pokemon[response_pokemon == 101] <- "Electrode"
response_pokemon[response_pokemon == 102] <- "Exeggcute"
response_pokemon[response_pokemon == 103] <- "Exeggutor"
response_pokemon[response_pokemon == 104] <- "Cubone"
response_pokemon[response_pokemon == 105] <- "Marowak"
response_pokemon[response_pokemon == 106] <- "Hitmonlee"
response_pokemon[response_pokemon == 107] <- "Hitmonchan"
response_pokemon[response_pokemon == 108] <- "Lickitung"
response_pokemon[response_pokemon == 109] <- "Koffing"
response_pokemon[response_pokemon == 110] <- "Weezing"
response_pokemon[response_pokemon == 111] <- "Rhyhorn"
response_pokemon[response_pokemon == 112] <- "Rhydon"
response_pokemon[response_pokemon == 113] <- "Chansey"
response_pokemon[response_pokemon == 114] <- "Tangela"
response_pokemon[response_pokemon == 115] <- "Kangaskhan"
response_pokemon[response_pokemon == 116] <- "Horsea"
response_pokemon[response_pokemon == 117] <- "Seadra"
response_pokemon[response_pokemon == 118] <- "Goldeen"
response_pokemon[response_pokemon == 119] <- "Seaking"
response_pokemon[response_pokemon == 120] <- "Staryu"
response_pokemon[response_pokemon == 121] <- "Starmie"
response_pokemon[response_pokemon == 122] <- "MrMime"
response_pokemon[response_pokemon == 123] <- "Scyther"
response_pokemon[response_pokemon == 124] <- "Jynx"
response_pokemon[response_pokemon == 125] <- "Electabuzz"
response_pokemon[response_pokemon == 126] <- "Magmar"
response_pokemon[response_pokemon == 127] <- "Pinsir"
response_pokemon[response_pokemon == 128] <- "Tauros"
response_pokemon[response_pokemon == 129] <- "Magikarp"
response_pokemon[response_pokemon == 130] <- "Gyarados"
response_pokemon[response_pokemon == 131] <- "Lapras"
response_pokemon[response_pokemon == 132] <- "Ditto"
response_pokemon[response_pokemon == 133] <- "Eevee"
response_pokemon[response_pokemon == 134] <- "Vaporeon"
response_pokemon[response_pokemon == 135] <- "Jolteon"
response_pokemon[response_pokemon == 136] <- "Flareon"
response_pokemon[response_pokemon == 137] <- "Porygon"
response_pokemon[response_pokemon == 138] <- "Omanyte"
response_pokemon[response_pokemon == 139] <- "Omastar"
response_pokemon[response_pokemon == 140] <- "Kabuto"
response_pokemon[response_pokemon == 141] <- "Kabutops"
response_pokemon[response_pokemon == 142] <- "Aerodactyl"
response_pokemon[response_pokemon == 143] <- "Snorlax"
response_pokemon[response_pokemon == 144] <- "Articuno"
response_pokemon[response_pokemon == 145] <- "Zapdos"
response_pokemon[response_pokemon == 146] <- "Moltres"
response_pokemon[response_pokemon == 147] <- "Dratini"
response_pokemon[response_pokemon == 148] <- "Dragonair"
response_pokemon[response_pokemon == 149] <- "Dragonite"
response_pokemon[response_pokemon == 150] <- "Mewtwo"
response_pokemon[response_pokemon == 151] <- "Mew"

# Data Splitting ---------------------------------------------------------------

# Check Response Balance
ggplot(data = response_pokemon) +
  geom_bar(aes(pokemonId)) +
  labs(x = "Pokemon ID", y = "Frequency", title = "Distribution of Pokemon ID")

# Data Splitting using Stratified Random Sampling
set.seed(1234)

subset_rows <- createDataPartition(response_pokemon$pokemonId, p = .25, list = FALSE)
response_subset <- response_pokemon[subset_rows, ]
pokemon_subset <- prepared_pokemon[subset_rows, ]

response_subset <- as.data.frame(response_subset)
colnames(response_subset) <- "pokemonId"

training_rows <- createDataPartition(response_subset$pokemonId, p = .80, list = FALSE)

training_predictors <- pokemon_subset[training_rows, ] # obs: 236786 columns: 31
training_response <- response_subset[training_rows, ] # obs: 23676 columns: 1

testing_predictors <- pokemon_subset[-training_rows, ] # obs: 59196 columns: 31
testing_response <- response_subset[-training_rows, ] # obs: 59196 columns: 1

training_response <- as.factor(training_response)
testing_response <- as.factor(testing_response)

#removing observations that only occur once
training_response_single_observations <- training_response_d %>%
  group_by(training_response) %>%
  tally() %>%
  filter(n == 1) %>%
  select(-n)

c <- as.character(training_response_single_observations$training_response)

training_response_no_single_obersvations <-  training_response_d %>%
  filter(response_pokemon != c)

reduced_response <- training_response %>%
  filter(training_response != c)
reduced_predictors <- training_predictors %>%
  filter(training_predictors != c)

# Linear Classification Models -------------------------------------------------

# Logistic Regression WORKS ON A TEENY SAMPLE SO IT'LL TAKE TIME
ctrl <- trainControl(
  method = "LGOCV",
  summaryFunction = defaultSummary,
  savePredictions = TRUE
)

set.seed(1234)
logistic_regression <- train(training_predictors,
  y = training_response,
  method = "multinom",
  metric = "Kappa",
  trControl = ctrl,
  maxit = 5,
  MaxNWts = 4896
)

logistic_regression

plot(logistic_regression, main = "Plot of Logistic Regression")

confusionMatrix(
  data = logistic_regression$pred$pred,
  reference = logistic_regression$pred$obs
)

# Linear Discriminant Analysis
set.seed(1234)
lda_model <- train(training_predictors,
  y = training_response,
  method = "lda",
  metric = "Kappa",
  trControl = ctrl
)

lda_model

plot(lda_model, main = "Plot of Linear Discriminant Analysis")

confusionMatrix(
  data = lda_model$pred$pred,
  reference = lda_model$pred$obs
)

# Partial Least Squares Discriminant Analysis WORKS ON A TEENY SAMPLE SO IT'LL TAKE TIME
ctrl <- trainControl(
  summaryFunction = defaultSummary,
  savePredictions = TRUE
)

set.seed(1234)
pls_model <- train(
  x = training_predictors,
  y = training_response,
  method = "pls",
  tuneGrid = expand.grid(.ncomp = 1:20),
  metric = "Kappa",
  trControl = ctrl,
  maxit = 10000
)

pls_model

plot(pls_model, main = "Plot of PLS Discriminant Analysis")

confusionMatrix(
  data = pls_model$pred$pred,
  reference = pls_model$pred$obs
)

# Penalized Model ERROR: NO CLASS CAN HAVE 1 OR 0 OBSERVATIONS
ctrl <- trainControl(
  method = "LGOCV",
  summaryFunction = defaultSummary,
  savePredictions = TRUE
)

glmnGrid <- expand.grid(
  .alpha = c(0, .1, .2, .4, .6, .8, 1),
  .lambda = seq(.01, .2, length = 10)
)

set.seed(123)
penalized_model <- train(
  x = training_predictors,
  y = training_response,
  method = "glmnet",
  tuneGrid = glmnGrid,
  metric = "Kappa",
  trControl = ctrl
)

penalized_model

plot(penalized_model, main = "Plot of Penalized Model")

confusionMatrix(
  data = penalized_model$pred$pred,
  reference = penalized_model$pred$obs
)

# Non-Linear Classification Models --------------------------------------------

# Quadratic Regularized Discriminant Analysis NEEDS BIGGER THAN 0.1 SUBSET
ctrl_nonLinear_models <- trainControl(
  method = "LGOCV",
  number = 10,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = defaultSummary
)
set.seed(123)
qda_model <- train(
  x = training_predictors,
  y = training_response,
  method = "qda",
  metric = "Kappa",
  trControl = ctrl_nonLinear_models
)

qda_model

plot(qda_model, main = "Plot of Quadratic Regularized Discriminant Analysis")

confusionMatrix(
  data = qda_model$pred$pred,
  reference = qda_model$pred$obs
)

# Regularized Discriminant Analysis RETURNING ONLY ZEROS FOR ACCURACY AND KAPPA
set.seed(123)
rda_model <- train(
  x = training_predictors,
  y = training_response,
  method = "rda",
  metric = "Kappa",
  tuneGrid = expand.grid(.lambda = 1:3, .gamma = 1:3),
  trControl = ctrl_nonLinear_models
)

rda_model

plot(rda_model, main = "Plot of Regularized Discriminant Analysis")

confusionMatrix(
  data = rda_model$pred$pred,
  reference = rda_model$pred$obs
)

# Mixture Discriminant Analysis
set.seed(123)
mda_model <- train(
  x = training_predictors,
  y = training_response,
  method = "mda",
  metric = "Kappa",
  tuneGrid = expand.grid(.subclasses = 1:3),
  trControl = ctrl_nonLinear_models
)

mda_model

plot(mda_model, main = "Plot of Mixture Discriminant Analysis")

confusionMatrix(
  data = mda_model$pred$pred,
  reference = mda_model$pred$obs
)

# Neural Networks
ctrl_nonLinear_models <- trainControl(
  method = "LGOCV",
  number = 10,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = defaultSummary
)
nnetGrid <- expand.grid(.size = 1:10, .decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$.size)
numWts <- (maxSize * (31 + 1) + (maxSize + 1) * 144) ## 31 is the number of predictors, 144 classes (pokemon IDs)

nnet_model <- train(
  x = training_predictors,
  y = training_response,
  method = "nnet",
  metric = "Kappa",
  tuneGrid = nnetGrid,
  trace = FALSE,
  maxit = 100,
  MaxNWts = numWts,
  trControl = ctrl_nonLinear_models
)

nnet_model

plot(nnet_model, main = "Plot of Neural Networks")

confusionMatrix(
  data = nnet_model$pred$pred,
  reference = nnet_model$pred$obs
)


# Flexible Discriminant Analysis
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:35)

set.seed(1234)
fda_model <- train(
  x = training_predictors,
  y = training_response,
  method = "fda",
  tuneGrid = marsGrid,
  trControl = ctrl_nonLinear_models
)

fda_model

plot(fda_model, main = "Plot of Flexible Discriminant Analysis")

confusionMatrix(
  data = fda_model$pred$pred,
  reference = fda_model$pred$obs
)

# Support Vector Machines
sigmaRangeReduced <- sigest(as.matrix(training_predictors))
svmRGridReduced <- expand.grid(.sigma = sigmaRangeReduced[1],
                               .C = 2^(seq(-4, 6)))
set.seed(123)
svm_model <- train(x = training_predictors, 
                   y = training_response,
                   method = "svmRadial",
                   metric = "Kappa",
                   tuneGrid = svmRGridReduced,
                   fit = FALSE,
                   trainControl = ctrl_nonLinear_models
)

svm_model

plot(svm_model, main = "Plot of Support Vector Machine")

confusionMatrix(
  data = svm_model$pred$pred,
  reference = svm_model$pred$obs
)

# K-Nearest Neighbors
set.seed(123)
knn_model <- train(x = training_predictors, 
                y = training_response,
                method = "knn",
                metric = "Kappa",
                ##tuneGrid = data.frame(.k = c(4*(0:5)+1, 20*(1:5)+1, 50*(2:9)+1)), ## 21 is the best
                tuneGrid = data.frame(.k = 1:50),
                trControl = ctrl_nonLinear_models
)

knn_model

plot(knn_model, main = "Plot of K-Nearest Neighbors")

confusionMatrix(
  data = knn_model$pred$pred,
  reference = knn_model$pred$obs
)

# Naive Bayes NEED TO REMOVE SINGLE OBSERVATIONS
set.seed(123)
nb_model <- train( x = training_predictors, 
                y = training_response,
                method = "nb",
                metric = "Kappa",
                tuneGrid = data.frame(.fL = 2,.usekernel = TRUE,.adjust = TRUE),
                trControl = ctrl_nonLinear_models
)

nb_model

plot(nb_model, main = "Plot of Naive Bayes")

confusionMatrix(
  data = nb_model$pred$pred,
  reference = nb_model$pred$obs
)
