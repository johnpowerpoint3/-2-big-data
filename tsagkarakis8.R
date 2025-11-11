library(tidyverse)


data <- read_csv("forestfires.csv")



glimpse(data)

model <- lm(area ~ temp + wind + rain, data)




calculateRMSE <- function(predictedValues, actualValues) {
  sqrt(mean((actualValues - predictedValues)^2))
}


kFoldCrossValidation <- function(data, formula, k = 10) {
  dataset <- data[sample(nrow(data)), ]
  
  folds <- cut(seq(1, nrow(dataset)), breaks = k, labels = FALSE)
  
  RMSE <- c()
  
  for (i in 1:k) {
    testIndexes <- which(folds == i, arr.ind = TRUE)
    testData <- dataset[testIndexes, ]
    trainData <- dataset[-testIndexes, ]
    
    model <- lm(formula, data = trainData)
    
    predicted <- predict(model, testData)
    
    error <- calculateRMSE(predicted, testData$area)
    RMSE <- c(RMSE, error)
  }
  
  return(mean(RMSE))
}

model_formula <- as.formula("area ~ temp + wind + rain")

mean_rmse <- kFoldCrossValidation(data, model_formula, k = 10)

mean_rmse 
summary(model)


small_fires <- data %>% filter(area < 3.2)

mean_rmse_small <- kFoldCrossValidation(small_fires, model_formula, k = 10)

mean_rmse_small

small_model <- lm(model_formula, data = small_fires)
summary(small_model)

