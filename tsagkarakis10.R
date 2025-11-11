
data <- read.csv("10000x42.csv")
set.seed(123)

training_sizes <- c(50, 100, 200, 500)
variable_counts <- c(5, 10, 20, 30, 41)

results <- data.frame()

for (train_size in training_sizes) {
  for (var_count in variable_counts) {
    
    train_indexes <- sample(nrow(data), train_size)
    small_train_data <- data[train_indexes, ]
    
    selected_vars <- c("Stilh1", paste0("Stilh", 2:(var_count + 1)))
    small_train_data <- small_train_data[, selected_vars]
    
    test_indexes <- sample(nrow(small_train_data), ceiling(nrow(small_train_data) * 0.3))
    testData <- small_train_data[test_indexes, ]
    trainData <- small_train_data[-test_indexes, ]
    
    formula <- as.formula(paste("Stilh1 ~", paste(selected_vars[-1], collapse = " + ")))
    
    estimate <- lm(formula, data = trainData)
    
    trainPred <- predict(estimate, trainData)
    testPred <- predict(estimate, testData)
    
    trainingError <- mean((trainData$Stilh1 - trainPred)^2)
    testingError <- mean((testData$Stilh1 - testPred)^2)
        results <- rbind(results, data.frame(
      TrainingSize = nrow(trainData),
      Variables = var_count,
      Ratio_Vars_to_Obs = var_count / nrow(trainData),
      TrainingError = trainingError,
      TestingError = testingError,
      OverfittingRatio = testingError / trainingError,
      R_squared = summary(estimate)$r.squared,
      Adj_R_squared = summary(estimate)$adj.r.squared
    ))
  }
}

print(results)

strong_overfitting <- results[results$OverfittingRatio > 1.5, ]
if (nrow(strong_overfitting) > 0) {
  print("full overfitting")
  print(strong_overfitting[order(-strong_overfitting$OverfittingRatio), ])
} else {
  print("duskolaki na bgei auto to mnm,,,, i mean:  No overfitting")
}
