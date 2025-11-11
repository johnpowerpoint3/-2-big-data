library(tidyverse)


data <- read_csv("HouseholdData.csv")

y <- data$FoodExpenditure

X <- data %>% 
  select(Income, FamilySize)


X_scaled <- scale(X)  
m <- nrow(X_scaled)

X_matrix <- cbind(1, X_scaled)  
y_matrix <- matrix(y, ncol=1)


calculateCost <- function(X, y, theta){
  m <- length(y)
  J <- sum((X %*% theta - y)^2) / (2*m)
  return(J)
}


gradientDescent <- function(X, y, theta, alpha=0.01, numIters=1000){
  m <- length(y)
  costHistory <- numeric(numIters)
  
  for(i in 1:numIters){
    theta <- theta - alpha * (1/m) * (t(X) %*% (X %*% theta - y))
    costHistory[i] <- calculateCost(X, y, theta)
  }
  
  return(list("coefficients" = theta, "costs" = costHistory))
}


theta_init <- matrix(0, nrow=ncol(X_matrix), ncol=1)
alpha <- 0.01
numIterations <- 1500

gd_result <- gradientDescent(X_matrix, y_matrix, theta_init, alpha, numIterations)

plot(gd_result$costs, type='l', col='blue', lwd=2,
     xlab='Iteration', ylab='Cost J(θ)', main='agkonas plot')

gd_coeff <- gd_result$coefficients
print("suntelestes Gradient Descent:")
print(gd_coeff)


model_ols <- lm(FoodExpenditure ~ Income + FamilySize, data = data)
ols_coeff <- coef(model_ols)
print("suntelestes OLS:")
print(ols_coeff)


mean_X <- colMeans(X)
sd_X <- apply(X, 2, sd)

theta0 <- gd_coeff[1] - sum((gd_coeff[-1] * mean_X / sd_X))
theta1 <- gd_coeff[2] / sd_X[1]
theta2 <- gd_coeff[3] / sd_X[2]

cat("\nsuntelestes GD:\n")
cat("Intercept:", theta0, "Income:", theta1, "FamilySize:", theta2, "\n")

cat("sugkrish:\n")
cat("OLS Intercept:", ols_coeff[1], "GD Intercept:", theta0, "\n")
cat("OLS Income:", ols_coeff[2], "GD Income:", theta1, "\n")
cat("OLS FamilySize:", ols_coeff[3], "GD FamilySize:", theta2, "\n")
