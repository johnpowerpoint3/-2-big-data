library(tidyverse)

data <- read_csv("communities+and+crime.csv")

y <- data$ViolentCrimesPerPop  
X <- data %>% select(-ViolentCrimesPerPop)  
X <- as.matrix(cbind(1, X))  


alpha <- 0.01     
iterations <- 3000 
m <- length(y)
n <- ncol(X)
beta <- runif(n, -0.5, 0.5) 
cost_history <- numeric(iterations)




for (iter in 1:iterations) {
  cost <- 0
  
  indices <- sample(1:m)
  for (i in indices) {
    xi <- X[i, , drop = FALSE]
    yi <- y[i]
    y_pred <- sum(xi * beta)
    error <- y_pred - yi
    beta <- beta - alpha * error * xi
    cost <- cost + error^2
  }
  cost_history[iter] <- cost / (2 * m)
}


print(beta)

plot(cost_history, type = "l",
     col = "purple", lwd = 2,
     main = "Learning Rate Plot",
     xlab = "Number of Iterations", ylab = "Cost Function")



