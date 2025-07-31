#i.
# Set the seed for reproducibility
set.seed(123)
# Number of observations
n <- 500

# Generate ai ~ N(n, 0,1)
ai <- rnorm(n, mean =0, sd = 1)

# Generate xi ~ P(n, 4)
xi <- rpois(n, lambda = 4)

# Calculate ei based on the piecewise function
ei <- ifelse(ai > 0, 
              (ai + (2*(ai)^2) - ((1/2) * (ai)^3)), 
              ((2 * ai) + (3 * (ai)^4) - (4/3 * ai)))

#Calculate the response variable yi
yi <- 5 + 6 * xi + ei
# Display the first few values
head(yi)


 
### ii. Function to Estimate the Coefficient of the Exogeneous Variable

#We need to create a function that takes yi and xi as inputs and estimates the coefficient of xi using a linear model.


#Function to estimate the coefficient of the exogeneous variable
estimate_coefficient <- function(y,x) {
  model <- lm(y ~ x)
  return(coef(model)[2])
}

# Example Usage
coef_estimate <- estimate_coefficient(yi, xi)
print(coef_estimate)


### iii. Estimating the Bias of the Coefficient Using 2000 Iterations

# Number of iterations
iterations <- 2000

# Vector to store the estimated coefficients
coef_estimates <- numeric(iterations)


for (i in 1:iterations) {
  ai <- rnorm(n, 0, 1)
  xi <- rpois(n, lambda = 4)
  
  # Calculate ei based on the piecewise function
  ei <- ifelse(ai > 0, 
                (ai + 2) * ((ai)^2) - ((1/2) * (ai)^3), 
              ((2 * ai) + (3 * (ai)^4) - (4/3 * ai)))

  
  # Calculate the response variable yi
  yi <- 5 + 6 * xi + ei
  
  # Estimate the coefficient of the exogeneous variable
  coef_estimates[i] <- estimate_coefficient(yi, xi)
}

# Calculate the bias
true_value <- 6
m<-sum(true_value-coef_estimates)
biass<-m/iterations
biass


