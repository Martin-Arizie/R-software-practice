
### Horvitz-Thompson Estimator####
##Part A
Question. write a function in R that computes the population total and variance of the Horvitz Thompson estimator



#### R Function to compute the population total and variance of the 
Horvitz-Thompson estimator

horvitz_thompson <- function(y, pi) {
  #### Arguments:
  #### : vector of sample values
  #### pi: vector of inclusion probabilities for each sample
  
  #### Check if the length of y and pi are equal
  if (length(y) != length(pi)) {
    stop("The length of y and pi must be equal")
  }
  
  #### Compute the Horvitz-Thompson estimator for the total
  total_estimate <- sum(y / pi)
  
  #### Number of samples
  n <- length(y)
  
  #### Compute the variance of the Horvitz-Thompson estimator
  var_of_estimate <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        var_of_estimate <- var_of_estimate + (1 - pi[i]) * (1 - pi[j]) * y[i] * y[j] / (pi[i] * pi[j] * (1 - pi[i] * pi[j]))
      }
    }
  }
  return(list(total = total_estimate , variance = var_of_estimate))
}

#### Example 
y <- c(50, 83, 12, 74)  # Sample values
pi <- c(0.5, 0.2, 0.3, 0.1)  # Inclusion probabilities

result <- horvitz_thompson(y, pi)
print(result)


## Part B. Estimartor

ratio_variance <- function(n, dist.type) {
  # Generate data
  data <- switch(dist.type,
    "normal" = rnorm(n, mean = 0, sd = 1),
    "t" = rt(n, df = 4),
    "uniform" = runif(n, min = 4, max = 7))
  
  # Calculate mean and median
  mean_val <- mean(data)
  median_val <- median(data)
  
  # Calculate variance ratio
  ratio <- (var(data) / n) /      ((pi / 2) * (var(data) / n))
  
  return(ratio)
}

# Simulate 1000 populations and count ratios > 1 for each distribution
n_sim <- 1000
counts <- rep(0, 3)  # Initialize count vector for each distribution

for (i in 1:1000) {
  # Loop through each distribution type
  for (j in 1:3) {
    dist_types <- c("normal", "t", "uniform")
    ratio <- ratio_variance(n = 500, dist.type = dist_types[j])
    if (ratio > 1) {
      counts[j] <- counts[j] + 1
    }
  }
}

# Print results
cat("Number of times ratio (mean variance / median variance) > 1:\n")
cat("Normal distribution:", counts[1], "\n")
cat("t-distribution:", counts[2], "\n")
cat("Uniform distribution:", counts[3], "\n")


