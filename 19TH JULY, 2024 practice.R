# Set parameters for the normal distribution
mu <- 0        # Mean of the normal distribution
sigma <- 1     # Standard deviation of the normal distribution
n <- 30        # Sample size
num_sim <- 10000 # Number of simulations

# Function to calculate sample skewness G1
calculate_G1 <- function(sample) {
  n <- length(sample)
  mean_x <- mean(sample)
  
  # Calculate moments
  M3 <- sum((sample - mean_x)^3) / n
  M2 <- sum((sample - mean_x)^2) / n
  
  # Calculate g1
  g1 <- M3 / (M2^(3/2))
  
  # Calculate G1
  G1 <- (sqrt(n * (n - 1)) / (n - 2)) * g1
  
  return(G1)
}

# Initialize a vector to store G1 values
G1_values <- numeric(num_sim)

# Run simulations
set.seed(123) # Set seed for reproducibility
for (i in 1:num_sim) {
  sample <- rnorm(n, mean = mu, sd = sigma)
  G1_values[i] <- calculate_G1(sample)
}

# Display the results
hist(G1_values, breaks = 50, main = "Sampling Distribution of G1",
     xlab = "G1", col = "blue", border = "black")
abline(v = mean(G1_values), col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Mean of G1"), col = c("red"), lwd = 2, lty = 2)

# Print summary statistics
cat("Mean of G1:", mean(G1_values), "\n")
cat("Standard Deviation of G1:", sd(G1_values), "\n")
cat("Skewness of G1:", mean((G1_values - mean(G1_values))^3) / sd(G1_values)^3, "\n")
