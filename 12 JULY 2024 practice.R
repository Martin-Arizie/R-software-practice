
#Question 2i

generate_populations <- function(x,y,z) {
  
  # Generate populations of size 500 from each distribution
  pop_normal <- rnorm(500, mean = 0, sd = 1)
  pop_t <- rt(500, df = 4)
  pop_uniform <- runif(500, min = 4, max = 7)
  
  # Calculate the mean and median for each population
  mean_normal <- mean(pop_normal)
  median_normal <- median(pop_normal)
  
  mean_t <- mean(pop_t)
  median_t <- median(pop_t)
  
  mean_uniform <- mean(pop_uniform)
  median_uniform <- median(pop_uniform)

 #Variance for the mean is given by S^2 / n
  # Variance for the median is given by (π / 2) * S^2 / n
  
  var_mean_normal <- var(pop_normal) / 500
  var_median_normal <- (pi / 2) * var(pop_normal) / 500
  
  var_mean_t <- var(pop_t) / 500
  var_median_t <- (pi / 2) * var(pop_t) / 500
  
  var_mean_uniform <- var(pop_uniform) / 500
  var_median_uniform <- (pi / 2) * var(pop_uniform) / 500
  
  # Calculate the ratio of variances
  ratio_normal <- var_mean_normal / var_median_normal
  ratio_t <- var_mean_t / var_median_t
  ratio_uniform <- var_mean_uniform / var_median_uniform
  
  return(c(ratio_normal, ratio_t, ratio_uniform))
}

generate_populations(ratio_normal, ratio_t, ratio_uniform)


#Question 2ii
count_ratios_greater_than_one <- function() {
  ratios <- replicate(1000, generate_populations())
  
  # Check how many times the ratio is greater than one for each distribution
  count_normal <- sum(ratios[1, ] > 1)
  count_t <- sum(ratios[2, ] > 1)
  count_uniform <- sum(ratios[3, ] > 1)
  
  return(c(count_normal, count_t, count_uniform))
}

# Call the function and print the results
result <- count_ratios_greater_than_one()
names(result) <- c("Normal Distribution", "t Distribution", "Uniform Distribution")
print(result)

count_ratios_greater_than_one(c(count_normal, count_t, count_uniform))





