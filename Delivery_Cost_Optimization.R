# Function to calculate the minimum cost of delivery
calculateDeliveryCost <- function(weights, distances) {
  n <- length(weights)  # Number of packages
  m <- length(distances)  # Number of destinations
  
  # Create a memoization table
  memo <- matrix(0, nrow = n, ncol = m)
  
  # Compute and store the minimum cost
  for (i in 1:n) {
    for (j in 1:m) {
      if (i == 1 && j == 1) {
        # Base case: starting point
        memo[i, j] <- weights[i] * distances[j]
      } else if (i == 1) {
        # Base case: first row
        memo[i, j] <- memo[i, j-1] + weights[i] * distances[j]
      } else if (j == 1) {
        # Base case: first column
        memo[i, j] <- memo[i-1, j] + weights[i] * distances[j]
      } else {
        # Recursive case
        memo[i, j] <- min(memo[i-1, j], memo[i, j-1]) + weights[i] * distances[j]
      }
    }
  }
  
  # Return the final minimum cost
  return(memo[n, m])
}

# Test the function
weights <- c(1, 2, 3)  # Package weights
distances <- c(3, 4, 2)  # Distances to destinations
min_cost <- calculateDeliveryCost(weights, distances)
cat("Minimum delivery cost:", min_cost)
