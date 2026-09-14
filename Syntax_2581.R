library(readxl)
library(ggplot2)
library(igraph)

# Read the input data
demand <- read_excel('Dataset.xlsx', sheet = 'Permintaan')
distance <- as.matrix(read_excel('Dataset.xlsx', sheet = 'Jarak')[,-1])
delivery_time <- as.matrix(read_excel('Dataset.xlsx', sheet = 'Waktu')[,-1])
capacity <- read_excel('Dataset.xlsx', sheet = 'Kapasitas')
cost <- read_excel('Dataset.xlsx', sheet = 'Cost')

# Define the dynamic programming function
dynamic_programming <- function(demand, distance, delivery_time, capacity, cost) {
  n <- length(demand)
  m <- length(cost)
  f <- array(0, dim = c(n, m))
  g <- array(0, dim = c(n, m))
  for (j in 1:m) {
    f[1, j] <- cost[[j]][1] * distance[1, 6]
    g[1, j] <- 6
  }
  cost_list <- list(f[1, ])
  for (i in 2:n) {
    for (j in 1:m) {
      f[i, j] <- Inf
      for (k in 1:m) {
        if (capacity[k] >= demand[i]) {
          temp <- f[i - 1, k] + cost[[j]][k] * distance[k, i] + cost[[j]][1] * distance[i, 6]
          if (temp < f[i, j]) {
            f[i, j] <- temp
            g[i, j] <- k
          }
        }
      }
    }
    cost_list <- c(cost_list, list(f[i, ]))
  }
  path <- c(6)
  j <- which.min(f[n, ])
  for (i in n:2) {
    path <- c(g[i, j], path)
    j <- g[i, j]
  }
  path <- c(1, path)
  return(list(cost = f[n, j], path = path, cost_list = cost_list))
}

# Call the dynamic programming function
result <- dynamic_programming(demand, distance, delivery_time, capacity, cost)

# Print the result
cat("The optimal cost is", result$cost, "\n")
cat("The optimal path is", paste(result$path, collapse = " -> "), "\n")

# Extract the cost list from the result
cost_list <- unlist(result$cost_list)

# Generate the plot for the cost optimization process
iterations <- seq_along(cost_list) - 1
df <- data.frame(iterations = iterations, costs = cost_list)

# Plot the cost optimization process
ggplot(data = df, aes(x = iterations, y = costs)) +
  geom_line() +
  labs(x = "Iterations", y = "Cost", title = "Cost Optimization Process") +
  theme_minimal()

# Plot the optimal path
optimal_path <- result$path

# Plot the graph with the optimal path highlighted
plot(optimal_path,xlab = "Pengantaran",ylab = "Port Index")
lines(optimal_path)
