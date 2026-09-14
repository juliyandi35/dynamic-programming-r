fibonacci <- function(n) {
  if (n <= 1) {
    return(n)
  }

  dp <- numeric(n+1)
  dp[1] <- 0
  dp[2] <- 1

  for (i in 3:(n+1)) {
    dp[i] <- dp[i-1] + dp[i-2]
  }

  return(dp[n+1])
}

# Example usage
n <- 15
result <- fibonacci(n)
print(paste("Fibonacci of", n, ":", result))
