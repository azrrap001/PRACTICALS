# Set seed for reproducibility  
set.seed(1)  

# 1. Generate Simulated Data  
# Create x as a sequence of numbers from 1 to 100  
x <- 1:100  

# Generate a noisy sine wave for y  
e <- rnorm(100, mean = 0, sd = 0.2) # noise  
y <- sin(x / 10) + e  

# Custom LOWESS Algorithm  
customLowess <- function(x, y, f) {  
  n <- length(x)  
  k <- floor(f * n) # number of nearest neighbors  
  
  smoothed_y <- numeric(n) # to hold the smoothed values  
  
  for (i in 1:n) {  
    # Compute distances and weights  
    distances <- abs(x - x[i])  
    idx <- order(distances)[1:k] # closest k neighbors  
    dmax <- max(distances[idx])  
    weights <- (1 - (distances[idx] / dmax)^3)^3  
    weights[which(distances[idx] > dmax)] <- 0  
    
    # Weighted least squares  
    W <- diag(weights) # diagonal weight matrix  
    X <- cbind(1, x[idx]) # design matrix with intercept  
    beta_hat <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% y[idx])  
    
    smoothed_y[i] <- beta_hat[1] + beta_hat[2] * x[i] # predicted value  
  }  
  
  return(smoothed_y) # return smoothed values  
}  

# 2. Compare with R's built-in lowess()  
f_value <- 0.2  
smoothed_custom <- customLowess(x, y, f_value)  
smoothed_builtin <- lowess(x, y, f = f_value, iter = 0)$y  

# 3. Plotting the results  
plot(x, y, main = "LOWESS Smoothing Comparison", xlab = "x", ylab = "y", pch = 19, col = rgb(0, 0, 0, 0.5))  
lines(x, smoothed_custom, col = "blue", lwd = 2, type = "l", lty = 1)  
lines(x, smoothed_builtin, col = "red", lwd = 2, type = "l", lty = 2)  

legend("topright", legend = c("Custom LOWESS", "Built-in LOWESS"), col = c("blue", "red"), lty = 1:2, bty = "n")