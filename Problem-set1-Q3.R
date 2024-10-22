# Create a dataframe with the new data
data <- data.frame(
  Y = c(45.8, 50.0, 49.58, 34.7, 47.0, 45.28, 46.4, 55.2, 52.6, 55.3, 44.5, 44.55),
  X2 = c(9.3, 6.6, 4.3, 27.0, 13.8, 14.1, 10.4, 12.1, 15.4, 10.5, 12.3, 16.3),
  X3 = c(5.1, 5.5, 5.4, 4.1, 5.2, 5.1, 5.2, 5.9, 5.0, 5.9, 5.0, 5.1)
)

# Fit the linear model
model <- lm(Y ~ X2 + X3, data = data)
summary(model)

# Add residuals to the data frame
data$residuals <- residuals(model)

# Print the data with residuals
print(data)

# Plot residuals vs. observed Y values
plot(data$residuals, data$Y,
     main = "Residuals vs. Observed Y Values",
     xlab = "Residuals",
     ylab = "Observed Y",
     pch = 16,          # Use solid circles for points
     col = "blue")      # Color of the points

# Calculate means
Y_mean <- mean(data$Y)
X2_mean <- mean(data$X2)
X3_mean <- mean(data$X3)

# Calculate covariance and variance
cov_X2_Y <- cov(data$X2, data$Y)
cov_X2_Y
cov_X3_Y <- cov(data$X3, data$Y)
cov_X3_Y
cov_X2_X3 <- cov(data$X2, data$X3)
cov_X2_X3
var_X2 <- var(data$X2)
var_X2
var_X3 <- var(data$X3)
var_X3

# Calculate standard deviations
sd_X2 <- sqrt(var_X2)
sd_X3 <- sqrt(var_X3)

# Calculate b3 and b2 using formulas
b3 <- (cov_X3_Y - cov_X2_X3 * (cov_X2_Y / var_X2)) / (var_X3 - (cov_X2_X3^2 / var_X2))
b3
b2 <- (cov_X2_Y - b3 * cov_X2_X3) / var_X2
b2

# Calculate b1 using the means and estimated b2 and b3
b1 <- Y_mean - b2 * X2_mean - b3 * X3_mean
b1


# Print the coefficients
print(paste("b1:", b1))
print(paste("b2:", b2))
print(paste("b3:", b3))

# Refit the model and display summary
model <- lm(Y ~ X2 + X3, data = data)
summary(model)

# Add residuals to the data frame
data$residuals <- residuals(model)
data

sigma_squared <- residuals_sum_of_squares / (N - 3)

N<-12
# Calculate SST (sum of squares total) for X2 and X3
SST_X2 <- sum((data$X2 - mean(data$X2))^2)
SST_X3 <- sum((data$X3 - mean(data$X3))^2)

# Calculate the correlation between X2 and X3
R_X2X3 <- cor(data$X2, data$X3)

# Calculate the variance of b2
var_b2 <- sigma_squared / (SST_X2 * (1 - R_X2X3^2))

# Calculate the variance of b3
var_b3 <- sigma_squared / (SST_X3 * (1 - R_X2X3^2))

var_b2
var_b3

# Plot residuals vs. observed Y values
plot(data$residuals, data$Y,
     main = "Residuals vs. Observed Y Values",
     xlab = "Residuals",
     ylab = "Observed Y",
     pch = 16,          # Use solid circles for points
     col = "blue")      # Color of the points
