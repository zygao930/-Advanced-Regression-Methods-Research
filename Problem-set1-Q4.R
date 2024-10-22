# Given data
correlation_Y_X1 <- 0.167  # Correlation between # of Cases and Ages
correlation_Y_X2 <- 0.36   # Correlation between # of Cases and Multimorbidity
correlation_X1_X2 <- -0.21 # Correlation between Ages and Multimorbidity
std_dev_Y <- 49.06         # Standard deviation of # of Cases
std_dev_X1 <- 14.30        # Standard deviation of Ages
std_dev_X2 <- 3.46         # Standard deviation of Multimorbidity

# Calculate the partial regression coefficient for Ages
numerator_X1 <- correlation_Y_X1 - correlation_Y_X2 * correlation_X1_X2
denominator <- 1 - correlation_X1_X2^2
partial_regression_X1 <- numerator_X1 / denominator * (std_dev_Y / std_dev_X1)

# Calculate the partial regression coefficient for Multimorbidity
numerator_X2 <- correlation_Y_X2 - correlation_Y_X1 * correlation_X1_X2
partial_regression_X2 <- numerator_X2 / denominator * (std_dev_Y / std_dev_X2)

# Print the results
partial_regression_X1
partial_regression_X2
