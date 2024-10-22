###########################################
##############  QUESTION 1  ###############
###########################################

calculate_beta_hat <- function(t, x) {
  c <- cbind(1, x)
  c_t <- t(c)
  beta_hat <- solve(c_t %*% c) %*% c_t %*% t
  return(beta_hat)
}

calculate_residual <- function(t, x, beta_hat) {
  c <- cbind(1, x)
  y_hat <- c %*% beta_hat
  residuals <- t - y_hat
  return(residuals)
}

calculate_SSR <- function(residuals) {
  SSR <- sum(residuals^2)
  return(SSR)
}

calculate_SEE <- function(SSR, df) {
  SEE <- sqrt(SSR / df)
  return(SEE)
}

calculate_var_b <- function(SSR, x) {
  c <- cbind(1, x)
  c_t <- t(c)
  var_b <- SSR * solve(c_t %*% c)
  return(var_b)
}

calculate_SS_tot <- function(t) {
  t_mean <- mean(t, na.rm = TRUE)
  y_e <- (t - t_mean)^2
  SST <- sum(y_e, na.rm = TRUE)
  return(SST)
}

calculate_r2 <- function(SSR, SS_tot) {
  r2 <- 1 - SSR / SS_tot
  return(r2)
}

calculate_adjusted_r2 <- function(r2, n, k) {
  adjusted_r2 <- 1 - ((1 - r2) * (n - 1) / (n - k - 1))
  return(adjusted_r2)
}

calculate_t_beta <- function(beta_hat, var_b) {
  t_beta <- beta_hat / sqrt(diag(var_b))
  return(t_beta)
}


#x <- matrix(c(9.3, 6.6, 4.3, 27.0, 13.8, 14.1, 10.4, 12.1, 15.4, 10.5, 12.3, 16.3, 
#              5.1, 5.5, 5.4, 4.1, 5.2, 5.1, 5.2, 5.9, 5.0, 5.9, 5.0, 5.1),  
#            nrow = 12, byrow = FALSE)

#t <- matrix(c(45.8, 50.0, 49.58, 34.7, 47.0, 45.28, 46.4, 55.2, 52.6, 55.3, 44.5, 44.55), nrow = 12, byrow = FALSE)

setwd('F:/ICPSRworkshop/02-Regression Analysis II Linear Models/Assignments/Problem Set 3/reg2_hw3_data')
dataset <- read.csv("KanzlerDaten.csv")
data <- na.omit(dataset) 
head(data)

t <- as.matrix(data['Y1'])
x <- as.matrix(data[, c('X2', 'X3', 'X4')])
x 

SS_tot <- calculate_SS_tot(t)
df <- nrow(t) - ncol(x) - 1  
beta_hat <- calculate_beta_hat(t, x)
residuals <- calculate_residual(t,x,beta_hat)
SSR <- calculate_SSR(residuals)
SEE <- calculate_SEE(SSR,df)
var_b <- calculate_var_b(SSR,x)
t_beta <- calculate_t_beta(beta_hat,var_b)
r2 <- calculate_r2(SSR,SS_tot)
adjusted_r2 <- calculate_adjusted_r2(r2, nrow(t), ncol(x))

cat("Beta_hat:\n", beta_hat, "\n")

#1. Begin with OLS routine. Save your residuals.
cat("Residuals:\n", residuals, "\n")
cat("SEE:\n", SEE, "\n")
cat("var_b:\n", var_b, "\n")
cat("t_beta:\n", t_beta, "\n")
cat("r2:\n", r2, "\n")
cat("adjusted_r2:\n", adjusted_r2, "\n")


#2. ACF and PACF correlograms
acf(residuals, lag.max = 30,type = c("correlation"), plot = TRUE)
pacf(residuals, lag.max = 30,plot = TRUE)

#3. find the appropriate diagnostic tests
#and use them to confirm.
Box.test(residuals, lag = 10, type = "Ljung-Box")

#########################  Transforming  #########################
#4. Transforming the data with AR1 error process
lagged_X2 <- c(NA, diff(data$X2))
data$X2tr <- data$X2 - lagged_X2
data$X2tr[is.na(data$X2tr)] <- data$X2tr[2]

x <- as.matrix(data[, c('X2tr', 'X3', 'X4')])


df <- nrow(t) - ncol(x) - 1  
t <- as.matrix(data['Y1'])
t

transformed_SS_tot <- calculate_SS_tot(t)
transformed_df <- nrow(t) - ncol(x) - 1  
transformed_beta_hat <- calculate_beta_hat(t, x)
transformed_residuals <- calculate_residual(t,x,transformed_beta_hat)
transformed_SSR <- calculate_SSR(transformed_residuals)
transformed_SEE <- calculate_SEE(transformed_SSR,transformed_df)
transformed_var_b <- calculate_var_b(transformed_SSR,x)
transformed_t_beta <- calculate_t_beta(transformed_beta_hat,transformed_var_b)
transformed_r2 <- calculate_r2(transformed_SSR,transformed_SS_tot)
transformed_adjusted_r2 <- calculate_adjusted_r2(transformed_r2, nrow(t), ncol(x))

cat("Transformed Beta_hat:\n", transformed_beta_hat, "\n")

#1. Begin with OLS routine. Save your residuals.
cat("Transformed Residuals:\n", transformed_residuals, "\n")
cat("Transformed SEE:\n", transformed_SEE, "\n")
cat("Transformed var_b:\n", transformed_var_b, "\n")
cat("Transformed t_beta:\n", transformed_t_beta, "\n")
cat("Transformed r2:\n", transformed_r2, "\n")
cat("Transformed adjusted_r2:\n", transformed_adjusted_r2, "\n")

acf(transformed_residuals, lag.max = 30,type = c("correlation"), plot = TRUE)
pacf(transformed_residuals, lag.max = 30,plot = TRUE)
Box.test(transformed_residuals, lag = 10, type = "Ljung-Box")

# Validate with built-in lm function
data_lm <- data.frame(
  y = as.vector(dataset[['Y1']]),
  x2 = as.vector(dataset[['X2']]),
  x3 = as.vector(dataset[['X3']]),
  x4 = as.vector(dataset[['X4']])
)

model <- lm(y ~ x2 + x3 + x4, data = data_lm)
summary(model)


###########################################
##############  QUESTION 2  ###############
###########################################
library(car)
library(lmtest)
library(pgirmess)
library(skedastic)

dataset <- read.csv("KanzlerDaten.csv")
data_heter <- na.omit(dataset) 

glimpse(data_heter)
model_heter <- lm(Y4 ~ X2 + X3 + X4, data = data_heter)
summary(model_heter)
heter_residuals <- residuals(model_heter)

#1. perform Breusch-Pagan test
bptest(model_heter)

#2. perform Glesjer test
glejser(model_heter,
        auxdesign = NA,
        sigmaest = c("main", "auxiliary"),
        statonly = FALSE)

residuals_sq <- residuals(model_heter)^2

aux_model_X2 <- lm(residuals_sq ~ X2, data = data_heter)
summary(aux_model_X2)

aux_model_X3 <- lm(residuals_sq ~ X3, data = data_heter)
summary(aux_model_X3)

aux_model_X4 <- lm(residuals_sq ~ X4, data = data_heter)
summary(aux_model_X4)

# Transform the predictor matrix
# The addition of 1 is a key part of stabilizing the variance of the residuals.
data_heter$Y4_transformed <- residuals(model_heter) / sqrt(1 + residuals_sq)
data_heter$X2_transformed <- data_heter$X2 / sqrt(1 + residuals_sq)
data_heter$X3_transformed <- data_heter$X3 / sqrt(1 + residuals_sq)
data_heter$X4_transformed <- data_heter$X4 / sqrt(1 + residuals_sq)

library(nlme)
# Fit GLS model with a variance structure
gls_model <- gls(Y4_transformed ~ X2_transformed + X3_transformed + X4_transformed,
                 data = data_heter,
                 weights = varPower(form = ~ X2_transformed))  # or other appropriate variance structure
summary(gls_model)

# Check residuals of the GLS model
gls_residuals <- residuals(gls_model)

# Glesjer test checking variables.
residuals_gls <- residuals(gls_model)^2

aux_model_X2 <- lm(residuals_gls ~ X2_transformed, data = data_heter)
summary(aux_model_X2)

plot(gls_residuals, main = "Residuals of GLS Model")
plot(heter_residuals, main = "Residuals of GLS Model")
# Compare GLS and OLS results
summary(model_heter)  # OLS results
summary(gls_model)    # GLS results


###########################################
##############  QUESTION 3  ###############
###########################################

dataset_Q3 <- read.csv("Singular.csv")
head(dataset_Q3)
y <- as.matrix(dataset_Q3['y'])
x <- as.matrix(dataset_Q3[, c('X2','X3','X4','X5','X6','X7')])

#1. fit the model
model_full <- lm(y ~ X2 + X3 + X4 + X5 + X6 + X7, data = dataset_Q3)
summary(model_full)

#2. use the Variance Inflation Factor (VIF) to diagnose multicollinearity
vif_value <- vif(model_full)
vif_value 

#Option 1 discard independent variables of multicollinearity
model_discard <- lm(y ~  X3 + X4 + X5, data = dataset_Q3)
summary(model_discard)
vif_value_discard <- vif(model_discard)
vif_value_discard

anova(model_discard, model_full)
AIC(model_discard, model_full)
BIC(model_discard, model_full)


#Option 2 PCA
# Perform PCA
pca <- prcomp(dataset_Q3[, c('X2', 'X3', 'X4', 'X5', 'X6', 'X7')], scale. = TRUE)
pca_scores <- pca$x

# Fit a model using the principal components
model_pca <- lm(y ~ pca_scores[, 1:5], data = data.frame(y = dataset_Q3$y, pca_scores))
summary(model_pca)
anova(model_pca, model_full)
AIC(model_pca, model_full)
BIC(model_pca, model_full)


###########################################
##############  QUESTION 4  ###############
###########################################
library(dplyr)
library(tibble)
dataset_Q4 <- read.csv("TVlaVie.csv", header = FALSE, skip = 3)
dataset_Q4 <- dataset_Q4 %>% filter(rowSums(is.na(.))==0)
dataset_Q4
head(dataset_Q4)

dataset_Q4$Life.Expectancy
model1 <- lm(V2 ~ V3, data = dataset_Q4)
model2 <- lm(V2 ~ V3 + V4, data = dataset_Q4)
anova(model1, model2)
AIC(model1, model2)
BIC(model1, model2)


###########################################
#############  QUESTION 4-2  ##############
###########################################
head(dataset_Q3)
modelA <- lm(y ~ X3 + X4, data = dataset_Q3)
modelB <- lm(y ~ X3 + X5, data = dataset_Q3)
modelC <- lm(y ~ X5 + X6 + X7, data = dataset_Q3)
# Calculate AIC
aicA <- AIC(modelA)
aicB <- AIC(modelB)
aicC <- AIC(modelC)

# Calculate BIC
bicA <- BIC(modelA)
bicB <- BIC(modelB)
bicC <- BIC(modelC)

# Print AIC and BIC
print(c(AIC_A = aicA, AIC_B = aicB, AIC_C = aicC))
print(c(BIC_A = bicA, BIC_B = bicB, BIC_C = bicC))


###########################################
##############  QUESTION 5  ###############
###########################################
dataset_Q5 <- read.csv("eo_survey.csv")
head(dataset_Q5)

# transform party from categorical to numerical
dataset_Q5$party_new <- as.numeric(factor(dataset_Q5$party, 
                                          levels = c("Democratic Party", 
                                                     "Republican Party", 
                                                     "No Party, Independent, Decline to state")))
dataset_Q5$party <- dataset_Q5$party_new
dataset_Q5 <- dataset_Q5 %>% select(-party_new)
head(dataset_Q5)

# build the logit model
model_logit <- glm(const ~ approve + ideo + party + inc, data = dataset_Q5, family = binomial)
summary(model_logit)

# Predicted probabilities for the first two observations
predicted_probability <- predict(model_logit, type = "response")[1:2]
predicted_probability



# MANUALLY calculate predicted probabilities
calculate_pred_probability <- function(approve,ideo,party,inc){
  X_beta <- -0.07649+(0.70263*approve)+(-0.15691*ideo)+(-0.24840*party)+(0.03409*inc)
  pred_probability <- exp(X_beta)/(1+exp(X_beta))
  return(pred_probability)
}

# MANUALLY calculate the first observations
pred_probability_first <- calculate_pred_probability(1,6,2,7)
pred_probability_first

# MANUALLY calculate the first observations
pred_probability_second <- calculate_pred_probability(3,2,1,11)
pred_probability_second


# Predicted Probability Plot
library(ggplot2)
library(tidyverse)
library(dplyr)

dataset_Q5$pred_prob <- predict(model_logit, dataset_Q5, type = "response")

dataset_Q5_plot <- dataset_Q5 %>%
  mutate(predlow = plogis(const - (1.96 * pred_prob)),
         pred = plogis(const),
         predhigh = plogis(const + (1.96 * pred_prob)))

dataset_Q5_plot

#####
# Predicted probability plot in ggplot2. 
dataset_Q5_plot %>%
  ggplot(aes(x =  ideo, y = pred_prob)) + 
  geom_ribbon(aes(ymin = predlow,
                  ymax = predhigh, 
                  fill =  const), alpha = 0.3) + 
  geom_line(aes(colour = const), size = 1) +
  labs(y = "Predicted Probability",
       x = "Ideology",
       title = "Predicted Probability by Ideology",
       subtitle = "Holding other variables constant",
       color = "Constitutionality",
       fill = "Constitutionality") +
  scale_color_manual(values = c("#3368d8", "#33a2d8"),
                     labels = c("Unconstitutional", "Constitutional")) +
  scale_fill_manual(values = c("#3368d8", "#33a2d8"),
                    labels = c("Unconstitutional", "Constitutional")) +
  guides(color = guide_legend(reverse = TRUE)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal()


newdata2 <- with(dataset_Q5, data.frame(approve = mean(approve, na.rm = TRUE),
                                 ideo = seq(1, 7, by = 0.1),
                                 party = "Democratic Party",
                                 inc = mean(inc, na.rm = TRUE)))


# Use the simulated data to created predicted logits
eomod <- glm(const ~ approve + ideo + party + inc, family = "binomial", data = dataset_Q5)
summary(eomod)
predznew <- predict(eomod, newdata = newdata2, type = "link", se = TRUE)
plotdat <- cbind(newdata2, predznew); plotdat


# Convert logits into predicted probabilities with standard errors.


newdata4 <- plotdat %>%
  mutate(predlow = plogis(fit - (1.96 * se.fit)),
         pred = plogis(fit),
         predhigh = plogis(fit + ((1.96 * se.fit))))
glimpse(newdata4)


# Make the plot of the predicted probabilities. Add labels and such too.


newdata4 %>%
  ggplot(aes(x = ideo, y = pred)) +
  geom_ribbon(aes(ymin = predlow, ymax = predhigh), 
              fill = "#a1d99b", alpha = 0.3) +  # Adjusted fill color for ribbon
  geom_line(color = "#2c7fb8", size = 1) +     # Adjusted line color and thickness
  labs(y = "Predicted Probability",
       x = "Ideology",
       title = "Predicted Probability by Ideology",
      ) +
  theme_minimal()


