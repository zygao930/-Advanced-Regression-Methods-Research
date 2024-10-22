# Q1
A <- matrix(c(5, -3, 8,
              4, 1, 0,
              8, 2, 4), 
            nrow = 3,
            byrow = FALSE)
A


B <- matrix(c(1, 1, 2,
              2, 0, 9,
              5, 8, 7), 
            nrow = 3,
            byrow = TRUE)
B

C <- matrix(c(1, 2, 7,
              4, 3, 9), 
            nrow = 2,
            byrow = TRUE)
C

# Matrix multiplication
B %*% A
C %*% A

# Q2
X <- matrix(c(1, 4, 2,
              1, 3, 5,
              1, 9, 0), 
            nrow = 3,
            byrow = TRUE)

X

y <- matrix(c(7,
              2,
              3), 
            nrow = 3,
            byrow = TRUE)

y

X_t <- t(X)
X_t

X %*% X_t

X_t %*% y

# Q4
e <- matrix(c(1,
              2,
              3), 
            nrow = 3,
            byrow = TRUE)
e
e_t <- t(e)
e %*% e_t
e_t %*% e

# Q5
# install.packages('matlib')
library(matlib)

t <- matrix(c(8,
              13,
              5), 
            nrow = 3,
            byrow = TRUE)
t

x <- matrix(c(5,
              9,
              2), 
            nrow = 3,
            byrow = TRUE)
c <- cbind(1,x)
c

c_t <- t(c)
c_t

beta_hat <- (solve(c_t %*% c) %*% c_t) %*% t
beta_hat

e_1 <- -0.29
e_2 <- 0.11
e_3 <- 0.16

SSR <- e_1^2 + e_2^2 + e_3^2
SSR
df <- 1

SEE <- sqrt(SSR/df)
SEE

var_b = SSR * solve(c_t %*% c)
var_b

sqrt(var_b[1,1])
sqrt(var_b[2,2])

r_square = 1 - SSR/32.6
r_square

data <- data.frame(
  y = c(8,13,5),
  x = c(5,9,2)
)

model <- lm(y ~ x,data = data)
summary(model)

# Q6: Generalization template for linear model

calculate_beta_hat <- function(t,x){
  c <- cbind(1,x)
  c_t <- t(c)
  beta_hat <- solve(c_t %*% c) %*% c_t %*% t
  return(beta_hat) 
}

calculate_residual <- function(t,x,beta_hat){
  c <- cbind(1,x)
  y_hat <-  c %*% beta_hat
  residuals <- t - y_hat
  return(residuals) 
}

calculate_SSR <- function(residuals){
  SSR <-  sum(residuals^2)
  return(SSR) 
}

calculate_SEE <- function(SSF,df){
  SEE <- sqrt(SSR/df)
  return(SEE) 
}

calculate_var_b <- function(SSR,x){
  c <- cbind(1, x)
  c_t <- t(c)
  var_b <- SSR * solve(c_t %*% c)
  return(var_b)
}

calculate_SS_tot <- function(t){
  t_mean <- mean(t)
  y_e <- (t-t_mean)^2
  SST <-  sum(y_e)
  return(SST)
}

calculate_var_b <- function(SSR,x){
  c <- cbind(1, x)
  c_t <- t(c)
  var_b <- SSR * solve(c_t %*% c)
  return(var_b)
}


calculate_r2 <- function(SSR,SS_tot){
  r2 <- 1 - SSR/SS_tot
  return(r2)
}

x <- matrix(c(39.4, 40.1, 44.3, 38.2, 48.4, 41.9, 45.9, 41.2,  # Average Age
              5511.8, 4855.2, 3825.5, 5600.6, 3974.4, 3847.2, 5081.2, 4382.9),  # Average Income
            nrow = 8, byrow = FALSE)

t <- matrix(c(29.3, 31.8, 44.3, 27.2, 57.6, 39.7, 53.8, 32.6), nrow = 8, byrow = FALSE)


SS_tot <- calculate_SS_tot(t)
SS_tot

df <- nrow(t) - 3  

beta_hat <- calculate_beta_hat(t,x)
residuals <- calculate_residual(t,x,beta_hat)
SSR <- calculate_SSR(residuals)
SEE <- calculate_SEE(SSR,df)
var_b <- calculate_var_b(SSR,x)
r2 <- calculate_r2(SSR,SS_tot)

cat("Beta_hat:\n", beta_hat, "\n")
cat("SSR:\n", SSR, "\n")
cat("SEE:\n", SEE, "\n")
cat("r2:\n", r2, "\n")
cat("var_b:\n", var_b, "\n")
cat("Standard Errors of Beta:\n", sqrt(diag(var_b)), "\n")
