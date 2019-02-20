
################################ ECON 613 HW 2 ################################ 
# The list of result objects for each question is listed at the end of this document.

#Set working directory
setwd("C:/Users/promv/Dropbox/Master Course work/Spring 2019/Econ 613 Applied econometric micro/Assignment 2")
#Clear workspcae
rm(list = ls())

# Time code runtime
ptm <- proc.time()

# House keeping ---------------------------------------------
#Load required library
#install.packages("ggplot2")
library(ggplot2)
#install.packages("matlib")
library(matlib)
#install.packages("dplyr")
library(dplyr)
#install.packages("caret")
library(caret)
#install.packages("gdata")
library(gdata)
#install.packages("stargazer")
library(stargazer)
#install.packages("mfx")
library(mfx)



# Exercise 1 Data Creation ------------------------------------------------
#set random seed
set.seed(12345)

#create X1: uniform distribution with range 1:3
X1 <- runif(n = 10000, min = 1, max = 3)
hist(X1)

#create X2: gamma distribution with shape 3 and scale 2
X2 <- rgamma(n = 10000, shape = 3, scale = 2)
hist(X2)

#create X3: binomial distribution with probability 0.3
X3 <- rbinom(n = 10000, size = 1,prob = 0.3)
hist(X3)

#create eps: normal distribution with mean 2 and sd 1
eps <- rnorm(n = 10000, mean = 2, sd = 1)
hist(eps)

#create Y: Y = 0.5 + (1.2)(X1) + (-0.9)(X2) + (0.1)(X3) + eps
Y <- 0.5 + (1.2)*(X1) + (-0.9)*(X2) + (0.1)*(X3) + eps
hist(Y)

#create ydum: a binomial variable which =1 when Y>mean(Y), and =0 otherwise
##create ydum = 0 for all 10000 observation
ydum <- replicate(n = 10000, 0)
##replace ydum = 1 if Y > mean(Y)
ydum[Y>mean(Y)] <- 1
hist(ydum)

#Create a dataframe containing all variables 
OLS_data <- data.frame(Y,X1,X2,X3)
Probit_data <- data.frame(ydum,X1,X2,X3)

#keep only dataframes
keep(ptm, OLS_data, Probit_data, sure = TRUE)



# Exercise 2 OLS ----------------------------------------------------------
#Calculate correlation between Y and X1
## Method 1 Using correlation formular
Y_X1_Corr_formular <- (1/(nrow(OLS_data)-1))*sum(scale(OLS_data$X1)*scale(OLS_data$Y))
## Method 2 Using R build-in package
Y_X1_Corr_R_package <- cor(OLS_data$Y,OLS_data$X1)
# it happens that they are exactly equal


#In order to facilitate bootstrap process afterward, I will make a function that return OLS coefficient first
#Function olscoeff is the function to retrieve OLS, Y = f(X1, X2, X3), coefficient
##input : dataframe is the dataframe in the form Y, X1, X2, .., Xn
##output : a matrix of OLS coeff
olscoeff <- function(dataframe) {
  # Create X matrix = [1, X1, X2, X3]
  X <- as.matrix(dataframe[,2:ncol(dataframe)])
  # add a column of 1 (intercept)
  X <- cbind(replicate(1,n=nrow(dataframe)),X)
  # Calculate coefficient using the formular B = (X'X)^-1 X'Y
  coeff <- Inverse(crossprod(X)) %*% t(X) %*% dataframe$Y
  rownames(coeff) <- c("c", "X1", "X2", "X3")
  colnames(coeff) <- c("OLS Coefficient")
  return(t(coeff))
}

#Calculate coefficient of Y = f(X1, X2, X3) using the created function
OLS_coeff <- t(olscoeff(OLS_data))

#Calculate the standard errors
##Method 1: assuming error has normal distribution (which it does), then
## Variance-Covariance matrix of OLS coefficient can be calculated by the formular SE(B) <- var(error)((X'X)^-1)
## So SE can be obtained by sqrt(diagnonal elements) of that matrix
### 1) generate predicted Y, OLS_Y_hat
X <- as.matrix(cbind(replicate(1, n=nrow(OLS_data)), OLS_data[,2:ncol(OLS_data)]))
OLS_Y_hat <- X %*% OLS_coeff
### 2) calculate error
OLS_error <- OLS_data$Y - OLS_Y_hat
### 3) calculate Variance-Covariance matrix
OLS_cov_mat <- as.numeric(var(OLS_error)) * Inverse(crossprod(X))
### 4) calculate SE
OLS_SE_formular <- data.frame(diag(apply(OLS_cov_mat, 1, sqrt)))
colnames(OLS_SE_formular) <- c("Standard Formular")


#Method 2: Using Bootstrap with 49 and 499 replications
## For 49 replications
OLS_coeff_boot_49 <- numeric()
###create for loop: random sampling data with replacement and calculate coefficient 49 times
for (i in 1:49) {
  ###generate new data frame by random sampling the main dataframe with replacement
  boot_sample <- OLS_data[sample(nrow(OLS_data), size = nrow(OLS_data), replace = TRUE),]
  ###retrieve OLS coefficient and keep them in OLS_coeff_boot_49
  OLS_coeff_boot_49 <- rbind(OLS_coeff_boot_49, olscoeff(boot_sample))
}
###calculate SE by finding SD for each column in OLS_coeff_boot_49
OLS_SE_boot_49 <- apply(OLS_coeff_boot_49, 2, sd)

## For 499 replications
OLS_coeff_boot_499 <- numeric()
###create for loop: random sampling data with replacement and calculate coefficient 499 times
for (i in 1:499) {
  ###generate new data frame by random sampling the main dataframe with replacement
  boot_sample <- OLS_data[sample(nrow(OLS_data), size = nrow(OLS_data), replace = TRUE),]
  ###retrieve OLS coefficient and keep them in OLS_coeff_boot_49
  OLS_coeff_boot_499 <- rbind(OLS_coeff_boot_499, olscoeff(boot_sample))
}
###calculate SE by finding SD for each column in OLS_coeff_boot_499
OLS_SE_boot_499 <- apply(OLS_coeff_boot_499, 2, sd)

#Using R built-in OLS function to crosscheck
OLS_R_package <- lm(Y ~ ., OLS_data)


#Combine all result into one table
##coefficient Table
OLS_coeff_table <- cbind(OLS_coeff, coef(summary(OLS_R_package))[, "Estimate"])
colnames(OLS_coeff_table) <- c("Standard formular", "R OLS package")
##SE table
OLS_SE_table <- cbind(OLS_SE_formular, as.data.frame(OLS_SE_boot_49), as.data.frame(OLS_SE_boot_499), coef(summary(OLS_R_package))[, "Std. Error"] )
rownames(OLS_SE_table) <- c("c", "X1", "X2", "X3")
colnames(OLS_SE_table) <- c("Standard formular", "Bootstrap 49", "Bootstrap 499", "R OLS package")




# Exercise 3 Numerical Optimization ---------------------------------------
#Write a function that returns the log likelihood of probit
##input : b is the vector of coefficient
##input : dataframe is the dataframe in the form Y(binary), X1, X2, .., Xn (have default value of Probit_data)
##output : log likelihood given b
probitLik <- function(b, dataframe=Probit_data) {
  #create XB matrix
  X <- cbind(replicate(nrow(dataframe),1),as.matrix(dataframe[,2:ncol(dataframe)]))
  XB <- X %*% b
  #calculate probit CDF for each observation (which is normal dist CDF so function pnorm is used)
  cdf <- apply(XB, c(1,2), pnorm)
  #Accoding to log likelihood formular, log(L) = sum((y_i*log(cdf_i)) + ((1-y_i)*log(1-cdf_i))),
  ## I replace cdf = (1-cdf) of observations that has y_i = 0
  cdf[dataframe$ydum == 0] <- 1-cdf[dataframe$ydum == 0]
  ## Then I take log. This give me (y_i*log(cdf_i)) + ((1-y_i)*log(1-cdf_i)) term 
  ## because loglikelihood_i = log(cdf_i) when y_i == 1 and loglikelihood_i = log(1-cdf_i) when y_i == 0
  log_likelihood <- apply(cdf, c(1,2), log)
  #calculate total log likelihood 
  total_log_likelihood <- sum(log_likelihood)
  #return total log likelihood
  return(total_log_likelihood)
}


#In order to perform steepest ascent, first, I write a function for finding gradient vector for each beta
##input : b is current coefficient vector
##input : dataframe is the dataframe in the form Y(binary), X1, X2, .., Xn (have default value of Probit_data)
##input : f is the function to find likelihood, which takes b as a first argument.
##input : h is the parameter for incremental step size (default = 0.01)
##output : gradient vector
gradient <- function(b, dataframe = Probit_data ,f = probitLik, h = 0.01) {
  #initialize gradient vector
  gradient_vector <- matrix(nrow = 4, ncol = 1)
  #for each b, replace b by b+h and keep track of how log likelihood change
  for (i in 1:4) {
    #increment coef by +h and calculate log likelihood
    coeff <- b
    coeff[i,1] <- b[i,1]+h
    plus_ll <- f(Probit_data,b=coeff)
    #increment coef by -h and calculate log likelihood
    coeff[i,1] <- b[i,1]-h
    minus_ll <- f(Probit_data,b=coeff)
    #Calculate gradient
    gradient_vector[i,1] <- (plus_ll-minus_ll) / 2*h
  }
  return(gradient_vector)
}



#Implement steepest ascent optimization algorithm
##initialize parameters
### a is scaling parameter
a <- 0.1
### initial coefficient
new_coeff <- as.matrix(c(0,0,0,0))
### tol is tolerance level
tol <- 1
### max_iter is the maximum allowed iteration (to prevent running for too long)
max_iter <- 1000
### calculate initial log likelihood
new_loglikelihood <- probitLik(new_coeff,Probit_data)
###ll_collect collect log likelihood of each iteration
ll_collect <- numeric()
###oeff_collect collect coefficient of each iteration
coeff_collect <- numeric()
for (i in 1:max_iter) {
  base_coeff <- new_coeff
  base_loglikelihood <- new_loglikelihood
  new_coeff <- base_coeff + a*(gradient(base_coeff))
  new_loglikelihood <- probitLik(new_coeff,Probit_data)
  ll_collect <- rbind(ll_collect,new_loglikelihood)
  coeff_collect <- cbind(coeff_collect,new_coeff)
  #stop condition: if log likelihood absolute change is less than tolerance level 
   if (abs(new_loglikelihood - base_loglikelihood) <= tol) {
     break
   }
}
# Use new_coeff as a result
Probit_coeff_steep <- new_coeff
# Compare coeff with real beta
Probit_coeff_steep <- cbind(Probit_coeff_steep, c(2.5,1.2,-0.9,0.1))
row.names(Probit_coeff_steep) <- c("intercept","X1","X2","X3")
colnames(Probit_coeff_steep) <- c("Steepest ascent", "Expected Coefficient")



# Exercise 4 Discrete Choice ----------------------------------------------
#Probit
##add negative to the function that return probit likelihood created in last exercise (turn min to max)
negprobitLik <- function(b, dataframe=Probit_data) {
  #create XB matrix
  X <- cbind(replicate(nrow(dataframe),1),as.matrix(dataframe[,2:ncol(dataframe)]))
  XB <- X %*% b
  #calculate probit CDF for each observation (which is normal dist CDF so function pnorm is used)
  cdf <- apply(XB, c(1,2), pnorm)
  #Accoding to log likelihood formular, log(L) = sum((y_i*log(cdf_i)) + ((1-y_i)*log(1-cdf_i))),
  ## I replace cdf = (1-cdf) of observations that has y_i = 0
  cdf[dataframe$ydum == 0] <- 1-cdf[dataframe$ydum == 0]
  ## Then I take log. This give me (y_i*log(cdf_i)) + ((1-y_i)*log(1-cdf_i)) term 
  ## because loglikelihood_i = log(cdf_i) when y_i == 1 and loglikelihood_i = log(1-cdf_i) when y_i == 0
  log_likelihood <- apply(cdf, c(1,2), log)
  #calculate total log likelihood 
  total_log_likelihood <- sum(log_likelihood)
  #return total log likelihood
  return(-total_log_likelihood)
}
#initialize search vector
b<-c(0,0,0,0)
# Optimize min(-likelihood) = max likelihood
Probit_optim <- nlm(negprobitLik,b,hessian = TRUE)
# get coefficient
Probit_optim_coeff <- Probit_optim$estimate
# get Hessian matrix
Probit_optim_Hessian <- Probit_optim$hessian
# get variance covariance matrix = inverse of hessian matrix
var_cov_Probit <- Inverse(Probit_optim_Hessian)
# get SE = square root of diagnoal of var-cov matrix
Probit_optim_SE <- sqrt(diag(var_cov_Probit))


#Logit
## Create a function that return -logit likelihood to facilitate minimization (turn min to max)
neglogitLik <- function(b, dataframe=Probit_data) {
  #create XB matrix
  X <- cbind(replicate(nrow(dataframe),1),as.matrix(dataframe[,2:ncol(dataframe)]))
  XB <- X %*% b
  #calculate logit CDF for each observation (which is logistic dist CDF so function plogis is used)
  cdf <- apply(XB, c(1,2), plogis)
  #Accoding to log likelihood formular, log(L) = sum((y_i*log(cdf_i)) + ((1-y_i)*log(1-cdf_i))),
  ## I replace cdf = (1-cdf) of observations that has y_i = 0
  cdf[dataframe$ydum == 0] <- 1-cdf[dataframe$ydum == 0]
  ## Then I take log. This give me (y_i*log(cdf_i)) + ((1-y_i)*log(1-cdf_i)) term 
  ## because loglikelihood_i = log(cdf_i) when y_i == 1 and loglikelihood_i = log(1-cdf_i) when y_i == 0
  log_likelihood <- apply(cdf, c(1,2), log)
  #calculate total log likelihood 
  total_log_likelihood <- sum(log_likelihood)
  #return total log likelihood
  return(-total_log_likelihood)
}
#initialize search vector
b<-c(0,0,0,0)
# Optimize min(-likelihood) = max likelihood
Logit_optim <- nlm(neglogitLik,b, hessian = TRUE)
# get coefficient
Logit_optim_coeff <- Logit_optim$estimate
# get Hessian matrix
Logit_optim_Hessian <- Logit_optim$hessian
# get variance covariance matrix = inverse of hessian matrix
var_cov_Logit <- Inverse(Logit_optim_Hessian)
# get SE = square root of diagnoal of var-cov matrix
Logit_optim_SE <- sqrt(diag(var_cov_Logit))


#Linear probability model
## a function that return loss (sum square error) for OLS
LPLoss <- function(b, dataframe=Probit_data) {
  #create XB matrix
  X <- cbind(replicate(nrow(dataframe),1),as.matrix(dataframe[,2:ncol(dataframe)]))
  XB <- X %*% b
  #calculate error for each observation (predicted_y - actual y)
  error <- XB - dataframe$ydum
  #square error
  error2 <- apply(error, c(1,2), function(x) x^2)
  #calculate sum sq error
  sum_sq_error <- sum(error2)
  #return - sum_sq_error to be minimized
  return(sum_sq_error)
}
#initialize search vector
b<-c(0,0,0,0)
# Optimize min(-likelihood) = max likelihood
LP_optim <- nlm(LPLoss,b,hessian = TRUE)
# get coefficient
LP_optim_coeff <- LP_optim$estimate
# get SE
## 1) generate predicted Y
X <- as.matrix(cbind(replicate(1, n=nrow(Probit_data)), Probit_data[,2:ncol(Probit_data)]))
LP_optim_yhat <- X %*% LP_optim_coeff
LP_optim_yhat[LP_optim_yhat>=0.5] <- 1
LP_optim_yhat[LP_optim_yhat<0.5] <- 0
## 2) calculate error
LP_error <- Probit_data$ydum - LP_optim_yhat
## 3) calculate Variance-Covariance matrix
var_cov_LP <- as.numeric(var(LP_error)) * Inverse(crossprod(X))
## 4) calculate SE
LP_optim_SE <- sqrt(diag(var_cov_LP))


# Compare the optimization result with R glm package result
#Probit
Probit_R_package <- glm(ydum ~ .,data = Probit_data, family = binomial(link="probit"))
#compare
Probit_compare <- rbind(Probit_optim_coeff,Probit_optim_SE,Probit_R_package$coefficients,summary(Probit_R_package)$coefficients[, 2])
row.names(Probit_compare) <- c("Probit from optimization:Coefficient","Probit from optimization:SE", "Probit from glm package","Probit from glm SE")

#Logit
Logit_R_package <- glm(ydum ~ .,data = Probit_data, family = binomial(link="logit"))
#compare
Logit_compare <- rbind(Logit_optim_coeff,Logit_optim_SE,Logit_R_package$coefficients,summary(Logit_R_package)$coefficients[, 2])
row.names(Logit_compare) <- c("Logit from optimization:Coefficient","Logit from optimization:SE", "Logit from glm package","Logit from glm SE")

#Linear probability
LP_R_package <- lm(ydum ~ ., Probit_data)
#compare
LP_compare <- rbind(LP_optim_coeff,LP_optim_SE,LP_R_package$coefficients,summary(LP_R_package)$coefficients[, 2])
row.names(LP_compare) <- c("LP from optimization:Coefficient","LP from optimization:SE", "LP from glm package","LP from glm SE")



# Exercise 5 Marginal Effects ---------------------------------------------
#Compute marginal Effect
##Probit marginal effect : Average marginal effects in the sample.
### Method 1: manually calculating average pdf and multiply with Coeff
#get XB
X <- cbind(replicate(nrow(Probit_data),1),as.matrix(Probit_data[,2:ncol(Probit_data)]))
XB <- X %*% Probit_optim_coeff
#get F'(XB) = first derivative of CDF(XB) = PDF(XB)
Probit_pdf <- apply(XB, c(1,2), dnorm)
#get average F'(XB)
Probit_pdf <- mean(Probit_pdf)
#ME = F'(XB)B
Probit_ME <- Probit_pdf*Probit_optim_coeff

### Method 2: compare result with mfx package (based on glm model, but the result should be similar)
Probit_ME_mfx <- probitmfx(Probit_R_package, Probit_data, atmean = FALSE)
#2 results is very close to each other 


##Logit marginal effect : Average marginal effects in the sample.
### Method 1: manually calculating average pdf and multiply with Coeff
#get XB
XB <- X %*% Logit_optim_coeff
#get F'(XB) = first derivative of CDF(XB) = PDF(XB)
Logit_pdf <- apply(XB, c(1,2), dlogis)
#get average F'(XB)
Logit_pdf <- mean(Logit_pdf)
#ME = F'(XB)B
Logit_ME <- Logit_pdf*Logit_optim_coeff

### Method 2: compare result with mfx package (based on glm model, but the result should be similar)
Logit_ME_mfx <- logitmfx(Logit_R_package, Probit_data, atmean = FALSE)


#Table for ME Report
ME_report <- cbind(Probit_ME, Logit_ME)
colnames(ME_report) <- c("Probit ME", "Logit ME")
row.names(ME_report) <- c("Intercept", "X1", "X2", "X3")



#Compute standard deviation
#The delta method
## Probit delta method
### 1) create X matrix
X <- cbind(replicate(nrow(Probit_data),1),as.matrix(Probit_data[,2:ncol(Probit_data)]))
### 2) #get F'(XB) = first derivative of CDF(XB) = PDF(XB)
XB <- X %*% Probit_optim_coeff
Probit_pdf <- t(apply(XB, c(1,2), dnorm))
### 3) calculate jacobian matrix
J <-  (1/nrow(Probit_data)) * (Probit_pdf %*% X)
### 4) calculate var(model) by JVJ'
Probit_var <- J %*% var_cov_Probit %*% t(J)
### 5) Create a function to calculate SD of each regressor's ME through SD(b_i) = sqrt(model_var/var_xi)
SDcalc<- function(model_var) {
  x <- as.matrix(apply(X,c(2),var))
  for (i in 1:nrow(x)) {
    x[i,1] <- sqrt(model_var/x[i,1])
  }
  return(t(x))
}
### 6) use the function in 5) to find marginal effect's SD
Probit_SD_delta <- SDcalc(Probit_var)

## Logit delta method
### 1) create X matrix
X <- cbind(replicate(nrow(Probit_data),1),as.matrix(Probit_data[,2:ncol(Probit_data)]))
### 2) #get F'(XB) = first derivative of CDF(XB) = PDF(XB)
XB <- X %*% Logit_optim_coeff
Logit_pdf <- t(apply(XB, c(1,2), dlogis))
### 2) calculate jacobian matrix
J <-  (1/nrow(Probit_data)) * (Logit_pdf %*% X)
### 3) calculate var(model) by JVJ'
Logit_var <- J %*% var_cov_Logit %*% t(J)
### 4) calculate SD using the function made above
Logit_SD_delta <- SDcalc(Logit_var)


#Bootstrap
Probit_ME_boot <- numeric()
Logit_ME_boot <- numeric()
#initialize b 
b <- c(0,0,0,0)
##create for loop to random sampling data with replacement 30 times (To limit runtime)
for (i in 1:30) {
  ###generate new data frame by random sampling the main dataframe with replacement
  boot_sample <- Probit_data[sample(nrow(Probit_data), replace = TRUE),]
  ###Estimate new Probit model using boot_sample data
  ### Note: to save time from optimization, I utilize R glm package which is faster
  boot_Probit <- nlm(negprobitLik, b, dataframe = boot_sample)
  boot_Logit <- nlm(neglogitLik, b, dataframe = boot_sample)
  ###get coefficient
  boot_Probit_coeff <- as.matrix(boot_Probit$estimate)
  boot_Logit_coeff <- as.matrix(boot_Logit$estimate)
  ###get XB
  X_boot <- cbind(replicate(nrow(boot_sample),1),as.matrix(boot_sample[,2:ncol(boot_sample)]))
  XB_probit_boot <- X_boot %*% boot_Probit_coeff
  XB_logit_boot <- X_boot %*% boot_Logit_coeff
  ###get average F'(XB)
  boot_Probit_pdf <- mean(apply(XB_probit_boot, c(1,2), dnorm))
  boot_Logit_pdf <- mean(apply(XB_logit_boot, c(1,2), dlogis))
  ###get ME = F'(XB)B
  boot_Probit_ME <- boot_Probit_coeff*boot_Probit_pdf
  boot_Logit_ME <- boot_Logit_coeff*boot_Logit_pdf
  ###keep ME in Probit_ME_boot, and Logit_ME_boot
  Probit_ME_boot <- rbind(Probit_ME_boot, t(boot_Probit_ME))
  Logit_ME_boot <- rbind(Probit_ME_boot, t(boot_Probit_ME))
}
###calculate SD by finding SD for each column
Probit_SD_boot<- apply(Probit_ME_boot, 2, sd)
Logit_SD_boot<- apply(Logit_ME_boot, 2, sd)

#Table for SD report
SD_report <- cbind(t(Probit_SD_delta), Probit_SD_boot, t(Logit_SD_delta), Logit_SD_boot)
SD_report <- SD_report[2:nrow(SD_report),]
colnames(SD_report) <- c("Probit SD Delta", "Probit SD Bootstrap", "Logit SD Delta", "Logit SD Bootstrap")


# List of objects which are the answer ------------------------------------

## Exercise 1: OLS_data and Probit_data

## Exercise 2 Correlation Y, X1: Y_X1_Corr_formular
## Exercise 2 OLS Coefficient: OLS_coeff_table
## Exercise 2 OLS SE: OLS_SE_table

## Exercise 3 function that return probit likelihood: probitLik
## Exercise 3 steepest ascent result: Probit_coeff_steep

## Exercise 4 probit, logit, linear probability model: Probit_compare, Logit_compare, LP_compare

## Exercise 5 Marginal effect: ME_report
## Exercise 5 SD: SD_report

# Report runtime
proc.time() - ptm