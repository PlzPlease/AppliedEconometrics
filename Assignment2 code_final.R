
################################ ECON 613 HW 2 ################################ 
# The list of result objects for each question is listed at the end of this document.

#Set working directory
setwd("C:/Users/promv/Dropbox/Master Course work/Spring 2019/Econ 613 Applied econometric micro/Assignment 2")
#Clear workspcae
rm(list = ls())

# House keeping ---------------------------------------------
#Load required library
#install.packages("ggplot2")
library(ggplot2)
#install.packages("matlib")
library(matlib)
#install.packages("caret")
library(caret)
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
keep(OLS_data, Probit_data, sure = TRUE)


# Exercise 2 OLS ----------------------------------------------------------
#Calculate correlation between Y and X1
Y_X1_Corr <- cor(OLS_data$Y,OLS_data$X1)

#In order to facilitate bootstrap process afterward, I will make a function that return OLS coefficient first

#Function olscoeff is the function to retrieve OLS, Y = f(X1, X2, X3), coefficient
##input : dataframe is the dataframe in the form Y, X1, X2, .., Xn
##output : a matrix of OLS coeff
olscoeff <- function(dataframe) {
  # Create X matrix = [1, X1, X2, X3]
  X <- as.matrix(dataframe[,2:ncol(dataframe)])
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
OLS_SE_formular <- matrix(nrow = 4, ncol = 1)
for (i in 1:4) {
  OLS_SE_formular[i,1] <- sqrt(OLS_cov_mat[i,i])
}

#Method 2: Using Bootstrap with 49 and 499 replications
## For 49 replications
OLS_coeff_boot_49 <- numeric()
###create for loop to random sampling data with replacement 49 times
for (i in 1:49) {
  ###generate new data frame by random sampling the main dataframe with replacement
  boot_sample <- OLS_data[sample(nrow(OLS_data), replace = TRUE),]
  ###retrieve OLS coefficient and keep them in OLS_coeff_boot_49
  OLS_coeff_boot_49 <- rbind(OLS_coeff_boot_49, olscoeff(boot_sample))
}
###calculate SE by finding SD for each column in OLS_coeff_boot_49
OLS_SE_boot_49 <- apply(OLS_coeff_boot_49, 2, sd)

## For 499 replications
OLS_coeff_boot_499 <- numeric()
###create for loop to random sampling data with replacement 49 times
for (i in 1:499) {
  ###generate new data frame by random sampling the main dataframe with replacement
  boot_sample <- OLS_data[sample(nrow(OLS_data), replace = TRUE),]
  ###retrieve OLS coefficient and keep them in OLS_coeff_boot_49
  OLS_coeff_boot_499 <- rbind(OLS_coeff_boot_499, olscoeff(boot_sample))
}
###calculate SE by finding SD for each column in OLS_coeff_boot_49
OLS_SE_boot_499 <- apply(OLS_coeff_boot_499, 2, sd)

#Using R built-in OLS function as a crosscheck
OLS_R_package <- lm(Y ~ ., OLS_data)

#Combine all result into one table
##coefficient Table
OLS_coeff_table <- cbind(OLS_coeff, coef(summary(OLS_R_package))[, "Estimate"])
colnames(OLS_coeff_table) <- c("Standard formular", "R OLS package")
##SE table
OLS_SE_table <- cbind(OLS_SE_formular, as.data.frame(OLS_SE_boot_49), as.data.frame(OLS_SE_boot_499), coef(summary(OLS_R_package))[, "Std. Error"] )
rownames(OLS_SE_table) <- c("c", "X1", "X2", "X3")
colnames(OLS_SE_table) <- c("Standard formular", "Bootstrap 49", "Bootstrap 499", "R OLS package")

#keep only result and data table
keep(OLS_data, Probit_data, Y_X1_Corr, OLS_coeff_table, OLS_SE_table, sure = TRUE)



# Exercise 3 Numerical Optimization ---------------------------------------
#Write a function that returns the log likelihood of probit
##input : dataframe is the dataframe in the form Y(binary), X1, X2, .., Xn
##input : b is the vector of coefficient
##output : log likelihood

probitLik <- function(b, dataframe=Probit_data) {
  #initilize log likelihood = 0
  log_likelihood <- 0
  #for loop to get the summation of each observation
  for (i in 1:nrow(dataframe)) {
    #create x_i vector
    x_i <- dataframe[i,2:ncol(dataframe)]
    x_i <- as.matrix(cbind(1,x_i))
    #calculate probit CDF (which is normal dist CDF so function pnorm is used)
    cdf <- pnorm(x_i %*% b)
    #calculate log likelihood, sum over all observation
    log_likelihood <- log_likelihood + (dataframe[i,1]*log(cdf)) + ((1-dataframe[i,1])*log(1-cdf))
  }
    return(log_likelihood)
}



#Function for finding gradient vector(incremental step, h = 0.01)
##input : b is current coefficient vector
##output : gradient vector
gradient <- function(b, dataframe = Probit_data ,f = probitLik, h = 0.01, initial_loglikelihood = 0) {
  if(base_loglikelihood == 0) { base_loglikelihood <- f(dataframe,b=b)}
  gradient_matrix <- matrix(nrow = 4, ncol = 1)
  #for each b, replace b by b+h and keep log likelihood change
  for (i in 1:4) {
    #increment coef by +h and calculate log likelihood
    coeff <- b
    coeff[i,1] <- b[i,1]+h
    plus_ll <- f(Probit_data,b=coeff)
    #increment coef by -h and calculate log likelihood
    coeff[i,1] <- b[i,1]-h
    minus_ll <- f(Probit_data,b=coeff)
    #Calculate gradient
    gradient_matrix[i,1] <- (plus_ll-minus_ll) / 2*h
  }
  return(gradient_matrix)
}

#Implement steepest ascent optimization algorithm
##initialize parameters
### a is scaling parameter
a <- 0.1
### initial coefficient
new_coeff <- as.matrix(c(0,0,0,0))
### tol is tolerance level
tol <- 5
### max_iter is the maximum allowed iteration (to prevent running for too long)
max_iter <- 100
### calculate initial log likelihood
new_loglikelihood <- probitLik(new_coeff,Probit_data)
#iter count count how many iteration made
iter_count <- 0
#ll_collect collect log likelihood of each iteration
ll_collect <- numeric()
for (i in 1:max_iter) {
  base_coeff <- new_coeff
  base_loglikelihood <- new_loglikelihood
  new_coeff <- base_coeff + a*(gradient(base_coeff, initial_loglikelihood = base_loglikelihood))
  new_loglikelihood <- probitLik(new_coeff,Probit_data)
  ll_collect <- rbind(ll_collect,new_loglikelihood)
  #stop condition: if log likelihood absolute change is less than tolerance level 
   if (abs(new_loglikelihood - base_loglikelihood) <= tol) {
     break
   }
  loopcount <- i
}
# Use new_coeff as a result
Probit_coeff_steep <- new_coeff
# Compare coeff with real beta
Probit_coeff_steep <- cbind(Probit_coeff_steep, c(0.5,1.2,-0.9,0.1))
row.names(Probit_coeff_steep) <- c("intercept","X1","X2","X3")
colnames(Probit_coeff_steep) <- c("Steepest ascent", "Actual value")

#keep only using data, function and results
keep(OLS_data, Probit_data, Y_X1_Corr, OLS_coeff_table, OLS_SE_table, Probit_coeff_steep, probitLik  , sure = TRUE)



# Exercise 4 Discrete Choice ----------------------------------------------
#Probit
##add negative to the function that return likelihood to facilitate minimization (turn min to max)
negprobitLik <- function(b, dataframe=Probit_data) {
  #initilize log likelihood = 0
  log_likelihood <- 0
  #for loop to get the summation of each observation
  for (i in 1:nrow(dataframe)) {
    #create x_i vector
    x_i <- dataframe[i,2:ncol(dataframe)]
    x_i <- as.matrix(cbind(1,x_i))
    #calculate probit CDF (which is normal dist CDF so function pnorm is used)
    cdf <- pnorm(x_i %*% b)
    #calculate log likelihood, sum over all observation
    log_likelihood <- log_likelihood + (dataframe[i,1]*log(cdf)) + ((1-dataframe[i,1])*log(1-cdf))
  }
  #return - log likelihood to facilitate minimum optimization
  return(-log_likelihood)
}
#initialize search vector
b<-c(0,0,0,0)
# Optimize min(-likelihood) = max likelihood
Probit_optim <- nlm(negprobitLik,b)
# get coefficient
Probit_optim_coeff <- Probit_optim$estimate


#Logit
## a function that return -likelihood to facilitate minimization (turn min to max)
neglogitLik <- function(b, dataframe=Probit_data) {
  #initilize log likelihood = 0
  log_likelihood <- 0
  #for loop to get the summation of each observation 
  for (i in 1:nrow(dataframe)) {
    #create x_i vector
    x_i <- dataframe[i,2:ncol(dataframe)]
    x_i <- as.matrix(cbind(1,x_i))
    #calculate logit CDF (which is logistic dist CDF so function plogis is used)
    cdf <- plogis(x_i %*% b)
    #calculate log likelihood, sum over all observation
    log_likelihood <- log_likelihood + (dataframe[i,1]*log(cdf)) + ((1-dataframe[i,1])*log(1-cdf))
  }
  #return - log likelihood to facilitate minimum optimization
  return(-log_likelihood)
}
# Optimize min(-likelihood) = max likelihood
Logit_optim <- nlm(neglogitLik,b)
# get coefficient
Logit_optim_coeff <- Logit_optim$estimate


#Linear probability model
## a function that return loss (sum square error) for OLS
LPLik <- function(b, dataframe=Probit_data) {
  #initilize sum square error = 0
  sum_sq_error <- 0
  #for loop to get the summation of each observation 
  for (i in 1:nrow(dataframe)) {
    #create x_i vector
    x_i <- dataframe[i,2:ncol(dataframe)]
    x_i <- as.matrix(cbind(1,x_i))
    #calculate OLS loss = error^2 = (predicted_y - actual y)^2
    error2 <- ((x_i %*% b) - dataframe[i,1])^2
    #accumulate error^2
    sum_sq_error <- sum_sq_error + error2
  }
  #return - sum_sq_error to be minimized
  return(sum_sq_error)
}
# Optimize min(-likelihood) = max likelihood
LP_optim <- nlm(LPLik,b)
# get coefficient
LP_optim_coeff <- LP_optim$estimate


# Compare the optimization result with R glm package result
#Probit
Probit_R_package <- glm(ydum ~ .,data = Probit_data, family = binomial(link="probit"))
#compare
Probit_coeff_compare <- rbind(Probit_optim_coeff,Probit_R_package$coefficients)
row.names(Probit_coeff_compare) <- c("Probit from optimization", "Probit from glm package")

#Logit
Logit_R_package <- glm(ydum ~ .,data = Probit_data, family = binomial(link="logit"))
#compare
Logit_coeff_compare <- rbind(Logit_optim_coeff,Logit_R_package$coefficients)
row.names(Logit_coeff_compare) <- c("Logit from optimization", "Logit from glm package")

#Linear probability
LP_R_package <- lm(ydum ~ ., Probit_data)
#compare
LP_coeff_compare <- rbind(LP_optim_coeff,LP_R_package$coefficients)
row.names(LP_coeff_compare) <- c("Linear probability from optimization", "Linear probability from glm package")

#Since coefficient from my own optimization and R glm package are very similar,
#I will use the result from R glm and lm package from this point on because they come with a nice reporting package

#Export regression table in Latex format
stargazer(Probit_optim, Logit_R_package, LP_R_package, title="Model comparison", align=TRUE)

#keep only using data, function and results
keep(OLS_data, Probit_data, Y_X1_Corr, OLS_coeff_table, OLS_SE_table, Probit_coeff_steep, probitLik, Probit_optim_coeff, negprobitLik, Logit_optim_coeff, neglogitLik, LP_optim_coeff, LPLik, Probit_R_package, Logit_R_package, LP_R_package, Probit_coeff_compare, Logit_coeff_compare, LP_coeff_compare, sure = TRUE)



# Exercise 5 Marginal Effects ---------------------------------------------
#Compute marginal Effect
##Probit marginal effect : Average marginal effects in the sample.
### Method 1: manually calculating average pdf and multiply with Coeff
#get B = coeff
Probit_coeff <- as.matrix(coef(summary(Probit_R_package))[, "Estimate"])
#get average F'(XB) = average pdf(XB)
Probit_pdf <- mean(dnorm(predict(Probit_R_package)))
#ME = F'(XB)B
Probit_ME <- Probit_pdf*Probit_coeff

##Logit marginal effect : Average marginal effects in the sample.
### Method 1: manually calculating average pdf and multiply with Coeff
#get B = coeff
Logit_coeff <- as.matrix(coef(summary(Logit_R_package))[, "Estimate"])
#get average F'(XB) = average pdf(XB)
Logit_pdf <- mean(dlogis(predict(Logit_R_package)))
#ME = F'(XB)B
Logit_ME <-Logit_coeff*Logit_pdf

#Table for ME Report
ME_report <- cbind(Probit_ME, Logit_ME)
colnames(ME_report) <- c("Probit ME", "Logit ME")


#Compute the standard deviation
#The delta method
## Probit delta method
### 1) create X matrix
X <- Probit_data[,2:ncol(Probit_data)]
X <- as.matrix(cbind(1,X))
### 2) create probit pdf vector (first derivative of cdf)
pdf_vec <- t(as.matrix(dnorm(predict(Probit_R_package))))
### 3) calculate jacobian matrix
J <-  (1/nrow(Probit_data)) * (pdf_vec %*% X)
### 4) calculate coefficient var-cov matrix
var_cov <- vcov(Probit_R_package)
### 5) calculate var(model) by JVJ'
Probit_var <- J %*% var_cov %*% t(J)
### 6) a function to calculate SE of each regressor's ME through SE(b_i) = sqrt(model_var/var_xi)
SEcalc<- function(model_var) {
  x <- as.matrix(apply(X,c(2),var))
  for (i in 1:nrow(x)) {
    x[i,1] <- sqrt(model_var/x[i,1])
  }
  return(t(x))
}
### get SE
Probit_SE_delta <- SEcalc(Probit_var)


## Logit delta method
### 1) create logit pdf vector (first derivative of cdf)
pdf_vec <- t(as.matrix(dlogis(predict(Probit_R_package))))
### 2) calculate jacobian matrix
J <-  (1/nrow(Probit_data)) * (pdf_vec %*% X)
### 3) calculate var(model) by JVJ'
Logit_var <- J %*% var_cov %*% t(J)
### 4) calculate SE using the function made above
Logit_SE_delta <- SEcalc(Logit_var)



#Bootstrap
Probit_ME_boot <- numeric()
Logit_ME_boot <- numeric()
##create for loop to random sampling data with replacement 1000 times
for (i in 1:1000) {
  ###generate new data frame by random sampling the main dataframe with replacement
  boot_sample <- Probit_data[sample(nrow(Probit_data), replace = TRUE),]
  ###Estimate new Probit model using boot_sample data
  ### Note: to save time from optimization, I utilize R glm package which is faster
  boot_Probit <- glm(ydum ~ .,data = boot_sample, family = binomial(link="probit"))
  boot_Logit <- glm(ydum ~ .,data = boot_sample, family = binomial(link="logit"))
  ###get coefficient
  boot_Probit_coeff <- as.matrix(coef(summary(boot_Probit))[, "Estimate"])
  boot_Logit_coeff <- as.matrix(coef(summary(boot_Logit))[, "Estimate"])
  ###get pdf
  boot_Probit_pdf <- mean(dnorm(predict(boot_Probit)))
  boot_Logit_pdf <- mean(dlogis(predict(boot_Logit)))
  ###retrieve ME
  boot_Probit_ME <- boot_Probit_coeff*boot_Probit_pdf
  boot_Logit_ME <- boot_Logit_coeff*boot_Logit_pdf
  ###keep ME in Probit_ME_boot, and Logit_ME_boot
  Probit_ME_boot <- rbind(Probit_ME_boot, t(boot_Probit_ME))
  Logit_ME_boot <- rbind(Probit_ME_boot, t(boot_Probit_ME))
}
###calculate SE by finding SD for each column
Probit_SE_boot<- apply(Probit_ME_boot, 2, sd)
Logit_SE_boot<- apply(Logit_ME_boot, 2, sd)

#Table for SD report
SD_report <- cbind(t(Probit_SE_delta), Probit_SE_boot, t(Logit_SE_delta), Logit_SE_boot)
SD_report <- SD_report[2:nrow(SD_report),]
colnames(SD_report) <- c("Probit SD Delta", "Probit SD Bootstrap", "Logit SD Delta", "Logit SD Bootstrap")

#keep only using data, function and results
keep(OLS_data, Probit_data, Y_X1_Corr, OLS_coeff_table, OLS_SE_table, Probit_coeff_steep, probitLik, Probit_optim_coeff, negprobitLik, Logit_optim_coeff, neglogitLik, LP_optim_coeff, LPLik, Probit_R_package, Logit_R_package, LP_R_package, Probit_coeff_compare, Logit_coeff_compare, LP_coeff_compare, ME_report, SD_report, Probit_SE_boot,Logit_SE_boot, Probit_SE_delta, Probit_var, Logit_var, Logit_SE_delta, sure = TRUE)



# List of objects which are the answer ------------------------------------

## Exercise 1: OLS_data and Probit_data

## Exercise 2 Correlation Y, X1: Y_X1_Corr
## Exercise 2 OLS Coefficient: OLS_coeff_table
## Exercise 2 OLS SE: OLS_SE_table

## Exercise 3 function that return probit likelihood: probitLik
## Exercise 3 steepest ascent result: Probit_coeff_steep

## Exercise 4 probit, logit, linear probability model: Probit_coeff_compare, Logit_coeff_compare, LP_R_package

## Exercise 5 Marginal effect: ME_report
## Exercise 5 SD: SD_report