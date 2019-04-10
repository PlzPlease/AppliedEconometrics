
################################ ECON 613 HW 4 ################################ 
# Author : Promvarat Pradit
# The list of result objects for each question is listed at the end of this document.

#Set working directory
setwd("C:/Users/promv/Dropbox/Master Course work/Spring 2019/Econ 613 Applied econometric micro/Assignment 4")
#Clear workspcae
rm(list = ls())

# # Time code runtime
# ptm <- proc.time()

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
#install.packages("bayesm")
library(bayesm)
#install.packages("mclogit")
library(mclogit)
#install.packages("fBasics")
library(fBasics)
#install.packages("reshape")
library(reshape)
#install.packages("plm")
library(plm)
library(readr)
require(data.table)

set.seed(12345)


# Exercise 1 Data ------------------------------------------------
# Load data
Koop_Tobias <- read_csv("Koop-Tobias.csv")
# Create Working data frame
mainDF <- Koop_Tobias[,c("PERSONID","TIMETRND","LOGWAGE","EDUC","POTEXPER")]

# Randomly select 5 individuals
represent_t <- Koop_Tobias[Koop_Tobias$PERSONID %in% sample(unique(Koop_Tobias$PERSONID),5),c("PERSONID","LOGWAGE","TIMETRND")]

# Reshape into wide
represent_t <- melt(represent_t, id.vars = c("PERSONID","TIMETRND")) %>% cast(PERSONID ~ TIMETRND)
represent <- represent_t[,-1]
rownames(represent) <- represent_t[,1]
remove(represent_t)


# Exercise 2 Random Effects ------------------------------------------------
# use FGLS to estimate Random effect model

# First step: estimate pooled OLS to find lambda
## estimate pooled OLS (did not include time trend)
Pooled_OLS <- lm(LOGWAGE ~ ., mainDF[,-2])
## get var(a_i) and var(e_it)
var_alpha <- vcov(Pooled_OLS)[2,2]
var_error <- var(Pooled_OLS$residuals)
## calculate T
T <- length(unique(mainDF$TIMETRND))
## calculate lambda = 1 - sqrt( var(e) / (var(e) + T*var(a)) )
lambda <- 1 - sqrt(var_error / (var_error + (T*var_alpha)))

# Second step: transform the data and estimate beta
## transform data to be X_it - lambda*X_bar_i
RE_transform_DF <- mainDF[,-2] %>% group_by(PERSONID) %>%  mutate(LOGWAGE = LOGWAGE-(lambda*mean(LOGWAGE)),EDUC = EDUC-(lambda*mean(EDUC)),POTEXPER = POTEXPER-(lambda*mean(POTEXPER))) 
RE_transform_DF <- RE_transform_DF[,-1]
## estimate RE using OLS on transformed data
RE_model <- lm(LOGWAGE ~ ., data = RE_transform_DF)

# Crosscheck with coefficient from package
RE_plm <- plm(LOGWAGE ~ EDUC + POTEXPER, data = mainDF, model = "random")

# Generate Report Table
RE_table <- t(as.data.frame(RE_model$coefficients))
rownames(RE_table) <- "Estimated Coefficient"


# Exercise 3 Fixed Effects Model -------------------------------------------

# Between Estimator
## Transform data to be averaged by PERSONID, i. And remove duplicates
FE_between_DF <- mainDF[,-2] %>% group_by(PERSONID) %>%  mutate(LOGWAGE = mean(LOGWAGE),EDUC = mean(EDUC),POTEXPER = mean(POTEXPER)) %>% distinct()
## drop PERSONID
FE_between_DF <- FE_between_DF[,-1]
## run OLS on the data
FE_between_model <- lm(LOGWAGE ~ ., data = FE_between_DF)
## Crosscheck with plm package
FE_between_plm <- plm(LOGWAGE ~ EDUC + POTEXPER, data =  mainDF, model = "between")


# Within Estimator
## Transform data to be x_it - x_bar_i
FE_within_DF <- mainDF[,-2] %>% group_by(PERSONID) %>%  mutate(LOGWAGE =LOGWAGE - mean(LOGWAGE),EDUC = EDUC - mean(EDUC),POTEXPER = POTEXPER - mean(POTEXPER))
## drop PERSONID
FE_within_DF <- FE_within_DF[,-1]
## run OLS on the data
FE_within_model <- lm(LOGWAGE ~ ., data = FE_within_DF)
## Crosscheck with plm package
FE_within_plm <- plm(LOGWAGE ~ EDUC + POTEXPER, data =  mainDF, model = "within")


# First time difference Estimator
## Transform data to be x_it - x_i{t-1}
FE_FD_DF <- as.data.table(mainDF[order(mainDF$PERSONID,mainDF$TIMETRND),-2]) %>% group_by(PERSONID) %>% mutate(LOGWAGE = LOGWAGE - shift(LOGWAGE),EDUC = EDUC - shift(EDUC), POTEXPER = POTEXPER - shift(POTEXPER)) %>% as.data.frame()
## drop PERSONID
FE_FD_DF <- FE_FD_DF[,-1]
## run OLS on the data
FE_FD_model <- lm(LOGWAGE ~ ., data = FE_FD_DF)
## Crosscheck with plm package
FE_FD_plm <- plm(LOGWAGE ~ EDUC + POTEXPER, data =  mainDF, model = "fd")


# Generate Report Table
FE_table <- rbind(FE_between_model$coefficients,FE_within_model$coefficients, FE_FD_model$coefficients)
rownames(FE_table) <- c("Between Estimates", "Within Estimates", "First Time Difference Estimates")



# Exercise 4 Understanding Fixed Effects ----------------------------------
# Create data of those 100 randomly selected
ex4DF <- Koop_Tobias[Koop_Tobias$PERSONID %in% sample(unique(Koop_Tobias$PERSONID),100),]
## create dummy variables for PERSONID
id_f <- factor(ex4DF$PERSONID)
id_dummy <- model.matrix(~id_f)
ex4_FE_DF <- cbind(ex4DF[,c("LOGWAGE", "EDUC", "POTEXPER")], id_dummy)
## Prepare X and Y matrix
X <- as.matrix(ex4_FE_DF[,2:ncol(ex4_FE_DF)])
Y <- as.matrix(ex4_FE_DF[,1])
  
# Function that return loss = sum square error
loss_function <- function(B) {
  Y_hat <- X %*% B
  loss <- sum((Y-Y_hat)^2)
}

# initialize B
B_ini <- matrix(0,nrow = 102, ncol = 1)

# Minimize loss
ex4_FE_model <- nlm(loss_function,B_ini)

# Generate Coefficient table
ex4_FE_coeff <- as.matrix(ex4_FE_model$estimate)
row.names(ex4_FE_coeff) <- colnames(ex4_FE_DF[,2:ncol(ex4_FE_DF)])

# Retrieve alpha_i
ex4_alpha <- id_dummy %*% as.matrix(ex4_FE_coeff[3:nrow(ex4_FE_coeff)])

# Create new dataset with alpha and time invariants variable (+ remove duplicates)
ex4_alpha_DF <- cbind(ex4_alpha, ex4DF[,6:10]) %>% distinct()
# regress alpha on invariant variables
ex4_alpha_model <- lm(ex4_alpha ~ ., data = ex4_alpha_DF)
# summarise to see how much these time invariant variables can explain alpha
summary(ex4_alpha_model)


# List of objects which are the answer ------------------------------------

## Exercise 1 represent
## Exercise 2 RE_table
## Exercise 3 FE_table
## Exercise 4 summary(ex4_alpha_model)
  