
################################ ECON 613 HW 3 ################################ 
# Author : Promvarat Pradit
# The list of result objects for each question is listed at the end of this document.

#Set working directory
setwd("C:/Users/promv/Dropbox/Master Course work/Spring 2019/Econ 613 Applied econometric micro/Assignment 3")
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
#install.packages("dummies")
library(dummies)


set.seed(12345)

# Exercise 1 Data Description ------------------------------------------------
# Load data
data(margarine)
product <- margarine$choicePrice
demos <- margarine$demos


# Exercise 1.1 Average and Dispersion in Product Characteristics
## Generate descriptive statistic
descStat_product <- basicStats(product[,3:ncol(product)])
## Subsetting the whole descriptive statistic table
descStat_product <- descStat_product[c("Mean","Median", "Minimum","Maximum", "Stdev", "Variance"),]


# Exercise 1.2 Market share and market share by product characteristic
## Generate market share by products
### group data by choice and count number of observation for each choice then divide by total observation to get market share
mktShare_product <- product %>% group_by(choice) %>% summarize(count = n()) %>% mutate(MarketShare = count*100/sum(count))
mktShare_product <- mktShare_product[,2:3]
### assign names
prod_name <- colnames(product)[3:ncol(product)]
row.names(mktShare_product) <- prod_name

## Generate market share by type (tub vs stick)
### create group by product type
prod_type <- prod_name
prod_type[endsWith(prod_name,"Stk")] = "Stick"
prod_type[endsWith(prod_name,"Tub")] = "Tub"
### group data by product type and sum market share
mktShare_type <- cbind(mktShare_product,prod_type)
mktShare_type <- mktShare_type %>% group_by(prod_type) %>% summarize(count = sum(count), MarketShare = sum(MarketShare))

## Generate market share by type (tub vs stick)
### create group by brand
prod_brand <- prod_name
prod_brand[startsWith(prod_name,"PPk")] = "PPk"
prod_brand[startsWith(prod_name,"PBB")] = "PBB"
prod_brand[startsWith(prod_name,"PFl")] = "PFl"
prod_brand[startsWith(prod_name,"PHse")] = "PHse"
prod_brand[startsWith(prod_name,"PGen")] = "PGen"
prod_brand[startsWith(prod_name,"PImp")] = "PImp"
prod_brand[startsWith(prod_name,"PSS")] = "PSS"
### group data by brand and sum market share
mktShare_brand <- cbind(mktShare_product,prod_brand)
mktShare_brand <- mktShare_brand %>% group_by(prod_brand) %>% summarize(count = sum(count), MarketShare = sum(MarketShare))


# Exercise 1.3 Mapping between observed attributes and choice
## merge data by hhid
mainData <- merge(product, demos, by = "hhid", all.x= TRUE)
## check if all hhid is found (no NA in income column)
any(is.na(mainData$Income))

## create market share for each product by ...
### Income
### group data by Income group and find market share within the same product
mktShare_map_inc <- mainData %>% group_by(Income,choice) %>% summarize(count = n()) %>% group_by(choice) %>% mutate(sumchoice = sum(count)) %>% mutate(MarketShare = count*100/sumchoice)
mktShare_map_inc <- mktShare_map_inc[,c(1,2,5)]
#### reshape from long to wide
library(reshape2)
mktShare_map_inc <- dcast(mktShare_map_inc, Income ~ choice)
colnames(mktShare_map_inc) <- c("Income",prod_name)

### Family Size| 3 groups : <=2 , 3-4 , >=5
#### create family size tag
FamilySize <- rep("family size <= 2", nrow(mainData))
FamilySize[mainData$Fs3_4==1] <- "family size <= 3-4"
FamilySize[mainData$Fs5.==1] <- "family size >= 5"
#### group data by family size and find market share within the same product
mktShare_map_famSize <- mainData %>% cbind(FamilySize) %>% group_by(FamilySize,choice) %>% summarize(count = n()) %>% group_by(choice) %>% mutate(sumchoice = sum(count)) %>% mutate(MarketShare = count*100/sumchoice)
mktShare_map_famSize <- mktShare_map_famSize[,c(1,2,5)]
#### reshape from long to wide
mktShare_map_famSize <- dcast(mktShare_map_famSize, FamilySize ~ choice)
colnames(mktShare_map_famSize) <- c("Family Size",prod_name)

### Education status| 2 groups : college vs non-college 
#### group data by college status and find market share within the same product
mktShare_map_college <- mainData %>% group_by(college,choice) %>% summarize(count = n()) %>% group_by(choice) %>% mutate(sumchoice = sum(count)) %>% mutate(MarketShare = count*100/sumchoice)
mktShare_map_college <- mktShare_map_college[,c(1,2,5)]
#### reshape from long to wide
mktShare_map_college <- dcast(mktShare_map_college, college ~ choice)
row.names(mktShare_map_college) <- c("non-college", "college")
mktShare_map_college <- mktShare_map_college[,2:ncol(mktShare_map_college)]
colnames(mktShare_map_college) <- prod_name

### Job status| 2 groups : white collar vs blue collar
#### group data by job status and find market share within the same product
mktShare_map_job <- mainData %>% group_by(whtcollar,choice) %>% summarize(count = n()) %>% group_by(choice) %>% mutate(sumchoice = sum(count)) %>% mutate(MarketShare = count*100/sumchoice)
mktShare_map_job <- mktShare_map_job[,c(1,2,5)]
#### reshape from long to wide
mktShare_map_job <- dcast(mktShare_map_job, whtcollar ~ choice)
row.names(mktShare_map_job) <- c("blue collar", "white collar")
mktShare_map_job <- mktShare_map_job[,2:ncol(mktShare_map_job)]
colnames(mktShare_map_job) <- prod_name

### Retirement status| 2 groups : retired vs not-retired
#### group data by retirement status and find market share within the same product
mktShare_map_retire <- mainData %>% group_by(retired,choice) %>% summarize(count = n()) %>% group_by(choice) %>% mutate(sumchoice = sum(count)) %>% mutate(MarketShare = count*100/sumchoice)
mktShare_map_retire <- mktShare_map_retire[,c(1,2,5)]
#### reshape from long to wide
mktShare_map_retire <- dcast(mktShare_map_retire, retired ~ choice)
row.names(mktShare_map_retire) <- c("not retired", "retiredr")
mktShare_map_retire <- mktShare_map_retire[,2:ncol(mktShare_map_retire)]
colnames(mktShare_map_retire) <- prod_name



# Exercise 2 First Model ------------------------------------------------
# A function that repeat row or column of a matrix
## credit : TszKin Julian, R blogger
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# prepare independent variable for conditional logit model 
## (X_ij - X_i1) cbind with matrix of 1 which is for alpha_j
X_ij <- as.matrix(mainData[,3:12] - mainData[,3])
## create indicator matrix
Indicator <- dummy("choice", data = mainData, sep="_")

# Function that return negative conditional logit log likelihood
conditonalLogitLikelihood <- function(B_input) {
  # extract beta and alpha
  B <- B_input[1]
  alpha <- as.matrix(B_input[2:11])
  # force alpha_1 = 0
  alpha[1] <- 0
  # calculate XB
  XB <- X_ij*B
  # calculate exp(XB+alpha)
  eXBA <- exp(XB + ( matrix(1,nrow = nrow(XB),ncol = 1) %*% t(alpha)))
  # calculate Pr_ij matrix
  Pr_ij <- t(apply(eXBA, 1, function(x) x / sum(x)))
  # Keep only the chosen choice
  Pr_ij <- Pr_ij*Indicator
  # calculate log-likelihood
  loglikelihood <- sum(log(rowSums(Pr_ij)))
  # return negative loglikelihood to be minimized later
  return(-loglikelihood)
}

# initialize B
## first element is beta, 2-11 elements are alpha_1 to alpha_10
B <- rep(0,11)

# Optimize
ConditionalLogit <- nlm(conditonalLogitLikelihood,B)

# create report table
ex2_table <- data.frame(ConditionalLogit$estimate[c(1,3:11)])
rownames(ex2_table) <- c("price", "alpha_2", "alpha_3", "alpha_4", "alpha_5", 
                         "alpha_6", "alpha_7", "alpha_8", "alpha_9", "alpha_10")
colnames(ex2_table) <- "Conditional Logit Point Esimates"



# Exercise 3 Second Model ------------------------------------------------
# prepare independent variable for multinomial logit model 
## (X_i) obtain household characteristic and add an column of 1 (for constant)
X_i <- as.matrix(cbind(mainData[,c(13,14,15,17,18,19)],matrix(1,nrow = nrow(mainData),ncol = 1)))
## create indicator matrix
Indicator <- dummy("choice", data = mainData, sep="_")

# Function that return negative multinomial logit log likelihood
multinomialLogitLikelihood <- function(B_input) {
  #reconstruct Beta matrix
  B <- matrix(B_input, nrow = 7, byrow = TRUE)
  #force beta_1 = 0
  B[,1] <- 0
  # calculate exp(X_iB_j)
  eXB <- exp(X_i %*% B)
  # calculate Pr_ij matrix
  Pr_ij <- t(apply(eXB, 1, function(x) x / sum(x)))
  # Keep only the chosen choice
  Pr_ij <- Pr_ij*Indicator
  # calculate log-likelihood
  loglikelihood <- sum(log(rowSums(Pr_ij)))
  # return negative loglikelihood to be minimized later
  return(-loglikelihood)
  }

# initialize B
B <- rep(0,70)

# Optimize
MultinomialLogit <- nlm(multinomialLogitLikelihood,B)

# create report table
ex3_table <- matrix(MultinomialLogit$estimate, nrow = 7, byrow = TRUE)
row.names(ex3_table) <- c("Income", "Familysize3-4","FamilySize>=5","College","White Collar", "Retired", "Intercept")
colnames(ex3_table) <- c("Choice1","Choice2","Choice3","Choice4","Choice5","Choice6","Choice7","Choice8","Choice9","Choice10")

# Crosscheck with multinomial logit package : the coefficient is similar
testdf <- data.frame(cbind(mainData$choice,X_i[,1:6]))
library(nnet)
mulLogit_test <- multinom(V1 ~ ., testdf)
mulLogit_test
t(ex3_table)



# Exercise 4 Marginal Effect ------------------------------------------------
# Marginal Effect for Conditional logit model
## 1) Calculate Probability of household i choosing choice j matrix, Pr_ij (n by 10)
### 1.1) create matrix X_ij
X_ij <- as.matrix(mainData[,3:12])
### 1.2) get beta
beta <- ex2_table[1,1]
### 1.3) calculate XB
XB <- X_ij* beta
### 1.4) get alpha matrix 
alpha <- c(0,ex2_table[2:10,1])
### 1.5) create XB+alpha and exponential it, e(XB+A)
eXBA <- exp(XB + ( matrix(1,nrow = nrow(XB),ncol = 1) %*% t(alpha)))
### 1.6) get Pr_ij matrix (each element divide by its rowsum)
Pr_ij <- t(apply(eXBA, 1, function(x) x / sum(x)))
## 2) Compute B(sum(Pr_ij))(1[j=k])
### 2.1) calculate B(sum(Pr_ij))
BPr_j <- beta * colSums(Pr_ij) 
### 2.2) transform this into diagonal matrix
BPr_j_diag <- rep.row(BPr_j,n=10) * diag(10)
## 3) Compute B(Pr_ik)(Pr_ij) (10 by 10)
BPr_kj <- beta * (t(Pr_ij) %*% Pr_ij)
## 4) Compute B(Pr_ij)(1[j=k])-B(Pr_ik)(Pr_ij) and divide be number of household (find average ME)
ME_conditionalLogit <- (BPr_j_diag - BPr_kj)/nrow(X_ij)
colnames(ME_conditionalLogit) <- c("d_Pr_i1","d_Pr_i2","d_Pr_i3","d_Pr_i4","d_Pr_i5",
                                    "d_Pr_i6","d_Pr_i7","d_Pr_i8","d_Pr_i9","d_Pr_i10")
row.names(ME_conditionalLogit) <- c("d_price_i1","d_price_i2","d_price_i3","d_price_i4","d_price_i5",
                                    "d_price_i6","d_price_i7","d_price_i8","d_price_i9","d_price_i10")



# Marginal Effect for Multinomial logit model
## 1) Calculate Probability of household i choosing choice j matrix, Pr_ij (n by 10)
### 1.1) get X_i
X_i <- as.matrix(cbind(mainData[,c(13,14,15,17,18,19)],matrix(1,nrow = nrow(mainData),ncol = 1)))
### 1.2) calculate exp(X_iB_j) matrix
eXB <- exp(X_i%*%ex3_table)
### 1.3) get Pr_ij matrix (each element divide by its rowsum)
Pr_ij <- t(apply(eXB, 1, function(x) x / sum(x)))
### 2) get B_j for income and replicate it n times to create (n by 10) matrix
Brep <-  rep.row(ex3_table[1,], n= nrow(Pr_ij))
### 3) compute B_bar for each household and each regressor 
B_bar = Pr_ij %*% t(ex3_table)
### 4) get B_bar for family income for each household and replicated it into 10 column
B_bar_inc <- B_bar[,1] %>% rep.col(10)
### 5) compute B_j-B_bar for family income
dB <- Brep-B_bar_inc
### 6) Calculate Pr_ij(B_j - B_bar) for Family Income
Pr_BjBbar <- Pr_ij*dB
### 7) find average
ME_multinomialLogit <- data.frame(t(apply(Pr_BjBbar, 2, mean)))
row.names(ME_multinomialLogit) <- c("ME of family income")
colnames(ME_multinomialLogit) <- c("d_Pr_i1","d_Pr_i2","d_Pr_i3","d_Pr_i4","d_Pr_i5",
                                   "d_Pr_i6","d_Pr_i7","d_Pr_i8","d_Pr_i9","d_Pr_i10")



# Exercise 5 Mixed Logit & IIA ------------------------------------------------
#Mixed Logit with full dataset
# prepare independent variable for conditional logit part 
X_ij <- as.matrix(mainData[,3:12] - mainData[,3])
# prepare independent variable for multinomial logit part 
W_i <- as.matrix(cbind(mainData[,c(13,14,15,17,18,19)],matrix(1,nrow = nrow(mainData),ncol = 1)))
## create indicator matrix
Indicator <- dummy("choice", data = mainData, sep="_")

# Function that return negative mixed logit log likelihood
mixedLogitLikelihood <- function(B_input) {
  # get beta
  beta <- B_input[1]
  # reconstruct gamma matrix
  gamma <- matrix(B_input[2:length(B_input)], ncol = ncol(X_ij), byrow = TRUE)
  # force gamma for choice 1 = 0
  gamma[,1] <- 0
  # calculate XB
  XB <- X_ij*beta
  # calculate WG
  WG <- W_i%*%gamma
  # calculate exp(XB+WG)
  eXBWG <- exp(XB+WG)
  # calculate Pr_ij matrix
  Pr_ij <- t(apply(eXBWG, 1, function(x) x / sum(x)))
  # Keep only the chosen choice
  Pr_ij <- Pr_ij*Indicator
  # calculate log-likelihood
  loglikelihood <- sum(log(rowSums(Pr_ij)))
  # return negative loglikelihood to be minimized later
  return(-loglikelihood)
}

# initialize B
B <- rep(0,71)

# Optimize
MixedLogit_f <- nlm(mixedLogitLikelihood,B)
# Keep likelihood
Likelihood_f <- -mixedLogitLikelihood(MixedLogit_f$estimate)

# create report table
ex5f_table <- rbind(matrix(MixedLogit_f$estimate[1],nrow = 1,ncol=10),matrix(MixedLogit_f$estimate[2:71], nrow = 7, byrow = TRUE))
row.names(ex5f_table) <- c("Price","Income", "Familysize3-4","FamilySize>=5","College","White Collar", "Retired", "Intercept")
colnames(ex5f_table) <- c("Choice1","Choice2","Choice3","Choice4","Choice5","Choice6","Choice7","Choice8","Choice9","Choice10")


# Now we try alternative specification where we remove data from one choice
# Here I remove choice 1, the largest group
altData <- mainData[mainData$choice != 1,] %>% subset(select = -c(PPk_Stk))
# then I redo mixed logit optimization
# prepare independent variable for conditional logit part 
X_ij <- as.matrix(altData[,3:11] - altData[,3])
# prepare independent variable for multinomial logit part 
W_i <- as.matrix(cbind(altData[,c(12,13,14,16,17,18)],matrix(1,nrow = nrow(altData),ncol = 1)))
# create indicator matrix
Indicator <- dummy("choice", data = altData, sep="_")
# Initialize beta_r
B <- rep(0,64)
# Optimize mixed logit
MixedLogit_r <- nlm(mixedLogitLikelihood,B)
# Keep likelihood
Likelihood_r <- -mixedLogitLikelihood(MixedLogit_r$estimate)
# create report table
ex5r_table <- rbind(matrix(MixedLogit_r$estimate[1],nrow = 1,ncol=9),matrix(MixedLogit_r$estimate[2:64], nrow = 7, byrow = TRUE))
row.names(ex5r_table) <- c("Price","Income", "Familysize3-4","FamilySize>=5","College","White Collar", "Retired", "Intercept")
colnames(ex5r_table) <- c("Choice2","Choice3","Choice4","Choice5","Choice6","Choice7","Choice8","Choice9","Choice10")

# calculate MCFadden, Train, and Tye Test (MTT test)
MTT_value <- -2*(Likelihood_f-Likelihood_r)
MTT_pval <- pchisq(MTT_value, df=56, lower.tail = FALSE)
MTT_table <- data.frame(c(Likelihood_f,Likelihood_r,MTT_value,MTT_pval))
row.names(MTT_table) <- c("LL_full model", "LL_restricted model", "Test statistic", "p-val")
colnames(MTT_table) <- "MTT Test"

# List of objects which are the answer ------------------------------------

## Exercise 1.1 Average and dispersion in product characteristics: descStat_product
## Exercise 1.2 Market share: mktShare_product, mktShare_type, mktShare_brand 
## Exercise 1.3 Mapping: mktShare_map_inc, mktShare_map_famSize, mktShare_map_college, mktShare_map_job, mktShare_map_retire

## Exercise 2 Conditional Logit: conditonalLogitLikelihood, ex2_table

## Exercise 3 multinomial Logit: multinomialLogitLikelihood, ex3_table

## Exercise 4 Marginal Effects: ME_conditionalLogit . ME_multinomialLogit

## Exercise 5 IIA: MixedLogit_f, ex5f_table, MixedLogit_r, ex5r_table, MTT_table