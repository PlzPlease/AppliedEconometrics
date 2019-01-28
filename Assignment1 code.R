
################################ ECON 613 HW 1 ################################ 
#Author: Promvarat Pradit
#Net ID: PP164

#Set working directory
setwd("C:/Users/promv/Dropbox/Master Course work/Spring 2019/Econ 613 Applied econometric micro/Assignment 1")
#Clear workspcae
rm(list = ls())

# Data Import and preperation ---------------------------------------------
#Load required library
#For csv import
library(readr)
library(dplyr)
library(stringr)
#install.packages("psych")
library(psych)
#install.packages("gdata")
library(gdata)
#require(data.table)
library(data.table)
#install.packages("future")
library(future)

#Importing data
#Import student data: datstu
###Forcing score and rankplace to be integer
datstu <- read_csv("datstu.csv", col_types = cols(rankplace = col_integer(), score = col_integer()))
colnames(datstu)[colnames(datstu)=="X1"] <- "stuIndex"

#Import senior school data: datsss
##Change first column's name to senior school index
datsss <- read_csv("datsss.csv")
colnames(datsss)[colnames(datsss)=="X1"] <- "sssIndex"

#Import junior school data: datjss
##Change first column's name to junior school index
datjss <- read_csv("datjss.csv")
colnames(datjss)[colnames(datjss)=="X1"] <- "jssIndex"



# Exercise 1 Missing data --------------------------------------------------------------
#Creating an empty vector to store results
result <- numeric()


###### Q1 Report number of student
#View(datstu)
#The only way to identify student identity is to treat each entry in datstu as one student
numStu <- describeData(datstu[,"stuIndex"])
result <- rbind(result, numStu[[1]])


###### Q2 Report number of school
#View(datsss)
summary(duplicated(datsss$schoolcode))
#datsss shows many duplicated and NA
#1 clean out all rows that does not have complete data
datsssCleaned <- datsss[!(is.na(datsss$schoolname)
                   |is.na(datsss$sssdistrict)
                   |is.na(datsss$ssslong)
                   |is.na(datsss$ssslat)),]
#2 clean out all duplicate school code base on the order of name length
datsssCleaned$nameLength <-  str_count(datsssCleaned$schoolname)
datsssCleaned <- datsssCleaned[order(datsssCleaned$schoolcode,datsssCleaned$nameLength),]
datsssCleaned <- datsssCleaned[!duplicated(datsssCleaned$schoolcode),]
summary(duplicated(datsssCleaned$schoolcode))
datsss <- datsssCleaned[,2:6]
#get result
numSchool <- describeData(datsssCleaned[,1])
result <- rbind(result, numSchool[[1]])


###### Q3 Report number of programs
##get program from each of the six choice, and rowbind them
Allprograms <- c()
for (i in 11:16) {
  Allprograms <- rbind(Allprograms, setNames(datstu[,i], "program name"))
}
Allprograms <- unique(Allprograms)
#drop NA
Allprograms <- na.omit(Allprograms)
#get result
numProgram <- describeData(Allprograms)
result <- rbind(result, numProgram[[1]])


###### Q4 Report number of choice
##idea : For all 6 choices, count every cell that 'both' school and program is not NA
#create a vector of one that has row eaual to datstu
one <- rep.int(1,nrow(datstu))
totalChoice <- 0
for (i in 1:6) {
   #make a vector consist of 1 for every row that both school choice and program choice is not NA
   schoolAndProgramNotNA <- one[!is.na(datstu[,4+i]) & !is.na(datstu[,10+i])]
   #sum value and accumulating through 6 choices
   totalChoice <- totalChoice + sum(schoolAndProgramNotNA)
}
result <- rbind(result, totalChoice)


###### Q5 Report number of student missing test score
##extract entry with missing test score
noScore <- datstu[is.na(datstu$score),]
numNoScore <- describeData(noScore[,"stuIndex"])
result <- rbind(result, numNoScore[[1]])


###### Q6 Report number of student apply to only one school
#school choice 1 must not be missing and all other subsequent choice must be either the same school or NA
oneSchool <- datstu[!is.na(datstu$schoolcode1)
                    & (is.na(datstu$schoolcode2) | datstu$schoolcode1 == datstu$schoolcode2)
                    & (is.na(datstu$schoolcode3) | datstu$schoolcode1 == datstu$schoolcode3)
                    & (is.na(datstu$schoolcode4) | datstu$schoolcode1 == datstu$schoolcode4)
                    & (is.na(datstu$schoolcode5) | datstu$schoolcode1 == datstu$schoolcode5)
                    & (is.na(datstu$schoolcode6) | datstu$schoolcode1 == datstu$schoolcode6)
                    ,]
numOneSchool <- describeData(oneSchool[,"stuIndex"])
result <- rbind(result, numOneSchool[[1]])


###### Q7 Report number of student apply to less than 6 choices
#at least one school choice must be NA
less6 <- datstu[is.na(datstu$schoolcode1) | is.na(datstu$choicepgm1)
                | is.na(datstu$schoolcode2)  | is.na(datstu$choicepgm2)
                | is.na(datstu$schoolcode3)  | is.na(datstu$choicepgm3)
                | is.na(datstu$schoolcode4)  | is.na(datstu$choicepgm4)
                | is.na(datstu$schoolcode5)  | is.na(datstu$choicepgm5)
                | is.na(datstu$schoolcode6)  | is.na(datstu$choicepgm6)
                ,]
numLess6 <- describeData(less6[,"stuIndex"])
result <- rbind(result, numLess6[[1]])
row.names(result) <- c("Number of students", "Number of schools", "Number of programs",
                     "Number of choices", "Number of students missing test score",
                     "Number of students apply to the same school",
                     "Number of students apply to less that 6 choices")
View(result)


# Exercise 2 Data --------------------------------------------------------------
#Create a school level dataset, where each row corresponds to a (school,program)
#idea: keep school and choice according to rank place then merge with datsss then aggregate into school dataset

#clear unused objects
keep(datstu, datsss, datjss, result, datsssCleaned, sure = TRUE)

#Step 1: clean out students that have 99 and NA rankplace
datstuPlaced <- datstu[!is.na(datstu$rankplace) & datstu$rankplace!=99,]
#step 2: Create 2 columns containing the schoolcode and program that students were placed into
for (i in 1:nrow(datstuPlaced)) {
  rankplace <- as.numeric(datstuPlaced[i,"rankplace"])
  datstuPlaced[i,"schoolcode"] <- datstuPlaced[i,4+rankplace]
  datstuPlaced[i,"program"] <- datstuPlaced[i,10+rankplace]
}
#step 3: keep only "stuIndex", "score", "schoolcodeFinal" and "programFinal"
datstuPlaced <- datstuPlaced[,c("score","schoolcode","program")]
#step 4: merge with datsss that had been cleaned of missing data
schoolDataset <- merge(datstuPlaced,datsssCleaned, by = "schoolcode", all.x = TRUE)
#step 5: check if we have perfect merge, seems like we do
summary(is.na(schoolDataset))
#step 6: Aggregate students data into school data by schoolcode and program
size <- rep(1,nrow(schoolDataset))
schoolDataset <- data.table(schoolDataset)
schoolDataset <- schoolDataset[, list(quality=mean(score), cutoff=min(score), size = sum(size)), by=c("schoolcode","program", "schoolname", "sssdistrict","ssslong","ssslat")]
#schoolDataset <- as.data.frame(schoolDataset)


# Exercise 3 Distance -----------------------------------------------------
# Since in exercise 4, we need to group data by test score, I will calculate distance only for student with test score
#clear unused objects
keep(datstu, datsss, datjss, result, schoolDataset, sure = TRUE)

#step 1 keep only student with test score
datstuDist <- datstu[!is.na(datstu$score),]
#Step 1 Prepare jss and sss data, making it contain only merging variable and coordinate
##For jss
jssDist <- datjss[,2:4]
colnames(jssDist) <- c("jssdistrict","jsslong", "jsslat")
##For sss
sssDist <- schoolDataset[,c("schoolcode","ssslong", "ssslat")]
sssDist <-  schoolDataset[, list(ssslong=mean(ssslong), ssslat=mean(ssslat)), by=c("schoolcode")]

#step 2 merge jssDist and sssDist to each choice then calcualte distance
##For jss
datstuDist <- merge(datstuDist,jssDist, by = "jssdistrict", all.x = TRUE)
summary(is.na(datstuDist$jsslong))
#there is one entry that jssdistrict is recoreded with N/A so I droped it
datstuDist <- datstuDist[!is.na(datstuDist$jsslat),]
##For sss and distance calculation
for (i in 1:6) {
  datstuDist[,"schoolcode"] <- datstuDist[,5+i]
  datstuDist <- merge(datstuDist,sssDist, by = "schoolcode", all.x = TRUE)
  distance <- sqrt(((69.172*(datstuDist$ssslong-datstuDist$jsslong))*cos(datstuDist$jsslat/57.3))^2 + (69.172*(datstuDist$ssslat-datstuDist$jsslat))^2)
  datstuDist <- cbind(datstuDist,distance)
  datstuDist <- subset(datstuDist, select = -c(schoolcode, ssslong, ssslat))
}
datstuDist = rename(datstuDist, distance_1 = distance , distance_2 = distance.1,
                    distance_3 = distance.2, distance_4 = distance.3,
                    distance_5 = distance.4, distance_6 = distance.5)



# Exercise 4 Descriptive Characteristics ----------------------------------
#idea merge schoolDataset which has cutoff and quality to datstudist
#Note: to make the overall consistent with the quantile analysis, only student with test score will be analyzed in the overall case

#Step 1 prepare schoolDataset
schoolDatasetCQ <- schoolDataset[,c("schoolcode","program","quality","cutoff")]


#Step 2 keep only using variable in datstuDist
datstuDesc <- subset(datstuDist, select = -c(jssdistrict, stuIndex, jsslong, jsslat))

#Step 3 merge the prepared dataset to datstuDist for each choice > name datstuDesc
for (i in 1:6) {
  datstuDesc[,"schoolcode"] <- datstuDesc[,3+i]
  datstuDesc[,"program"] <- datstuDesc[,9+i]
  datstuDesc <- merge(datstuDesc,schoolDatasetCQ, by = c("schoolcode","program"),all.x = TRUE)
  qName <- paste("quality",i,sep="_")
  cName <- paste("cutoff",i,sep="_")
  assign(qName,datstuDesc[,25])
  assign(cName, datstuDesc[,26])
  datstuDesc <- subset(datstuDesc, select = -c(schoolcode,program, quality,cutoff))
}
quality <- as.data.frame(do.call("cbind",mget(ls(pattern = "^quality*"))))
cutoff <- as.data.frame(do.call("cbind",mget(ls(pattern = "^cutoff*"))))
datstuDesc <- cbind(datstuDesc,quality,cutoff)

#step 4 find descriptive stat for the whole samples
allDesc <- describe(datstuDesc[,17:ncol(datstuDesc)])
allDesc <- allDesc[,3:4]
colnames(allDesc) <- c("Overall: mean", "Overall: sd")

#step 5 differentiate student by test score quantiles
datstuDesc <- within(datstuDesc, quartile <- as.integer(cut(score, quantile(score), include.lowest=TRUE)))
quantileDesc <- rep(1,18)
for (q in 1:4) {
  desc <- describe(datstuDesc[datstuDesc$quartile==q,17:(ncol(datstuDesc)-1)])
  desc <- desc[,3:4]
  quantileDesc <- cbind(quantileDesc,desc)
}
quantileDesc <- quantileDesc[,2:ncol(quantileDesc)]
colnames(quantileDesc) <- c("Quantile 1:mean", "Quantile 1:sd",
                            "Quantile 2:mean", "Quantile 2:sd",
                            "Quantile 3:mean", "Quantile 3:sd",
                            "Quantile 4:mean", "Quantile 4:sd")
reportTable <- cbind(allDesc,quantileDesc)
View(reportTable)




# Exercise 5 Diversification ----------------------------------------------
#Note: to make the overall consistent with the quantile analysis, only student with test score will be analyzed in the overall case
#clear unused object
keep(datstu, datsss, datjss, result, schoolDataset, datstuDist, datstuDesc, reportTable, sure = TRUE)

#step 1 create school group (decile)
schoolDataset <- within(schoolDataset, schoolGroup <- as.integer(cut(cutoff, quantile(cutoff , probs = seq(0,1,by=0.1)), include.lowest=TRUE)))
schoolDatasetLean <- schoolDataset[,c("schoolcode","program","schoolGroup")]

#step 2 merge schoolGroup to each choice in student data datstuDesc
datstuSchGrp <- datstuDesc
for (i in 1:6) {
  datstuSchGrp[,"schoolcode"] <- datstuSchGrp[,3+i]
  datstuSchGrp[,"program"] <- datstuSchGrp[,9+i]
  datstuSchGrp <- merge(datstuSchGrp,schoolDatasetLean,by = c("schoolcode","program"),all.x = TRUE)
  dec <- paste("schoolGroup",i,sep="_")
  assign(dec,datstuSchGrp[,38])
  datstuSchGrp <- subset(datstuSchGrp, select = -c(schoolcode,program, schoolGroup))
}
schoolGroup <- as.data.frame(do.call("cbind",mget(ls(pattern = "^schoolGroup*"))))
datstuSchGrp <- cbind(datstuSchGrp,schoolGroup)
  
#step 3 compute each student # of school group applied to 
trpGrp <- as.data.frame(t(datstuSchGrp[,36:41]))
numGroup <- t(as.data.frame(lapply(trpGrp, n_distinct)))
datstuSchGrp <- cbind(datstuSchGrp, numGroup)

#step 4 report the figure (mean & sd) for whole sample
allNumGrp <- describe(datstuSchGrp[,"numGroup"])
allNumGrp <- allNumGrp[,c(3,4,5,8,9)]
rownames(allNumGrp) <- c("Overall")

#step 5 differentiate student by test score quantiles
quantileNumGrp <- rep(1,5)
for (q in 1:4) {
  qNumGrp <- describe(datstuSchGrp[datstuSchGrp$quartile==q,"numGroup"])
  qNumGrp <- qNumGrp[,c(3,4,5,8,9)]
  quantileNumGrp <- rbind(quantileNumGrp,qNumGrp)
}
quantileNumGrp <- quantileNumGrp[2:nrow(quantileNumGrp),]
rownames(quantileNumGrp) <- c("Quantile 1", "Quantile 2", "Quantile 3", "Quantile 4")
numGrpReportTable <- rbind(allNumGrp,quantileNumGrp)
View(numGrpReportTable)

keep(datstu, datsss, datjss, result, schoolDataset, datstuDist, datstuDesc, reportTable, datstuSchGrp,numGrpReportTable, sure = TRUE)

#Note1: the result table for exercise 1 is the object "result"
#Note2: the school level dataset create for exercise 2 is the dataframe "schoolDataset"
#Note3: student dataset with distance data foe exercise 3 is "datstuDist"
#Note4: student data with cutoff, quality and distance for each school choice is "datstuDesc" and the result table is "reportTable"
#Note5: student data with number of school group chosen is "datstuSchGrp" and the result table is "numGrpReportTable"
