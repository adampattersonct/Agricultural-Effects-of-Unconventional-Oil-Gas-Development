#########################################################################################################
### This script serves to interpolate the missing Spud and Completion years amongst well observations ###
#########################################################################################################

# Author : Adam Patterson

# Clear Global Environment 
rm(list=ls())
set.seed(1)
#### Load relevant packages
library(randomForest)

# Set WD and load data
setwd("M:/project/adam/paper 2/input/wells")
data<-read.csv("Horizontal_Wells.csv", header = TRUE)
data.a<-data

# Subset data into columns with 99 percent or more of non-NA values. Random Forest does not like NA values
data<-data[, which(colMeans(!is.na(data)) > .99)]

# Create Spud year variable #
data$Spud_Year<-substr(data$Spud.Date,1,4)
data$Spud.Date<-NULL
data$Spud_Year<-as.numeric(data$Spud_Year)

# Create Test Set
spud_na<-data[which(is.na(data$Spud_Year)),]

# Create Training Set
spud<-data[-which(is.na(data$Spud_Year)),]

# Train the Random Forest algorithm with Spud Year wells to deploy on missing Spud Years
rf<-randomForest(Spud_Year~.,data=spud,
                 ntree=100,mtry=12,nodesize=10,na.action=na.omit, importance=TRUE)

## The mean residual squared error results in too high of an error to use this method. I will use KNN imputation for interpolation of Spud year
rf

