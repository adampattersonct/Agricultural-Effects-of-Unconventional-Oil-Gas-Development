
###############################################################
## This script uses ML to classify Unknown and Directionally ##
## drilled wells into Vertically or Horizontally drilled     ##
###############################################################

# Author : Adam Patterson

# Clear Global Environment 
rm(list=ls())

#### Load relevant packages
library(randomForest)

# Set WD load data
setwd("M:/project/adam/paper 2/input/wells")
data<-read.csv("Well_Production_Data.csv")
data.a<-data

# Subset data into columns with 99 percent or more of non-NA values. Random Forest does not like NA values
data<-data[, which(colMeans(!is.na(data)) == 1)]
data$long<-data.a$Surface.Hole.Longitude..WGS84.
data$lat<- data.a$Surface.Hole.Latitude..WGS84.

# Convert Factors to Characters
i <- sapply(data, is.factor)
data[i] <- lapply(data[i], as.character)

# Separate Horizontal and Vertical Drilled Wells, Merge to create Training Dataset for our classifier 
horz<-data[data$Drill.Type == "H",]
vert<-data[data$Drill.Type == "V",]
train<-rbind(horz,vert)

# Separate Directional and Unidentified Drilled Wells, Merge to create Test Dataset for our prediction 
direct<-data[data$Drill.Type == "D",]
unident<-data[data$Drill.Type == "U",]
test<-rbind(direct,unident)

# Classify response variable as factor for classifier 
train$Drill.Type<-as.factor(train$Drill.Type)
str(train)

train$Drill.Type<-as.factor(train$Drill.Type)
test$Drill.Type<-as.factor(test$Drill.Type)

# Set seed for reproducible research within the random forest
set.seed(1)

# Train the Random Forest algorithm with Vertical and Horizontal wells to deploy on directional and unidentified wells
rf<-randomForest(Drill.Type~.,data=train,
                 ntree=100,mtry=12,nodesize=10,na.action=na.omit, importance=TRUE)

# Use Random Forest classifier to predict new data from training test set
prediction<-predict(rf,train.test)

# Use Random Forest classifier to predict drill type on new data (the unidentified and directionally drilled data)
prediction_test<-predict(rf,test)

# Create dataset with all original variables to input predicted drill type 
direct1<-data.a[data.a$Drill.Type == "D",]
unident1<-data.a[data.a$Drill.Type == "U",]
test1<-rbind(direct1,unident1)
test1$Drill.Type<-as.character(prediction_test)

# Subset observed horizontally drilled wells and classified horizontally drilled wells. 
horz1<-test1[test1$Drill.Type == "H",]
horz_true<-data.a[data.a$Drill.Type == "H",]

# Create final data
data_final<-rbind(horz_true,horz1)

# Write final data
write.csv(data_final,"Horizontal_Wells.csv")