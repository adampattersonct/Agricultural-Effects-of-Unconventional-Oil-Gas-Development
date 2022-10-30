#########################################################################################################
### This script serves to interpolate the missing Spud and Completion years amongst well observations ###
#########################################################################################################

# Author : Adam Patterson

# Clear Global Environment  #
rm(list=ls())

# Load relevant packages #
library(caret)

# Set WD #
setwd("M:/project/adam/paper 2/input/wells")

#################################
# Interpolate Horizontal Wells  #
#################################

# Load well production data #
data<-read.csv("Horizontal_Wells.csv", header = TRUE)
data_all<-data
safe<-data
safe$id<-1:nrow(safe)

##    Interpolate Spud Year    ##

#### Subset data into columns with few NAs ####
data1<-data[,which(colMeans(!is.na(data)) > .99)]
# Create spud year variable #
data1$Spud_Year<-substr(data1$Spud.Date,1,4)
data1$Spud_Year<-as.numeric(data1$Spud_Year)

#### Eliminate rows with many or all NA values as Knn Impute will not allow this ####
data1<-data1[rowSums(is.na(data1)) < 25, ]

### Train Model using K nearnest neighbors aproach with Knn Impute algorithm ###
na_model<-preProcess(data1, method = c("knnImpute"),na.action = na.pass)
### Predict imputeted values given our dataset 
na_inpute<-predict(na_model,data1)

### Create descriptive statistic variable to unstandardize the imputed value ###
mean<- mean(data1$Spud_Year, na.rm = TRUE)
sd<- sd(data1$Spud_Year, na.rm = TRUE)
data1$Spud_Year1<-round((na_inpute$Spud_Year*sd) + mean,0)

# Check to see if imputation was corrrect (compare original year to imputed year for non Na's) #
data1$Spud_Year-data1$Spud_Year1


## Interpolate Completion Year ##

# Create completion year variable #
data1$Complete_Year<-substr(data1$Completion.Date,1,4)
data1$Complete_Year<-as.numeric(data1$Complete_Year)

### Train Model (with imputed Spud Year to gain predictive power) using K nearnest neighbors aproach with Knn Impute algorithm ###
na_model.c<-preProcess(data1, method = c("knnImpute"),na.action = na.pass)
### Predict imputeted values given our dataset 
na_inpute<-predict(na_model.c,data1)

### Create descriptive statistic variable to unstandardize the imputed value ###
mean1<- mean(data1$Complete_Year, na.rm = TRUE)
sd1<- sd(data1$Complete_Year, na.rm = TRUE)
data1$Complete_Year1<-round((na_inpute$Complete_Year*sd1) + mean1,0)

# Check to see if imputation was corrrect (compare original year to imputed year for non Na's) #
data1$Complete_Year-data1$Complete_Year1

# Perform data management to have original dataset fit into other code 
safe<-safe[,which(colMeans(!is.na(safe)) > .99)]
safe<-safe[rowSums(is.na(safe)) < 25, ]
index<-safe$id
data.b<-data_all[index,]
inputed<-data1[,c(36,38)]
data.b<-cbind(data.b,inputed)
colnames(data.b)[142]<-"Spud_Year"
colnames(data.b)[143]<-"Complete_Year"

### After matching Spud Year to yearly wind data, our NARR wind data starts from 1978. Thus, I omit all Spud Years before 1978 to loop without
### error. I have adjusted the code to fill in the 1978 value for years 1950-1978 but I do not feel comfortable with that given the large wind 
### direction variation from year to year.
data.b<-data.b[data.b$Spud_Year > 1978,]

# Write dataset to WD
write.csv(data.b,"Horizontal_Wells_Interpolated.csv")


#################################
#   Interpolate Vertical Wells  #
#################################

# Clear Global Environment  #
rm(list=ls())


data<-read.csv("vertical__wells.csv")
colnames(data)[143]<-"Spud_Year"
data_all<-data
safe<-data
safe$id<-1:nrow(safe)

#### Subset data into columns with few NAs ####
data1<-data[,which(colMeans(!is.na(data)) > .99)]

# Classify spud year variable #
data1$Spud_Year<-as.numeric(data1$Spud_Year)


#### Eliminate rows with many or all NA values as Knn Impute will not allow this ####
data1<-data1[rowSums(is.na(data1)) < 25, ]

### Train Model using K nearnest neighbors aproach with Knn Impute algorithm ###
na_model<-preProcess(data1, method = c("knnImpute"),na.action = na.pass)
### Predict imputeted values given our dataset 
na_inpute<-predict(na_model,data1)

### Create descriptive statistic variable to unstandardize the imputed value ###
mean<- mean(data1$Spud_Year, na.rm = TRUE)
sd<- sd(data1$Spud_Year, na.rm = TRUE)
data1$Spud_Year1<-round((na_inpute$Spud_Year*sd) + mean,0)

# Check to see if imputation was corrrect (compare original year to imputed year for non Na's) #
data1$Spud_Year-data1$Spud_Year1

data1<-data1[data1$Spud_Year > 1978,]

# Perform data management to have original dataset fit into other code 
safe<-safe[,which(colMeans(!is.na(safe)) > .99)]
safe<-safe[rowSums(is.na(safe)) < 25, ]
index<-safe$id
data.b<-data_all[index,]
inputed<-data1[,c(36,38)]
data.b<-cbind(data.b,inputed)
colnames(data.b)[142]<-"Spud_Year"
#colnames(data.b)[143]<-"Complete_Year"

# Write dataset to WD
write.csv(data.b,"Vertical_Wells_Interpolated.csv")


