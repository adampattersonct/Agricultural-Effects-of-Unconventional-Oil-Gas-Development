####################################################################################################
## The purpose of this script is to merge covariate data, from 2010-2019, and standardize both   ###
## data sources to create set of covariates for our baseline model                               ###
####################################################################################################

# Author: Adam Patterson 

rm(list=ls())

install.packages("robustHD")
library(robustHD)

setwd("M:/project/adam/paper 2/input/wells")

# New Covariate Data #
data<-read.csv("2010-2019covariates.csv")
data<-data[-1,]

# Standardize temperature and precipitation using min-max standardization 
min.temp<-min(data$temp)
max.temp<-max(data$temp)
data$temperature<- (data$temp - min.temp) / (max.temp - min.temp)

min.prec<-min(data$prec)
max.prec<-max(data$prec)
data$precipitation<- (data$prec - min.prec) / (max.prec - min.prec)

# Agriculture Data #
data1<-read.csv("update_panel_yield.csv")

# Standardize temperature and precipitation using min-max standardization 
min.temp1<-min(data1$tavg, na.rm = TRUE)
max.temp1<-max(data1$tavg, na.rm = TRUE)
data1$temperature<- (data1$tavg - min.temp1) / (max.temp1 - min.temp1)

min.prec1<-min(data1$prec, na.rm = TRUE)
max.prec1<-max(data1$prec, na.rm = TRUE)
data1$precipitation<- (data1$prec - min.prec1) / (max.prec1 - min.prec1)

## Create fips_year variable to match with agriculture data ##
data1$temp<-NA
data1$precip<-NA
# Classify variable as character to match with previous fips_year
data$fips_year<-paste(data$fips,"_",data$year, sep="")
data1$fips_year<-as.character(data1$fips_year)

# Match fips_year between both datasets for temperature 
match(data1$fips_year,data$fips_year)
data$temperature[match(data1$fips_year,data$fips_year)]
data1$temp<-data$temperature[match(data1$fips_year,data$fips_year)]

# Match fips_year between both datasets for precipitation 
match(data1$fips_year,data$fips_year)
data$precipitation[match(data1$fips_year,data$fips_year)]
data1$precip<-data$precipitation[match(data1$fips_year,data$fips_year)]


# Replace NA with 0 to add columns together
data1$temperature[is.na(data1$temperature)]<-0
data1$temp[is.na(data1$temp)]<-0

data1$precipitation[is.na(data1$precipitation)]<-0
data1$precip[is.na(data1$precip)]<-0

# Create final standardized variable
data1$temperature_final<-data1$temperature + data1$temp
data1$precipitation_final<-data1$precipitation + data1$precip

# Perform housekeeping to delete created/unneccesary variables 
data1<-data1[,-c(60:63)]
data1$tavg<-NULL
data1$prec<-NULL
write.csv(data1,"update_panel_yield1.csv")
