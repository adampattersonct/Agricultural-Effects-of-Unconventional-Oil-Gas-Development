
#################################################################################
# This script calculates a back of the envelope calculation for dollar losses  ##
# from lost production as a result of  UOGD                                    ##
#################################################################################
# This script was not finished or used in WP
# Author: Adam Patterson 

# Clear global environment 
#rm(list=ls())

library(gtools)
library(mlr3)
library(mlr3learners)
library(DoubleML)
library(ranger)
library(fixest)
library(texreg)
library(ggplot2)
library(iplots)
library(car)
#setwd("M:/project/adam/paper 2/input/wells/Discrete Yield Mapped")

setwd("M:/project/adam/paper 2/input/wells/Yield Mapped Vertical1/")

data<-read.csv("Upwind_Panel_20km_Spud.csv")
setwd("M:/project/adam/paper 2/input/wells")


# Acres impacted pre 2010
dat<-data[data$upwnd_well>1,]
corn<-sum(na.omit(dat$corn_area))
soy<-sum(na.omit(dat$soy_area))

# Acres impacted in 2017 alone
#### Calculating Total Soy Area
data<-read.csv("soy_census1.csv")
data1<-read.csv("fips_codes.csv")
data$CV....<-NULL

data$County.ANSI<-as.numeric(data$County.ANSI)
data$County.ANSI<-ifelse(data$County.ANSI<10,paste(0,data$County.ANSI),data$County.ANSI)
data$County.ANSI<-gsub(" ","",data$County.ANSI)

if (data$County.ANSI < 100){
  data$County.ANSI<-paste(0,data$County.ANSI)
} else {
  data$County.ANSI<-data$County.ANSI
}

data$County.ANSI<-paste(0,data$County.ANSI)
data$County.ANSI<-gsub(" ","",data$County.ANSI)
data$County.ANSI<-substr(data$County.ANSI,nchar(data$County.ANSI)-2,nchar(data$County.ANSI))
data$fips<-paste0(data$State.ANSI,data$County.ANSI)

#load fips locations with uogd within 25km
fips<-read.csv("fips_uogd.csv")
uogd<-fips$fips

total_area<-NA
for (i in 1:nrow(fips)) {
  wells<-data[data$fips == uogd[i],]
  area<-wells$Value
  total_area<-c(total_area,area)
}
total_area<-total_area[-c(1:2,13)]
total_area<-gsub(",","",total_area)
total_area<-as.numeric(total_area)
mean<-mean(total_area)
missing<-2*mean
sum<-sum(total_area)
total_acres<-sum+missing


#### Calculating Total Corn Area
data<-read.csv("corn_census1.csv")
data1<-read.csv("fips_codes.csv")
data$CV....<-NULL

data$County.ANSI<-as.numeric(data$County.ANSI)
data$County.ANSI<-ifelse(data$County.ANSI<10,paste(0,data$County.ANSI),data$County.ANSI)
data$County.ANSI<-gsub(" ","",data$County.ANSI)

if (data$County.ANSI < 100){
  data$County.ANSI<-paste(0,data$County.ANSI)
} else {
  data$County.ANSI<-data$County.ANSI
}

data$County.ANSI<-paste(0,data$County.ANSI)
data$County.ANSI<-gsub(" ","",data$County.ANSI)
data$County.ANSI<-substr(data$County.ANSI,nchar(data$County.ANSI)-2,nchar(data$County.ANSI))
data$fips<-paste0(data$State.ANSI,data$County.ANSI)

#load fips locations with uogd within 25km
fips<-read.csv("fips_uogd.csv")
uogd<-fips$fips

total_area<-NA
for (i in 1:nrow(fips)) {
  wells<-data[data$fips == uogd[i],]
  area<-wells$Value
  total_area<-c(total_area,area)
}
total_area<-total_area[-c(1,57)]
total_area<-gsub(",","",total_area)
total_area<-as.numeric(total_area)
total_acres<-sum(total_area)
























