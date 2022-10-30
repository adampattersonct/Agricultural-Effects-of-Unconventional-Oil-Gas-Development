###############################################################
## This script merges all wells together given that Enverus  ##
## downloads are limited to 250,000 observations or less     ##
###############################################################

# Author : Adam Patterson 
# Script 1

# Clear global environment 
rm(list=ls())
# Set WD
setwd("M:/project/adam/paper 2/input/wells/Enverus Raw/")

# Load all files from folder into environment 
myFiles <- list.files(pattern="*.CSV")
data<-read.csv(myFiles[1])

# Create empty matrix and name columns for binding
empty<-matrix(NA,ncol = 139, nrow=1)
empty<-`colnames<-`(empty,colnames(data))

# Run loop to merge all data together
for (i in 1:19) {
  data<-read.csv(myFiles[i])
  empty<-rbind(empty,data)
}
# Delete first NA row
empty<-empty[-1,]

# Set WD to code directory 
setwd("M:/project/adam/paper 2/input/wells")

# Write aggregated data into set WD
write.csv(empty, "Well_Production_Data.csv")

