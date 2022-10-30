###############################################################
## This script calculates direction and distance of each     ##
## well within 40km of a county centroid                     ##   
###############################################################

# Author : Adam Patterson

## Clear Global Environment, Set WD, load relevant packages  ##
rm(list=ls())
setwd("M:/project/adam/paper 2/input/wells")
library(openxlsx)
library(haven)
library(geosphere)

## Read Local Data
data<-read.csv("Horizontal_Wells_Interpolated.csv")
data2<-read_dta("us_panel.dta")
data3<-read.csv("fips_codes.csv")

# Get list of unique fips locations
x<-unique(data2$fips)
mat<-as.data.frame(x)
colnames(mat)[1]<-"fips"

# Map longitude to unique fips list
match(mat$fips,data2$fips)
data2$longitude[match(mat$fips,data2$fips)]
mat$long<-data2$longitude[match(mat[,1],data2$fips)]

# Map latitude to unique fips list
match(mat$fips,data2$fips)
data2$latitude[match(mat$fips,data2$fips)]
mat$lat<-data2$latitude[match(mat$fips,data2$fips)]

## Test to make sure the distance function is working proper between the first row and all the data.
## Therefore, first distance should be 0 and it checks out.
distHaversine(data2[,c('longitude','latitude')][1,], data2[,c('longitude','latitude')])

mat2<-matrix(NA,nrow = nrow(data),ncol = nrow(mat))
for (i in 1:nrow(mat)) {
  x1<-distHaversine(mat[,c('long','lat')][i,], data[,c("Surface.Hole.Longitude..WGS84.","Surface.Hole.Latitude..WGS84.")])
  mat2[,i]<-x1
}
mat2<-`colnames<-`(mat2,mat$fips)
#Take results given in M and convert dataset to KM
mat_km_scaled<-.001*mat2
mat_km_scaled<-as.data.frame(mat_km_scaled)

# Create county and year variable from wells data and map into USDA Fips codes for mapping into agricultural output
data2$fips_year<-paste(data2$fips,"/",data2$year, sep ="")
data$Production_Year<-substr(data$First.Prod.Date,1,4)
data$County<-substr(as.character(data$County.Parish),1,nchar(as.character(data$County.Parish))-5)
data3$county<-gsub(" ", "",data3$county)
data$County<-gsub(" ", "",data$County)
data$County<-gsub("\\.","",data$County)
data$County<-tolower(data$County)
data$State<-tolower(data$State)
data3$county<-tolower(data3$county)
data3$state<-tolower(data3$state)
data$county_state<-paste(data$County,"/",data$State, sep = "")
data3$county_state<-paste(data3$county,"/",data3$state, sep = "")
match(data$county_state,data3$county_state)
data3$fips[match(data$county_state,data3$county_state)]
data$fips<-data3$fips[match(data$county_state,data3$county_state)]
data$fips[data$county_state=="broomfield/co"]<-"8014"
data$fips[data$county_state=="brazos-lb/tx"]<-"48041"

## Create matrix of all wells from each county (include bearing direction of well and distance from county point)
## Use buffer of 40km (our upper bound km for robustness check) to subset data down into smaller km intervals in a later script
buffer<-40
mat4.b<-data[1,c(6,143:144,139:140)]
mat4.c<-data[1,c(6,143:144,139:140)]
mat4.a<-matrix(NA,ncol = 8,nrow=1)
mat4<-matrix(NA,ncol = 9,nrow=1)
mat4<-as.data.frame(mat4)
mat4.a<-as.data.frame(mat4.a)
colnames(mat4)<-colnames(mat4.b)
colnames(mat4.a)<-colnames(mat4.b)
colnames(mat4.a)[6]<-"distance"
colnames(mat4.a)[7]<-"bearing"
colnames(mat4.a)[8]<-"fips_point"
colnames(mat4)[4]<-"year"

# Create loop to capture wells less than or equal to our set buffer boundary 
for (i in 1:nrow(mat)) {
    a<-which(mat_km_scaled[,i] <= buffer)
    if (length(a) < 0 | length(a) > 0) {
      distance<-mat_km_scaled[a,i]
      bearing<-finalBearing(mat[,c('long','lat')][i,], data[a,c('Surface.Hole.Longitude..WGS84.','Surface.Hole.Latitude..WGS84.')])
      mat4<-data[a,c(6,143:144,139:140)]
      mat6<-cbind(mat4,distance,bearing)
      mat6$distance<-mat6$distance
      mat6$fips_point<-mat[i,1]
      mat4.a<-rbind(mat4.a,mat6)
    } else {
      next
    }
  }
wells_within_20km<-mat4.a
wells_within_20km$bearing_dir<-(wells_within_20km$bearing + 360) %% 360
  
# Write data to WD
write.csv(wells_within_20km, "County_Wells.csv")