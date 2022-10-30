###############################################################
## This script runs a loop for each km buffer distance in    ##
## providing a robustness check mapping SPUD YEAR            ##   
###############################################################


## Clear Global Environment, Set WD, load relevant packages  ##
rm(list=ls())
setwd("M:/project/adam/paper 2/input/wells")
library(openxlsx)
library(haven)
library(geosphere)

## Read Local Data
data<-read.csv("Horizontal_Wells_Interpolated.csv", header = TRUE)
data10<-read.csv("fips_codes.csv")
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


## Create Inverted Distance Weighted matrix of all wells from each county (include bearing direction of well from county point)
mat4.b<-data[1,c(6,143:144,139:140)]
mat4.c<-data[1,c(6,143:144,139:140)]

###############################################################
##          Start loop for each km distance                  ##
###############################################################
for (buffer in 4:4) {
  
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
  which(colnames(data)== "Well.Number")

  for (i in 1:nrow(mat)) {
    a<-which(mat_km_scaled[,i] <= buffer)
    if (length(a) < 0 | length(a) > 0) {
      distance<-mat_km_scaled[a,i]
      bearing<-finalBearing(mat[,c('long','lat')][i,], data[a,c('Surface.Hole.Longitude..WGS84.','Surface.Hole.Latitude..WGS84.')])
      mat4<-data[a,c(6,143:144,139:140)]
      mat6<-cbind(mat4,distance,bearing)
      mat6$distance<-1/mat6$distance
      mat6$fips_point<-mat[i,1]
      mat4.a<-rbind(mat4.a,mat6)
    } else {
      next
    }
  }
  wells_within_20km<-mat4.a
  summary(wells_within_20km$bearing)
  wells_within_20km$bearing_dir<-(wells_within_20km$bearing + 360) %% 360
  
  ## Part 2
  setwd("M:/project/adam/paper 2/input/data/narr")
  wells<-wells_within_20km
  wind<-read.csv("yearly_wind_direction2.csv")
  ### Calculate Average Wind Direction per Well ###
  
  # Create empty data frame to feed data into
  # For each well row, we calculate the distance of long/lat in NARR data.
  mat2<-matrix(NA,nrow = 42,ncol = nrow(wells))
  mat2<-as.data.frame(mat2)
  
  for (i in 1:nrow(wells)) {
    x1<-distHaversine(wells[,c('Surface.Hole.Longitude..WGS84.','Surface.Hole.Latitude..WGS84.')][i,], wind[,c(2,3)])
    x1.a<-sort(x1,decreasing = FALSE)
    x1.b<-order(x1,decreasing = FALSE)
    total_distance<-sum(x1.a[1:3])
    one_share<- 1/(x1.a[1] / total_distance)
    two_share<- 1/(x1.a[2] / total_distance)
    three_share<- 1/(x1.a[3] / total_distance)
    total_share<-sum(one_share,two_share,three_share)
    one_weight<-one_share/total_share
    two_weight<-two_share/total_share
    three_weight<-three_share/total_share
    one<-wind[x1.b[1],]
    one_weighted<- one*one_weight
    two<-wind[x1.b[2],]
    two_weighted<- two*two_weight
    three<-wind[x1.b[3],]
    three_weighted<- three*three_weight
    closest_three<-rbind(one_weighted,two_weighted,three_weighted)
    test<-apply(closest_three, 2,sum)
    mat2[,i]<-test[4:45]
  }
  mat2<-`rownames<-`(mat2,1979:2020)
  mat2<-`colnames<-`(mat2,wells$X)
  mat3<-t(mat2)
  mat3<-`rownames<-`(mat3,wells$X)
  
  
  ### Match Yearly Wind Average Direction to SPUD Year, April 29, 2021 ###
  wind<-mat3
  wind<-`colnames<-`(wind, 1979:2020)
  wind<-as.data.frame(wind)
  wind<-wind[-1,]
  wells<-wells[-1,]
  # Cbind data
  data5<-cbind(wells,wind)


  for (i in 1:nrow(data5)) {
    col_index<-which(data5$Spud_Year[i] == colnames(data5))
    if (length(col_index) > 0) {
      data5$wnd_dir_spud[i]<-data5[i,col_index]
    } else {
      next
    }
  }
  
  
  # Subset leaving out non used  years 
  data5<-data5[,c(1:9,52)]
  #data5<-na.omit(data5)
  
  ##### Create new variable for 180 degrees opposite of wind direction to create direction the wind is coming from
  for (i in 1:nrow(data5)) {
    if ( data5$wnd_dir_spud[i] < 180 ) {
      data5$wind_180[i]<- data5$wnd_dir_spud[i] + 180 
    } else {
      data5$wind_180[i]<- data5$wnd_dir_spud[i] - 180 
    }
  }
  
  data5$wind_dir_spud<-data5$wind_180
  
  
  ##################################################
  ##### Separate Upwind Wells, May 8, 2021 #########
  ##################################################
  ##################################################
  
  for (i in 1:nrow(data5)) {
    if (data5$wind_dir_spud[i] < 45) {
      if (  ((data5$wind_dir_spud[i] + 315) < data5$bearing_dir[i]) & (data5$bearing_dir[i] < (data5$wind_dir_spud[i]+45))) {
        data5$upwind[i]<-"1"
      } else {
        data5$upwind[i]<-"0"
      }
    }
    else if (data5$wind_dir_spud[i] > 315) {
      if (  ((data5$wind_dir_spud[i] - 45) < data5$bearing_dir[i]) & (data5$bearing_dir[i] < (data5$wind_dir_spud[i]-315))) {
        data5$upwind[i]<-"1"
      } else {
        data5$upwind[i]<-"0"
      }
    }    
    else {
      if ( ((data5$wind_dir_spud[i]-45) < data5$bearing_dir[i]) & (data5$bearing_dir[i] < (data5$wind_dir_spud[i]+45))) {
        data5$upwind[i]<-"1"
      } else {
        data5$upwind[i]<-"0"
      }  
    } 
  }
  
  
  # Downwind
  for (i in 1:nrow(data5)) {
    if (data5$wnd_dir_spud[i] < 45) {
      if (  ((data5$wnd_dir_spud[i] + 315) < data5$bearing_dir[i]) & (data5$bearing_dir[i] < (data5$wnd_dir_spud[i]+45))) {
        data5$upwind1[i]<-"1"
      } else {
        data5$upwind1[i]<-"0"
      }
    }
    else if (data5$wnd_dir_spud[i] > 315) {
      if (  ((data5$wnd_dir_spud[i] - 45) < data5$bearing_dir[i]) & (data5$bearing_dir[i] < (data5$wnd_dir_spud[i]-315))) {
        data5$upwind1[i]<-"1"
      } else {
        data5$upwind1[i]<-"0"
      }
    }    
    else {
      if ( ((data5$wnd_dir_spud[i]-45) < data5$bearing_dir[i]) & (data5$bearing_dir[i] < (data5$wnd_dir_spud[i]+45))) {
        data5$upwind1[i]<-"1"
      } else {
        data5$upwind1[i]<-"0"
      }  
    } 
  }
  
  colnames(data5)[14]<-"dwn_wnd"
  ##### Map upwind and downwind wells to yield data
  setwd("M:/project/adam/paper 2/input/wells")
  data11<-data5
  data11$upwind<-as.integer(data11$upwind)
  data11$dwn_wnd<-as.integer(data11$dwn_wnd)
  data22<-read.csv("us_panel_wellcount3.csv")
  
  # Create fips variable per year to match
  data11$fips_year<-paste(data11$fips,"/",data11$Spud_Year,sep = "")
  data22$fips_year<-paste(data22$fips,"/",data22$year, sep ="")
  
  # Aggregate total upwind wells per fips/year
  total_well_per_county_per_year<-aggregate(data11$upwind,list(as.factor(data11$fips_year)),sum)
  colnames(total_well_per_county_per_year)[1]<-"fips_year"
  colnames(total_well_per_county_per_year)[2]<-"upwnd_wells"
  
  # Match upwind well number to agriculture fips and year number
  match(data22$fips_year,total_well_per_county_per_year$fips_year)
  total_well_per_county_per_year$upwnd_wells[match(data22$fips_year,total_well_per_county_per_year$fips_year)]
  data22$upwnd_well<-total_well_per_county_per_year$upwnd_wells[match(data22$fips_year,total_well_per_county_per_year$fips_year)]
  
  # Convert NA values to 0
  summary(data22$upwnd_well)
  data22$upwnd_well[is.na(data22$upwnd_well)]<-0
  summary(data22$upwnd_well)
  
  # Aggregate total downwind wells per fips/year
  total_well_per_county_per_year1<-aggregate(data11$dwn_wnd,list(as.factor(data11$fips_year)),sum)
  colnames(total_well_per_county_per_year1)[1]<-"fips_year"
  colnames(total_well_per_county_per_year1)[2]<-"dwnwnd_wells"
  
  # Match downwind well number to agriculture fips and year number
  match(data22$fips_year,total_well_per_county_per_year1$fips_year)
  total_well_per_county_per_year1$dwnwnd_wells[match(data22$fips_year,total_well_per_county_per_year1$fips_year)]
  data22$dwnwnd_well<-total_well_per_county_per_year1$dwnwnd_wells[match(data22$fips_year,total_well_per_county_per_year1$fips_year)]
  
  # Convert NA values to 0
  summary(data22$dwnwnd_well)
  data22$dwnwnd_well[is.na(data22$dwnwnd_well)]<-0
  summary(data22$dwnwnd_well)
  
  ## Create state dummy for fixed effects
  match(data22$fips,data10$fips)
  data10$state[match(data22$fips,data10$fips)]
  data22$state<-data10$state[match(data22$fips,data10$fips)]

  write.csv(data22,paste0("Upwind_Panel_",buffer,"km_Spud",".csv"))
  
}



