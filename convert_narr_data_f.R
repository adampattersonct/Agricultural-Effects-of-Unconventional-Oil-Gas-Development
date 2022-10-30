###############################################
## Converting NARR data into U and V Wind,   ##
## calculating yearly averages, calculating  ##
## wind direction and speed                  ##
###############################################

# Author : Adam Patterson

# Set WD and load packages
setwd("M:/project/adam/paper 2/input/data/narr")
library(sf) 
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(lattice)
library(plyr)

### Read in U Wind data ###
# Create first layer for long, lat values to merge with
uwnd1 <- raster("uwnd.10m.mon.mean.nc", band=1) # band indicates the month( starting from Jan 1979 to Mar 2021) We want to stop after band 504 for full 2020 year //
crs(uwnd1) <- "+proj=lcc +lat_0=50 +lon_0=-107 +lat_1=50 +lat_2=50 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs" #this is the source from uwnd1 raster
r.pts <- rasterToPoints(uwnd1, spatial=TRUE)
proj4string(r.pts)
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
r.pts <- spTransform(r.pts, CRS(geo.prj)) 
proj4string(r.pts)
x1<-data.frame(r.pts@data, long=coordinates(r.pts[,1])) 

## Warning message of: 'in proj4string(r.pts) : CRS object has comment, which is lost in output ' does not appear to impact x1 datafram for values needed
## Thus, I continue on accepting the warning message. (I have done a small amount of research about this online...continuing on until after baseline)

coordinates(r.pts[,1])
r.pts[,1]

summary(x1$long.x)
summary(x1$long.y)

# Create a loop to merge all layers to 1 large dataset. 
for (i in c(2:504)) {
  uwnd <- raster("uwnd.10m.mon.mean.nc", band=i) # band indicates the month( starting from Jan 1979 to Mar 2021) We want to stop after band 504 for full 2020 year /// What is the special concept (1 degree?) and the measure (wind speed?)
  crs(uwnd) <- "+proj=lcc +lat_0=50 +lon_0=-107 +lat_1=50 +lat_2=50 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs"
  r.pts <- rasterToPoints(uwnd, spatial=TRUE)
  proj4string(r.pts)
  geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
  r.pts <- spTransform(r.pts, CRS(geo.prj)) 
  proj4string(r.pts)
  x2<-data.frame(r.pts@data, long=coordinates(r.pts[,1])) 
  colnames(x2)[1]<-paste("u_mean",i,sep="")
  x1<-merge(x1, x2, by=c("long.x","long.y"))
  
}
#/write.csv(x1, "u_wind_mon.csv")
#x1<-u


### Read in V Wind data ###
# Create first layer for long, lat values to merge with
vwnd1 <- raster("vwnd.10m.mon.mean.nc", band=1) # band indicates the month( starting from Jan 1979 to Mar 2021) We want to stop after band 504 for full 2020 year /// What is the special concept (1 degree?) and the measure (wind speed?)
crs(vwnd1) <- "+proj=lcc +lat_0=50 +lon_0=-107 +lat_1=50 +lat_2=50 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs"
r.pts1 <- rasterToPoints(vwnd1, spatial=TRUE)
proj4string(r.pts1)
geo.prj1 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
r.pts1 <- spTransform(r.pts1, CRS(geo.prj1)) 
proj4string(r.pts1)
x3<-data.frame(r.pts1@data, long=coordinates(r.pts1[,1])) 

# Create a loop to merge all layers to 1 large dataset. 
for (i in c(2:504)) {
  uwnd <- raster("vwnd.10m.mon.mean.nc", band=i) # band indicates the month( starting from Jan 1979 to Mar 2021) We want to stop after band 504 for full 2020 year /// What is the special concept (1 degree?) and the measure (wind speed?)
  crs(uwnd) <- "+proj=lcc +lat_0=50 +lon_0=-107 +lat_1=50 +lat_2=50 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs"
  r.pts <- rasterToPoints(uwnd, spatial=TRUE)
  proj4string(r.pts)
  geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
  r.pts <- spTransform(r.pts, CRS(geo.prj)) 
  proj4string(r.pts)
  x4<-data.frame(r.pts@data, long=coordinates(r.pts[,1])) 
  colnames(x4)[1]<-paste("u_mean",i,sep="")
  x3<-merge(x3, x4, by=c("long.x","long.y"))
  
}
#/write.csv(x3, "v_wind_mon.csv")
#x3<-v



## Rename to stay consistent with previously written code ##
grid_uwnd1<-x1
grid_vwnd1<-x3

### Calculate yearly average of U wind ###
month<-grid_uwnd1[,-c(1,2,3)]
x1<-matrix(NA, ncol = 1, nrow = nrow(month))
x1<-as.data.frame(x1)
for (i in seq(1,493,12)) {
  x<-apply(month[,i:(i+11)], 1, mean)
  x1<-cbind(x1,x)
}
x1<-`colnames<-`(x1,seq(1978,2020,1))
x1<-x1[,-1]
yearly_uwind<-cbind(grid_uwnd1[,c(2,3)],x1)
#/write.csv(yearly_uwind, "yearly_uwind.csv")

### Calculate yearly average of V wind ###
month1<-grid_vwnd1[,-c(1,2,3)]
x2<-matrix(NA, ncol = 1, nrow = nrow(month1))
x2<-as.data.frame(x2)
for (i in seq(1,493,12)) {
  y<-apply(month1[,i:(i+11)], 1, mean)
  x2<-cbind(x2,y)
}
x2<-`colnames<-`(x2,seq(1978,2020,1))
x2<-x2[,-1]
yearly_vwind<-cbind(grid_vwnd1[,c(2,3)],x2)
#/write.csv(yearly_vwind, "yearly_vwind.csv")

############################# After data has been created, call from WD######################################
setwd("M:/project/adam/paper 2/input/data/narr")
yearly_uwind<-read.csv("yearly_uwind.csv")
yearly_vwind<-read.csv("yearly_vwind.csv")
############################# ###############################################################################


# Calculate wind direction per year   
b1<-matrix(NA,ncol = 1,nrow = nrow(yearly_vwind))
b1<-as.data.frame(b1)
for (i in 4:45) {
  # Feed v and u wind data into atan2 function
  wind_dir_trig<-atan2(yearly_vwind[,i],yearly_uwind[,i])
  wind_dir_degrees = wind_dir_trig * 180/pi
  # Convert wind data to meteorological direction, add 180 for interpretation of what direction wind is coming from 
  wind_dir = wind_dir_degrees + 180
  #Conver this trig coordinate to cardinal coordinates 
  #wind_dir<- 90 - wind_dir_trig_degrees
  b1<-cbind(b1,wind_dir)
}
#Check summary statistics, see that min is near 0 and max is near 360. Make sure not just adding 180 to Azimuth angles 
summary(b1$wind_dir)
# Cbind yearly directional dataset with long/lat values 
b1<-b1[,-1]
yearly_wind_direction<-cbind(yearly_uwind[,2:3],b1)
write.csv(yearly_wind_direction, "yearly_wind_direction.csv")


# Calculate wind speed per year
b2<-matrix(NA,ncol = 1,nrow = nrow(yearly_vwind))
b2<-as.data.frame(b2)
for (i in 4:45) {
  speed<-sqrt((yearly_vwind[,i]^2)+(yearly_uwind[,i]^2))
  b2<-cbind(b2,speed)
}
b2<-b2[,-1]
yearly_wind_speed<-cbind(yearly_uwind[,2:3],b2)
write.csv(yearly_wind_speed, "yearly_wind_speed.csv")


# Read into WD that well data is in 
setwd("M:/project/adam/paper 2/input/wells")
write.csv(yearly_wind_direction, "yearly_wind_direction.csv")
write.csv(yearly_wind_speed, "yearly_wind_speed.csv")


