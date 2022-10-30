###########################################
## Calculate Average Wind Direction of   ##
## 3 closest resolution grids to wells   ##
## and mapping yearly wind direction of  ## 
## Spud year to each well                ##
###########################################

# Author: Adam Patterson 

# Set WD and load data
library(geosphere)
setwd("M:/project/adam/paper 2/input/data/narr")
wind<-read.csv("yearly_wind_direction2.csv")
setwd("M:/project/adam/paper 2/input/wells")
wells<-read.csv("County_Wells.csv")
wells<-wells[-1,]

# Create empty data frame to feed data into
# For each well row, we calculate the distance of long/lat in NARR data.
mat2<-matrix(NA,nrow = 42,ncol = nrow(wells))
mat2<-as.data.frame(mat2)

# Create average wind direction of 3 closest resolution grids
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

###################################################
### Match yearly average direction to Spud year ###
###################################################
wind<-mat3
wind<-`colnames<-`(wind, 1979:2020)
wind<-as.data.frame(wind)
wind<-wind[-1,]
wells<-wells[-1,]

# Cbind data into one large dataset for loop
data5<-cbind(wells,wind)

# Map each spud year to yearly average direction 
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

# Write data
write.csv(data5, "County_Wells_Wind.csv")