#############################################
## This script maps each well to Upwind or ##
## Downwind from the county centroid       ##
#############################################

# Author: Adam Patterson 

data5<-read.csv("County_Wells_Wind.csv")
data5<-data5[-1,]

## Create new variable for 180 degrees opposite of wind direction to 
## interpretation of what direction the wind is coming from
for (i in 1:nrow(data5)) {
  if ( data5$wnd_dir_spud[i] < 180 ) {
    data5$wind_180[i]<- data5$wnd_dir_spud[i] + 180 
  } else {
    data5$wind_180[i]<- data5$wnd_dir_spud[i] - 180 
  }
}
data5$wind_dir_spud<-data5$wind_180

## Separate Upwind Wells ##
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

## Separate Upwind Wells ##
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

# Write data into WD
write.csv(data5,"40km_wells.csv")