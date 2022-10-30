#########################################################################################################
### This script serves to separate datasets for vertically drilled wells (COGD) based on a manually   ###
### input discrete distance buffer                                                                    ###
#########################################################################################################

# Author : Adam Patterson

rm(list=ls())
library(gtools)

# Load appropriate data
setwd("M:/project/adam/paper 2/input/wells")
dat<-read.csv("40km_wells_vertical.csv")
data10<-read.csv("fips_codes.csv")
data21<-read.csv("update_panel_yield1.csv")
data21$fips_year<-NULL
data22<-data21

# Create variables to match within other data
data21$fips_year<-paste(data21$fips,"/",data21$year, sep ="")
dat$fips_year<-paste(dat$fips,"/",dat$Spud_Year,sep = "")

# Load all files from UOGD wells folder into environment 
setwd("M:/project/adam/paper 2/input/wells/Yield Mapped")
myFiles <- mixedsort(list.files("M:/project/adam/paper 2/input/wells/Yield Mapped"))
my.data <- lapply(myFiles, read.csv)


# Set WD for product of loop to write into
setwd("M:/project/adam/paper 2/input/wells/Yield Mapped Vertical1")

for (i in 1:40) {
  data11<-dat[dat$distance <= i,]
  data22<-as.data.frame(my.data[i])
  # Aggregate total upwind wells per fips/year
  total_well_per_county_per_year<-aggregate(data11$upwind,list(as.factor(data11$fips_year)),sum)
  colnames(total_well_per_county_per_year)[1]<-"fips_year"
  colnames(total_well_per_county_per_year)[2]<-"upwnd_wells"
  
  # Match upwind well number to agriculture fips and year number
  match(data22$fips_year,total_well_per_county_per_year$fips_year)
  total_well_per_county_per_year$upwnd_wells[match(data22$fips_year,total_well_per_county_per_year$fips_year)]
  data22$upwnd_well_vertical<-total_well_per_county_per_year$upwnd_wells[match(data22$fips_year,total_well_per_county_per_year$fips_year)]
  
  # Convert NA values to 0
  summary(data22$upwnd_well_vertical)
  data22$upwnd_well_vertical[is.na(data22$upwnd_well_vertical)]<-0
  summary(data22$upwnd_well_vertical)
  
  # Aggregate total downwind wells per fips/year
  total_well_per_county_per_year1<-aggregate(data11$dwn_wnd,list(as.factor(data11$fips_year)),sum)
  colnames(total_well_per_county_per_year1)[1]<-"fips_year"
  colnames(total_well_per_county_per_year1)[2]<-"dwnwnd_wells"
  
  # Match downwind well number to agriculture fips and year number
  match(data22$fips_year,total_well_per_county_per_year1$fips_year)
  total_well_per_county_per_year1$dwnwnd_wells[match(data22$fips_year,total_well_per_county_per_year1$fips_year)]
  data22$dwnwnd_well_vertical<-total_well_per_county_per_year1$dwnwnd_wells[match(data22$fips_year,total_well_per_county_per_year1$fips_year)]
  
  # Convert NA values to 0
  summary(data22$dwnwnd_well_vertical)
  data22$dwnwnd_well_vertical[is.na(data22$dwnwnd_well_vertical)]<-0
  summary(data22$dwnwnd_well_vertical)
  
  
  # Write data
  write.csv(data22,paste0("Upwind_Panel_",i,"km_Spud",".csv"))
}




