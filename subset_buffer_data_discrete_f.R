
#########################################################################################################
### This script serves to separate datasets based on a manually input discrete distance buffer        ###
#########################################################################################################

# Author : Adam Patterson

rm(list=ls())

# Load appropriate data
setwd("M:/project/adam/paper 2/input/data/narr")
dat<-read.csv("40km_wells.csv")
setwd("M:/project/adam/paper 2/input/wells")
data10<-read.csv("fips_codes.csv")
data21<-read.csv("update_panel_yield1.csv")
data21$fips_year<-NULL
data22<-data21

# Create variables to match within other data
data21$fips_year<-paste(data21$fips,"/",data21$year, sep ="")
dat$fips_year<-paste(dat$fips,"/",dat$Spud_Year,sep = "")

# Set WD for product of loop to write into
setwd("M:/project/adam/paper 2/input/wells/Yield Mapped")

# Set i for different threshold
# i = 
for (i in 1:40) {
  data11<-dat[dat$distance <= i,]
  data22<-data21
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
  data22$dwnwnd_well[is.na(data22$dwnwnd_well)]<-0

  ## Create state dummy for fixed effects
  match(data22$fips,data10$fips)
  data10$state[match(data22$fips,data10$fips)]
  data22$state<-data10$state[match(data22$fips,data10$fips)]
  
  # Write data
  write.csv(data22,paste0("Upwind_Panel_",i,"km_Spud",".csv"))
}




