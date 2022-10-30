###############################################################
##    This script creates robustness regression results      ##
##    using upwind well IDW weighting                        ##
###############################################################

# Author : Adam Patterson 

# Clear global environment 
rm(list=ls())

library(gtools)
library(fixest)
library(texreg)
library(ggplot2)
library(iplots)
library(car)


setwd("M:/project/adam/paper 2/input/wells/Yield Mapped Vertical1")


# Load all files from folder into environment 
myFiles <- mixedsort(list.files("M:/project/adam/paper 2/input/wells/Yield Mapped Vertical1"))
my.data <- lapply(myFiles, read.csv)

## Corn Yield

######################
## Positive Control ##
######################

# Create empty vectors for coefficients and standard errors to concatenate 
coefs<-"NA"
stderr<-"NA"

for (i in 1:40) {
  data<-as.data.frame(my.data[i])
  mod<-feols(cornyield~upwnd_well+(as.factor(state)*as.factor(year))+as.factor(fips)+temperature_final+temperature_final^2+precipitation_final+precipitation_final^2| state^year+fips,data=data)
  co<-mod$coefficients[1]
  se<-mod$se[1]
  coefs<-c(coefs,co)
  stderr<-c(stderr,se)
}
stderr<-stderr[-1]
coefs<-coefs[-1]
num<-1:40
data<-as.data.frame(cbind(coefs,stderr,num))
str(data)

i <- sapply(data, is.factor)
data[i] <- lapply(data[i], as.character)


data$coefs<-as.numeric(data$coefs)
data$num<-as.numeric(data$num)
data$stderr<-as.numeric(data$stderr)
# Create Standard Error lines
data$upper<-data$coefs+data$stderr
data$lower<-data$coefs-data$stderr

## Plot with Standard Errors
ggplot(data, aes(x=num)) + 
  geom_line(aes(y=upper, color="Standard Error")) + 
  geom_line(aes(y=lower, color="Standard Error")) + 
  geom_line(aes(y=coefs, color="Coefficient")) + ylab("") + xlab("") +
  labs(color="Legend") + labs(title = "") +ylim(-3,3)


######################
## Negative Control ##
######################

# Create empty vectors for coefficients and standard errors to concatenate 
coefs1<-"NA"
stderr1<-"NA"

for (i in 1:40) {
  data<-as.data.frame(my.data[i])
  mod1<-feols(cornyield~dwnwnd_well+(as.factor(state)*as.factor(year))+as.factor(fips)+temperature_final+temperature_final^2+precipitation_final+precipitation_final^2| state^year+fips,data=data)
  co1<-mod1$coefficients[1]
  se1<-mod1$se[1]
  coefs1<-c(coefs1,co1)
  stderr1<-c(stderr1,se1)
}
stderr1<-stderr1[-1]
coefs1<-coefs1[-1]
num1<-1:40
data1<-as.data.frame(cbind(coefs1,stderr1,num1))
str(data1)

j <- sapply(data1, is.factor)
data1[j] <- lapply(data1[j], as.character)


data1$coefs1<-as.numeric(data1$coefs1)
data1$num1<-as.numeric(data1$num1)
data1$stderr1<-as.numeric(data1$stderr1)
# Create Standard Error lines
data1$upper1<-data1$coefs1+data1$stderr1
data1$lower1<-data1$coefs1-data1$stderr1


## Plot Downwind with Standard Errors
ggplot(data1, aes(x=num1)) + 
  geom_line(aes(y=upper1, color="Standard Error")) + 
  geom_line(aes(y=lower1, color="Standard Error")) + 
  geom_line(aes(y=coefs1, color="Coefficient")) + ylab("") + xlab("") +
  labs(color="Legend") +  labs(title = "") +ylim(-3,3)


## Plot both Upwind and Downwind with Standard Errors
data11<-cbind(data,data1)
ggplot(data11, aes(x=num1)) + 
  geom_line(aes(y=upper1, color="Standard Error"), color="red1") + 
  geom_line(aes(y=lower1, color="Standard Error"),color="red1") + 
  geom_line(aes(y=coefs1, color="Dwnwnd Coefficient"), color="black") + ylab("Result") + xlab("Km Distance") +
  geom_line(aes(y=upper, color="Standard Error"),color="red1") + 
  geom_line(aes(y=lower, color="Standard Error"),color="red1") + 
  geom_line(aes(y=coefs, color="Upwnd Coefficient"), color="black") +  labs(title = "Corn Km Distance Robustness with Upwind and Downwid") +ylim(-3,3) 
+ labs(color="Legend")




###############################
### Soy Dependent Variable ###
###############################

######################
## Positive Control ##
######################

# Create empty vectors for coefficients and standard errors to concatenate 
coefs2<-"NA"
stderr2<-"NA"

for (i in 1:40) {
  data<-as.data.frame(my.data[i])
  mod2<-feols(soyyield~upwnd_well+(as.factor(state)*as.factor(year))+as.factor(fips)+temperature_final+temperature_final^2+precipitation_final+precipitation_final^2| state^year+fips,data=data)
  co2<-mod2$coefficients[1]
  se2<-mod2$se[1]
  coefs2<-c(coefs2,co2)
  stderr2<-c(stderr2,se2)
}
stderr2<-stderr2[-1]
coefs2<-coefs2[-1]
num2<-1:40
data2<-as.data.frame(cbind(coefs2,stderr2,num2))
str(data2)

k <- sapply(data2, is.factor)
data2[k] <- lapply(data2[k], as.character)


data2$coefs2<-as.numeric(data2$coefs2)
data2$num2<-as.numeric(data2$num2)
data2$stderr2<-as.numeric(data2$stderr2)
# Create Standard Error lines
data2$upper2<-data2$coefs2+data2$stderr2
data2$lower2<-data2$coefs2-data2$stderr2


## Plot with Standard Errors
ggplot(data2, aes(x=num2)) + 
  geom_line(aes(y=upper2, color="Standard Error")) + 
  geom_line(aes(y=lower2, color="Standard Error")) + 
  geom_line(aes(y=coefs2, color="Coefficient")) + ylab("") + xlab("") +
  labs(color="Legend") + labs(title = "") +ylim(-3,3)


######################
## Negative Control ##
######################

# Create empty vectors for coefficients and standard errors to concatenate 
coefs12<-"NA"
stderr12<-"NA"

for (i in 1:40) {
  data<-as.data.frame(my.data[i])
  mod12<-feols(soyyield~dwnwnd_well+(as.factor(state)*as.factor(year))+as.factor(fips)+temperature_final+temperature_final^2+precipitation_final+precipitation_final^2| state^year+fips,data=data)
  co12<-mod12$coefficients[1]
  se12<-mod12$se[1]
  coefs12<-c(coefs12,co12)
  stderr12<-c(stderr12,se12)
}
stderr12<-stderr12[-1]
coefs12<-coefs12[-1]
num12<-1:40
data12<-as.data.frame(cbind(coefs12,stderr12,num12))
str(data12)

l <- sapply(data12, is.factor)
data12[l] <- lapply(data12[l], as.character)


data12$coefs12<-as.numeric(data12$coefs12)
data12$num12<-as.numeric(data12$num12)
data12$stderr12<-as.numeric(data12$stderr12)
# Create Standard Error lines
data12$upper12<-data12$coefs12+data12$stderr12
data12$lower12<-data12$coefs12-data12$stderr12


## Plot with Standard Errors
ggplot(data12, aes(x=num12)) + 
  geom_line(aes(y=upper12, color="Standard Error")) + 
  geom_line(aes(y=lower12, color="Standard Error")) + 
  geom_line(aes(y=coefs12, color="Coefficient")) + ylab("") + xlab("") +
  labs(color="Legend") +  labs(title = "") +ylim(-3,3)


## Plot both with Standard Errors
data112<-cbind(data2,data12)

ggplot(data112, aes(x=num12)) + 
  geom_line(aes(y=upper12, color="Standard Error"), color="red3") + 
  geom_line(aes(y=lower12, color="Standard Error"),color="red3") + 
  geom_line(aes(y=coefs12, color="Dwnwnd Coefficient"), color="black") + ylab("Result") + xlab("Km Distance") +
  geom_line(aes(y=upper2, color="Standard Error"),color="red1") + 
  geom_line(aes(y=lower2, color="Standard Error"),color="red1") + 
  geom_line(aes(y=coefs2, color="Upwnd Coefficient"), color="black") +  labs(title = "Soy Km Distance Robustness with Upwind and Downwid") +ylim(-3,3) 
+ labs(color="Legend")




