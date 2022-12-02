install.packages("plm")
library(plm)
## This code provides data management to run an staggered event study manually ##


install.packages('plm')
library(plm)

rm(list=ls())
setwd("~/Desktop/Sandro Research")
data<-read.csv("event_panel_county1.csv")
#write.csv(data,"event_panel_county1.csv")
data<-data[,-c(1:3)]

## Start here after loaded event study data 
unique<-unique(data$fips)


  
# Create balanced panel using make pbalanced function   
#test<-make.pbalanced(data,balance.type = "fill")  
test<-make.pbalanced(data) 
data<-test
#write.csv(test,"event_panel_county2.csv")

# create new dataset to match 
sub<-data[1,]
sub$wellChange<-NA
sub$uogd.4<-NA
sub$uogd.3<-NA
sub$uogd.2<-NA
sub$uogd.1<-NA
sub$uogd<-NA
sub$uogd1<-NA
sub$uogd2<-NA
sub$uogd3<-NA
sub$uogd4<-NA

i=20121

i=20
subset$wellChange[i]<-subset[i,7]-subset[i-1,7]
subset$wells[is.na(subset$wells)]<-0


subset$wellChange<-0
for (i in 1:nrow(subset)) {
  try(subset$wellChange[i]<-subset[i,7]-subset[i-1,7], silent=TRUE)
  subset$uogd.4<-0
  subset$uogd.3<-0
  subset$uogd.2<-0
  subset$uogd.1<-0
  subset$uogd<-0
  subset$uogd1<-0
  subset$uogd2<-0
  subset$uogd3<-0
  subset$uogd4<-0 
}



for (i in unique) {
  subset<-data[data$fips== i,]
  subset<-subset[order(subset$year),]
  subset$wells[is.na(subset$wells)]<-0
  if (sum(subset$wells) > 0){
    subset$wellChange<-0
    for (i in 1:nrow(subset)) {
      try(subset$wellChange[i]<-subset[i,7]-subset[i-1,7], silent=TRUE)
      subset$uogd.4<-0
      subset$uogd.3<-0
      subset$uogd.2<-0
      subset$uogd.1<-0
      subset$uogd<-0
      subset$uogd1<-0
      subset$uogd2<-0
      subset$uogd3<-0
      subset$uogd4<-0
      index<-which.max(subset$wellChange)
      if (length(index) > 0){
        # t=0
        subset$uogd[index]<-1
        # t > 0
        index2<- (index + 1)
        if (index2 <= nrow(subset)){
          subset$uogd1[index2]<-1  
        }
        index3<- (index2 + 1)
        if (index3 <= nrow(subset)){
          subset$uogd2[index3]<-1  
        }
        index4<- (index3 + 1)
        if (index4 <= nrow(subset)){
          subset$uogd3[index4]<-1  
        }
        index5<- (index4 + 1)
        if (index5 <= nrow(subset)){
          subset$uogd4[index5:nrow(subset)]<-1  
        }
        # t < 0
        index.2<- (index - 1)
        if (index.2 >= 1){
          subset$uogd.1[index.2]<-1
        }
        index.3<- (index.2 - 1)
        if (index.3 >= 1){
          subset$uogd.2[index.3]<-1
        }
        index.4<- (index.3 - 1)
        if (index.4 >= 1){
          subset$uogd.3[index.4]<-1
        }
        index.5<- (index.4 - 1)
        if (index.5 >= 1){
          subset$uogd.4[1:index.5]<-1
        }
        
        ## Add bin
      }
    }
  } else {
    subset$wellChange<-0
    subset$uogd.4<-0
    subset$uogd.3<-0
    subset$uogd.2<-0
    subset$uogd.1<-0
    subset$uogd<-0
    subset$uogd1<-0
    subset$uogd2<-0
    subset$uogd3<-0
    subset$uogd4<-0
  } 
  try(sub<-rbind(sub,subset), silent=TRUE)
}
# Let i be an arbitary fips county, if the code holds for i then it assumptively holds for all counties
x1<-sub[sub$fips == 20119,]
x<-sub[sub$fips == 20121,]
sample<-data[data$fips == 20121,]


write.csv(sub,"event_regression_data.csv")

data<-read.csv("event_regression_data.csv")

data<-sub



library(gtools)
library(fixest)
library(texreg)
library(ggplot2)
library(iplots)
library(car)
library(eiCompare)

#rm(list=ls())
#setwd("M:/project/adam/paper 2/input/wells/")
#data<-read.csv("sample_event1.csv")

# standardize at t=-1
data$uogd.1<-0

# Corn Yield
mod<-feols(cornyield~uogd.4+uogd.3+uogd.2+uogd.1+uogd+uogd1+uogd2+uogd3+uogd4+temperature_final+temperature_final^2
           +precipitation_final+precipitation_final^2+as.factor(fips)+as.factor(year)|fips, data=data)

# Coefficient plot
number<-c(-4,-3,-2,-1,0,1,2,3,4)
pre_four<-mod$coefficients[1]
pre_three<-mod$coefficients[2]
pre_two<-mod$coefficients[3]
#pre_one<-mod$coefficients[4]
pre_one<-0
t_zero<-mod$coefficients[4]
post_one<-mod$coefficients[5]
post_two<-mod$coefficients[6]
post_three<-mod$coefficients[7]
post_four<-mod$coefficients[8]
coefs<-c(pre_four,pre_three,pre_two,pre_one,t_zero,post_one,post_two,post_three,post_four)
dat<-as.data.frame(cbind(number,coefs))

ggplot(dat, aes(x=number)) + 
  geom_line(aes(y=coefs, color="Coefficient")) + ylab("") + xlab("") +
  labs(color="Legend") + labs(title = "") +ylim(-3,3) + geom_vline(xintercept = 0)

# Create Confidence Intervals Lower bound
c.4<-confint(mod, 'uogd.4', level=0.95)[1]
c.3<-confint(mod, 'uogd.3', level=0.95)[1]
c.2<-confint(mod, 'uogd.2', level=0.95)[1]
#c.1<-confint(mod, 'uogd.1', level=0.95)[1]
c<-confint(mod, 'uogd', level=0.95)[1]
c1<-confint(mod, 'uogd1', level=0.95)[1]
c2<-confint(mod, 'uogd2', level=0.95)[1]
c3<-confint(mod, 'uogd3', level=0.95)[1]
c4<-confint(mod, 'uogd4', level=0.95)[1]
c.1<-0
conl<-c(c.4,c.3,c.2,c.1,c,c1,c2,c3,c4)

#Upper Bound
c.4u<-confint(mod, 'uogd.4', level=0.95)[2]
c.3u<-confint(mod, 'uogd.3', level=0.95)[2]
c.2u<-confint(mod, 'uogd.2', level=0.95)[2]
#c.1u<-confint(mod, 'uogd.1', level=0.95)[2]
cu<-confint(mod, 'uogd', level=0.95)[2]
c1u<-confint(mod, 'uogd1', level=0.95)[2]
c2u<-confint(mod, 'uogd2', level=0.95)[2]
c3u<-confint(mod, 'uogd3', level=0.95)[2]
c4u<-confint(mod, 'uogd4', level=0.95)[2]
c.1u<-0
conu<-c(c.4u,c.3u,c.2u,c.1u,cu,c1u,c2u,c3u,c4u)
dat<-as.data.frame(cbind(number,coefs,conl,conu))

dat$number<-as.numeric(dat$number)
dat$coefs<-as.numeric(dat$coefs)
dat$conl<-as.numeric(dat$conl)
dat$conu<-as.numeric(dat$conu)

# Plot
ggplot(dat, aes(x=number)) + 
  geom_line(aes(y=conu, color="Confidence Interval")) + 
  geom_line(aes(y=conl, color="Confidence Interval")) + 
  geom_line(aes(y=coefs, color="Coefficient")) + ylab("") + xlab("") +
  labs(color="Legend") + labs(title = "") + ylim(-15,7) + geom_vline(xintercept = 0)

# Event study style plot
ggplot(dat, aes(x = number, y = coefs)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymax = conu, ymin = conl)) + geom_vline(xintercept = -1, col= "red", linetype="dashed") + xlim(-4, 4)



# Soy yield
mod<-feols(soyyield~uogd.4+uogd.3+uogd.2+uogd.1+uogd+uogd1+uogd2+uogd3+uogd4+temperature_final+temperature_final^2
           +precipitation_final+precipitation_final^2+as.factor(fips)+as.factor(year)|fips, data=data)

summary(mod)

# Coefficient plot
number<-c(-4,-3,-2,-1,0,1,2,3,4)
pre_four<-mod$coefficients[1]
pre_three<-mod$coefficients[2]
pre_two<-mod$coefficients[3]
#pre_one<-mod$coefficients[4]
pre_one<-0
t_zero<-mod$coefficients[4]
post_one<-mod$coefficients[5]
post_two<-mod$coefficients[6]
post_three<-mod$coefficients[7]
post_four<-mod$coefficients[8]
coefs<-c(pre_four,pre_three,pre_two,pre_one,t_zero,post_one,post_two,post_three,post_four)
dat<-as.data.frame(cbind(number,coefs))

ggplot(dat, aes(x=number)) + 
  geom_line(aes(y=coefs, color="Coefficient")) + ylab("") + xlab("") +
  labs(color="Legend") + labs(title = "") +ylim(-10,3) + geom_vline(xintercept = 0)

# Create Confidence Intervals Lower bound
c.4<-confint(mod, 'uogd.4', level=0.95)[1]
c.3<-confint(mod, 'uogd.3', level=0.95)[1]
c.2<-confint(mod, 'uogd.2', level=0.95)[1]
c.1<-confint(mod, 'uogd.1', level=0.95)[1]
c<-confint(mod, 'uogd', level=0.95)[1]
c1<-confint(mod, 'uogd1', level=0.95)[1]
c2<-confint(mod, 'uogd2', level=0.95)[1]
c3<-confint(mod, 'uogd3', level=0.95)[1]
c4<-confint(mod, 'uogd4', level=0.95)[1]
conl<-c(c.4,c.3,c.2,c.1,c,c1,c2,c3,c4)

#Upper Bound
c.4u<-confint(mod, 'uogd.4', level=0.95)[2]
c.3u<-confint(mod, 'uogd.3', level=0.95)[2]
c.2u<-confint(mod, 'uogd.2', level=0.95)[2]
c.1u<-confint(mod, 'uogd.1', level=0.95)[2]
cu<-confint(mod, 'uogd', level=0.95)[2]
c1u<-confint(mod, 'uogd1', level=0.95)[2]
c2u<-confint(mod, 'uogd2', level=0.95)[2]
c3u<-confint(mod, 'uogd3', level=0.95)[2]
c4u<-confint(mod, 'uogd4', level=0.95)[2]
conu<-c(c.4u,c.3u,c.2u,c.1u,cu,c1u,c2u,c3u,c4u)
dat<-as.data.frame(cbind(number,coefs,conl,conu))

dat$number<-as.numeric(dat$number)
dat$coefs<-as.numeric(dat$coefs)
dat$conl<-as.numeric(dat$conl)
dat$conu<-as.numeric(dat$conu)
# Plot
ggplot(dat, aes(x=number)) + 
  geom_line(aes(y=conu, color="Confidence Interval")) + 
  geom_line(aes(y=conl, color="Confidence Interval")) + 
  geom_line(aes(y=coefs, color="Coefficient")) + ylab("") + xlab("") +
  labs(color="Legend") + labs(title = "") + ylim(-5,5) + geom_vline(xintercept = 0)

# Event study style plot
ggplot(dat, aes(x = number, y = coefs)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = conu, ymin = conl)) + geom_vline(xintercept = -1, col= "red")

# Example county to detail staggered adoption structure
sample<-data[data$fips ==  20119,]






