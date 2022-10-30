###############################################################
##    This script creates a bootstrap using a package rather ##
##    than manually looping many interations of our model    ##                       
###############################################################
# Note: These results were not used in the WP 
# Author : Adam Patterson 


rm(list=ls())
library(gtools)
library(mlr3)
library(mlr3learners)
library(DoubleML)
library(ranger)
library(fixest)
library(texreg)
library(ggplot2)
library(iplots)
library(car)
#setwd("M:/project/adam/paper 2/input/wells/Discrete Yield Mapped")

setwd("M:/project/adam/paper 2/input/wells/Yield Mapped Vertical1/")

data<-read.csv("Upwind_Panel_20km_Spud.csv")


set.seed(1)
# Create two learners (using random forest algorithm) for double machine learning algorithm
lgr::get_logger("mlr3")$set_threshold("warn")
ml_g = lrn("regr.ranger", num.trees = 40, mtry = 1, min.node.size = 2,max.depth = 5)
ml_m = lrn("regr.ranger", num.trees = 40, mtry = 1, min.node.size = 2,max.depth = 5)
ml_r =lrn("regr.ranger", num.trees = 40, mtry = 1, min.node.size = 2,max.depth = 5)


### Soy Yield 
### Positive Control ####
#########################

# Create empty vectors for coefficients and standard errors to concatenate 
coefs<-"NA"
stderr<-"NA"
## Condition data for ML algorithms 
data1<-data[!is.na(data$soyyield),]
#data1<-data1[,c(6,7,52,60:65)]
data1<-data1[,c(6,7,49:50,53,60:66)]
test<-data1
test$year<-as.factor(test$year)
test$fips<-as.factor(test$fips)

for (i in 1:1000) {
    test = test[sample(1:nrow(test), nrow(test), replace = TRUE), ]
    obj_dml_data<-double_ml_data_from_data_frame(test,
                                                 y_col = "soyyield",
                                                 d_cols = "upwnd_well",  x_cols= c("fips","year","upwnd_well_vertical"),                         
                                                 use_other_treat_as_covariate = FALSE)
    doubleml_plr = DoubleMLPLR$new(obj_dml_data,ml_g, ml_m,n_folds = 10,score = "partialling out", dml_procedure= "dml2")
    doubleml_plr$fit()
    co<-doubleml_plr$coef
    se<-doubleml_plr$se
    coefs<-c(coefs,co)
    stderr<-c(stderr,se)
}
stderr<-stderr[-1]
coefs<-coefs[-1]
num<-1:40
dat<-as.data.frame(cbind(coefs,stderr))
str(dat)

i <- sapply(dat, is.factor)
dat[i] <- lapply(dat[i], as.character)


dat$coefs<-as.numeric(dat$coefs)
dat$num<-as.numeric(dat$num)
dat$stderr<-as.numeric(dat$stderr)

hist(as.numeric(stderr))
hist(as.numeric(coefs))

a<-ggplot(dat, aes(x=coefs)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + ggtitle("") +
  xlab("") + ylab("")

p<-ggplot(dat, aes(x=coefs)) + 
  geom_histogram(color="black", fill="white")

### Corn Yield 
### Positive Control ####
#########################

# Create empty vectors for coefficients and standard errors to concatenate 
coefs1<-"NA"
stderr1<-"NA"

## Condition data for ML algorithms 
data1<-data[!is.na(data$cornyield),]
data1<-data1[,c(6,7,49:50,52,60:66)]
test<-data1
test$year<-as.factor(test$year)
test$fips<-as.factor(test$fips)

for (i in 1:40) {
  data<-as.data.frame(my.data[i])
  ## Condition data for ML algorithms 
  data1<-data[!is.na(data$cornyield),]
  data1<-data1[,c(6,7,49:50,52,60:66)]
  test<-data1
  test$year<-as.factor(test$year)
  test$fips<-as.factor(test$fips)
  obj_dml_data<-double_ml_data_from_data_frame(test,
                                               y_col = "cornyield",
                                               d_cols = "dwnwnd_well", x_cols= c("fips","year","dwnwnd_well_vertical"),                       
                                               use_other_treat_as_covariate = FALSE)
  doubleml_plr = DoubleMLPLR$new(obj_dml_data,ml_g, ml_m,n_folds = 10,score = "partialling out")
  doubleml_plr$fit()
  co<-doubleml_plr$coef
  se<-doubleml_plr$se
  coefs1<-c(coefs1,co)
  stderr1<-c(stderr1,se)
}
stderr1<-stderr1[-1]
coefs1<-coefs1[-1]

     