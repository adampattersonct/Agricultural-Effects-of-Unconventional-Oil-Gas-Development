
########################################
## This script provides well visuals  ##
########################################

# Author: Adam Patterson

rm(list=ls())
library(ggplot2)
library(gridExtra)

# Load data
setwd("M:/project/adam/paper 2/input/wells")
data<-read.csv("Horizontal_Wells_Interpolated.csv")
data5<-read.csv("cashreceipts2012.csv")
data6<-read.csv("upwind_panel3.csv")
data6<-read.csv("update_panel_yield.csv")
data$Well.Count<-1
data1<-data


########################################
####### Total well per state ###########
########################################

enverus<-aggregate(data1$Well.Count, list(data1$State), sum)
enverus<-enverus[enverus$x>4750,]
colnames(enverus)[1]<-"State"
colnames(enverus)[2]<-"Well.Count"
str(enverus)
enverus$State<-as.factor(enverus$State)
enverus$sum1<-475005

c2<-ggplot(enverus,aes(x=State,y=Well.Count,fill=reorder(State, -Well.Count))) + 
  geom_bar(stat="identity", alpha=.3) +labs(title = "UOGD Well Count by State (including States with over 1 % of Total Wells)",y= "Total Well Count", x = "")+ scale_fill_discrete(name = "State") + geom_text(aes(label =paste(round(Well.Count / sum1 * 100, 1), "%")), vjust = -0.2)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
c2

c2<-ggplot(enverus,aes(x=State,y=Well.Count,fill=reorder(State, -Well.Count))) + 
  geom_bar(stat="identity", alpha=.3) +labs(title = "",y= "", x = "")+ scale_fill_discrete(name = "State") + geom_text(aes(label =paste(round(Well.Count / sum1 * 100, 1), "%")), vjust = -0.2)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
c2


########################################
#######  Total Well per year  #########
########################################

well_per_year<-aggregate(data$Well.Count,list(as.factor(data$Spud_Year)),sum)
colnames(well_per_year)[1]<-"Year"
colnames(well_per_year)[2]<-"Total_Wells"
well_per_year1<-well_per_year[well_per_year$Total_Wells>4750,]
well_per_year1$Year<-as.factor(well_per_year1$Year)
well_per_year1$sum1<-475005


c3<-ggplot(well_per_year1,aes(x=Year,y=Total_Wells,fill=reorder(Year, -Total_Wells))) + 
  geom_bar(stat="identity", alpha=.3) +labs(title = "UOGD Well Count by State (including States with over 1 % of Total Wells)",y= "Total Well Count", x = "")+ scale_fill_discrete(name = "Year") + geom_text(aes(label =paste(round(Total_Wells / sum1 * 100, 1), "%")), vjust = -0.2)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
c3

c3<-ggplot(well_per_year1,aes(x=Year,y=Total_Wells,fill=reorder(Year, -Total_Wells))) + 
  geom_bar(stat="identity", alpha=.3) +labs(title = "",y= "", x = "")+ scale_fill_discrete(name = "Year") + geom_text(aes(label =paste(round(Total_Wells / sum1 * 100, 1), "%")), vjust = -0.2)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
c3


########################################
#######   Cash Receipts 2012    ########
########################################

data5<-data5[-1,]
data5$State.receipts.for.all.commodities<-gsub(",","",data5$State.receipts.for.all.commodities)
data5$State<-as.factor(data5$State)
data5$State.receipts.for.all.commodities<-as.numeric(data5$State.receipts.for.all.commodities)
data5$sum1<-sum(data5$State.receipts.for.all.commodities)
data5<-data5[1:29,]

c4<-ggplot(data5,aes(x=State,y=State.receipts.for.all.commodities,fill=reorder(State, -State.receipts.for.all.commodities))) + 
  geom_bar(stat="identity", alpha=.3) +labs(title = "Cash Receipts by State (including States with over 1 % of National Receipts)",y= "Total Cash Receipts", x = "")+ scale_fill_discrete(name = "State") + geom_text(aes(label =paste(round(State.receipts.for.all.commodities / sum1 * 100, 1), "%")), vjust = -0.2)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
c4




########################################
#######   Density of AG yield    #######
########################################

a<-ggplot(data6, aes(x=cornyield)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + ggtitle("") +
  xlab("") + ylab("")

b<-ggplot(data6, aes(x=soyyield)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="turquoise2") + ggtitle("") +
  xlab("") + ylab("")

c<-ggplot(data6, aes(x=cottonyield)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="orange2") + ggtitle("Density of Cotton") +
  xlab("Cotton Yield") + ylab("Density")

grid.arrange(a,b,c, ncol=2)
