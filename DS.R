##### Pract1
##### Data Exploration & Cleaning #######
#1
rm(list=ls())
Z=c(14.5,16.7,18.8,10.3,NA,11.5,19.5,20.6)
mean(Z)
Z[is.na(Z)]=mean(Z,na.rm=TRUE)
Z
mean(Z)
var(Z)
class(Z)
str(Z)
dim(Z)

#2
rm(list=ls())
ht=c(5.1,5.3,4.10,5.6,6.1,5.2,NA,4.5)
wt=c(70,69,55,65,110,NA,60,52)
ht1=na.omit(ht)
wt1=na.omit(wt)
mean(ht1)
var(ht1)
mean(wt1)
var(wt1)
ht[is.na(ht)]=mean(ht,na.rm=TRUE)
wt[is.na(wt)]=mean(wt,na.rm=TRUE)
mean(ht)
var(ht)
mean(wt)
var(wt)
#We notice that after imputation the mean remains the same, but the variance reduces
cor(ht,wt)

#3
rm(list = ls())
data("cars")
help(cars)
#The data give the speed of cars and the distances taken to stop. Note that the data were recorded in the 1920s.
attach(cars)
class(cars)
str(cars)
dim(cars)
head(cars)
tail(cars)

#4
rm(list = ls())
data("attenu")
help("attenu")
attach(attenu)
#This data gives peak accelerations measured at various observation stations for 23 earthquakes in California. The data have been used by various workers to estimate the attenuating affect of distance on ground acceleration.
class(attenu)
str(attenu)
dim(attenu)
head(attenu)
tail(attenu)


#5
rm(list = ls())
data("airquality")
help("airquality")
#Daily air quality measurements in New York, May to September 1973.
attach(airquality)
class(airquality)
str(airquality)
dim(airquality)
head(airquality)
tail(airquality)

aq=na.omit(airquality)
summary(aq)

Ozone[is.na(Ozone)]=mean(Ozone,na.rm=TRUE)
Solar.R[is.na(Solar.R)]=mean(Solar.R,na.rm=TRUE)
Wind[is.na(Wind)]=mean(Wind,na.rm=TRUE)
Temp[is.na(Temp)]=mean(Temp,na.rm=TRUE)
Month[is.na(Month)]=mean(Month,na.rm=TRUE)
Day[is.na(Day)]=mean(Day,na.rm=TRUE)
aqi=data.frame(Ozone,Solar.R,Wind,Temp,Month,Day)
summary(aqi)
cor(Ozone,Solar.R)

##### Pract2
#1
rm(list = ls())
library(ggplot2)
data("cars")
ggplot(cars,aes(x = speed, y = dist))+geom_point()+xlab("speed")+ylab("distance")+ggtitle("Scatter Plot of Speed vs Distance")
ggplot(cars,aes(x = dist))+geom_histogram()+xlab("distance")+ggtitle("Histogram of Distance")
ggplot(cars,aes(x = speed))+geom_histogram()+xlab("speed")+ggtitle("Histogram of speed")

#2
rm(list = ls())
data("women")
ggplot(women,aes(x = height, y = weight))+geom_point()+xlab("height")+ylab("weight")+ggtitle("Scatter Plot of height vs weight")
#ggplot(women,aes(x = height))+geom_histogram()+xlab("weight")+ggtitle("Histogram of weight")

#3
rm(list = ls())
year=2005:2010
sales=c(25,35,20,39,47,58)
data1=data.frame(year,sales)
ggplot(data1,aes(x = year,y = sales))+geom_bar(stat = "identity",width = 0.5)+xlab("year")+ylab("sales")+ggtitle("Bar Plot of yearwise sales")

#4
rm(list = ls())
source1=c("Excise","Customs","Corporation Tax","IT","Others")
taxrev1=c(6528,7109,2566,670,783)
data1=data.frame(source1,taxrev1)
ggplot(data1,aes(x = source1,y = taxrev1))+geom_bar(stat = "identity",width = 0.5)+xlab("source")+ylab("tax revenue")+ggtitle("Bar Plot of tax revenue")

#5
rm(list = ls())
year=c(rep(2015,4),rep(2020,4))
bloodgp=rep(c("O","A","B","AB"),2)
donation=c(2154,926,975,355,2700,1425,1590,760)
data1=data.frame(year,donation,bloodgp)
ggplot(data1,aes(x = year,y = donation,fill=bloodgp))+geom_bar(stat = "identity",position = "dodge")+ggtitle("Bar Plot blood donation")
ggplot(data1,aes(x = year,y = donation,fill=bloodgp))+geom_bar(stat = "identity",position = "stack")+ggtitle("Bar Plot blood donation")

#6
rm(list = ls())
year=2006:2010
hum=c(2810,3542,4301,5362,6593)
sci=c(890,1363,1662,2071,2752)
com=c(540,471,652,895,1113)
data1=data.frame(year,hum,sci,com)
year2=rep(year,3)
std=c(hum,sci,com)
fac=c(rep(c("Humanities"),5),rep(c("Science"),5),rep(c("Commerce"),5))
data2=data.frame(year2,std,fac)
ggplot(data1,aes(x = year,y = hum+sci+com))+geom_bar(stat = "identity")+xlab("year")+ylab("total no. of students")+ggtitle("Bar Plot of total no. of students")
ggplot(data2,aes(x = year2,y = std,fill=fac))+geom_bar(stat = "identity",position = "dodge")+ggtitle("Bar Plot no. of students in different depts")
ggplot(data2,aes(x = year2,y = std,fill=fac))+geom_bar(stat = "identity",position = "stack")+ggtitle("Bar Plot no. of students in different depts")
