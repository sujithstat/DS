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
class(attenu)
str(attenu)
dim(attenu)
head(attenu)
tail(attenu)


#5
rm(list = ls())
data("airquality")
help("airquality")
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

#7
rm(list=ls())
library(ggplot2)
data("airquality")
ggplot(airquality,aes(y=Wind)) + geom_boxplot() + ggtitle("Boxplot of wind")

#8
rm(list = ls())
data("diamonds")
ggplot(diamonds,aes(x=cut,y=price)) + geom_boxplot() + ggtitle("Boxplot of diamonds")

#9
rm(list=ls())
data("iris")
#windows(width = 4,height = 4)
ggplot(iris,aes(x=Species,y=Sepal.Length)) + geom_boxplot() + ggtitle("Boxplot of Sepal.Length")
ggplot(iris,aes(x=Species,y=Sepal.Width)) + geom_boxplot() + ggtitle("Boxplot of Sepal.Width")
ggplot(iris,aes(x=Species,y=Petal.Length)) + geom_boxplot() + ggtitle("Boxplot of Petal.Length")
ggplot(iris,aes(x=Species,y=Petal.Width)) + geom_boxplot() + ggtitle("Boxplot of Petal.Width")

##### Pract3
####### Simple Linear Regression - 1
#1
rm(list = ls())
data("anscombe")
attach(anscombe)
cor(x1,y1)
cor(x2,y2)
cor(x3,y3)
cor(x4,y4)
model1=lm(y1~x1);model1
model2=lm(y2~x2);model2
model3=lm(y3~x3);model3
model4=lm(y4~x4);model4
par(mfrow=c(2,2))
plot(x = x1,y = y1)
abline(model1)
plot(x = x2,y = y2)
abline(model2)
plot(x = x3,y = y3)
abline(model3)
plot(x = x4,y = y4)
abline(model4)
r1=model1$residuals
r2=model2$residuals
r3=model3$residuals
r4=model4$residuals

plot(predict(model1),r1,abline(h=0))
plot(predict(model2),r2,abline(h=0))
plot(predict(model3),r3,abline(h=0))
plot(predict(model4),r4,abline(h=0))
#From the residual plots we see that only the residuals in the 1st plot are randomly distributed, and in the other plots they follow some pattern. Hence. only the 1st model is adequate.

#2
rm(list = ls())
data("cars")
attach(cars)
cor.test(speed,dist)
# cor=0.8068949,  p-value = 1.49e-12, since p-value < 0.05 we reject H0:rho=0
model1=lm(dist~speed);model1
ggplot(cars,aes(x = speed,y = dist))+geom_point(size=2)+geom_smooth(method = "lm")
summary(model1)
summary(model1)$r.squared
confint(model1)
ggplot(NULL,aes(x = predict(model1),y = model1$residuals))+geom_point(size=2)+geom_hline(yintercept = c(0,0))
shapiro.test(model1$residuals)
qqnorm(model1$residuals)
qqline(model1$residuals)


########Pract4
######## SLR-2
rm(list=ls())
data("cars")
nrow(cars)
samp_size=floor(0.8*nrow(cars));samp_size
set.seed(123)
train_ind=sample(seq_len(nrow(cars)),size = samp_size);train_ind
train=cars[train_ind,]
test=cars[-train_ind,]
trainm=lm(train$dist~train$speed);trainm
summary(trainm)
Res_train=resid(trainm)
b0=coefficients(trainm)[1]
b1=coefficients(trainm)[2]
test_dist=b0+b1*test$speed;test_dist
#fitted values of the test set
Res_test=test$dist-test_dist
#Actual value - fitted value
var.test(Res_train,Res_test)
#H0: sigma1sq=sigma2sq <=> sigma1sq/sigma2sq=1
#Since p-value > 0.05 we do not reject H0
#Therefore, the model is a good fit.

#2
rm(list = ls())
library(ggplot2)
data("mtcars")
attach(mtcars)
mtcars
cor.test(mpg,hp)
# cor=-0.7761684,  p-value = 1.788e-07, since p-value < 0.05 we reject H0:rho=0
model1=lm(mpg~hp);model1
ggplot(mtcars,aes(x = hp,y = mpg))+geom_point(size=2)+geom_smooth(method = "lm")
summary(model1)
summary(model1)$r.squared
confint(model1)
ggplot(NULL,aes(x = predict(model1),y = model1$residuals))+geom_point(size=2)+geom_hline(yintercept = c(0,0))
#
shapiro.test(model1$residuals)
#Normality assumption is valid at 1% los but not valid at 5% los
qqnorm(model1$residuals)
qqline(model1$residuals)




nrow(mtcars)
samp_size=floor(0.75*nrow(mtcars));samp_size
set.seed(123)
train_ind=sample(seq_len(nrow(mtcars)),size = samp_size);train_ind
train=mtcars[train_ind,]
test=mtcars[-train_ind,]
trainm=lm(train$mpg~train$hp);trainm
summary(trainm)
Res_train=resid(trainm)
b0=coefficients(trainm)[1]
b1=coefficients(trainm)[2]
test_mpg=b0+b1*test$hp;test_mpg
#fitted values of the test set
Res_test=test$mpg-test_mpg
#Actual value - fitted value
var.test(Res_train,Res_test)
#H0: sigma1sq=sigma2sq <=> sigma1sq/sigma2sq=1
#Since p-value > 0.05 we do not reject H0
#Therefore, the model is a good fit.

####### Pract5
####### SLR-3
rm(list = ls())
data("airquality")
attach(airquality)
help("airquality")
#Daily air quality measurements in New York, May to September 1973.
class(airquality)
str(airquality)
dim(airquality)
head(airquality)
oz=Ozone
tem=Temp
oz[is.na(oz)]=mean(oz,na.rm = TRUE)    ###########################
tem[is.na(tem)]=mean(tem,na.rm = TRUE) ###########################
data1=data.frame(tem,oz)

library(ggplot2)
cor.test(oz,tem)
# cor=0.608742 ,  p-value < 2.2e-16, since p-value < 0.05 we reject H0:rho=0
model1=lm(oz~tem);model1
#model2=lm(tem~oz);model2

ggplot(data1,aes(x = tem,y = oz))+geom_point(size=2)+geom_smooth(method = "lm")

summary(model1)
summary(model1)$r.squared
confint(model1)
ggplot(NULL,aes(x = predict(model1),y = model1$residuals))+geom_point(size=2)+geom_hline(yintercept = c(0,0))
shapiro.test(rstandard(model1))$p.value
shapiro.test(rstudent(model1))$p.value
qqnorm(model1$residuals)
qqline(model1$residuals)
#Normality assumption is not valid


#2
rm(list = ls())
data("faithful")
attach(faithful)

help("faithful")
#Waiting time between eruptions and the duration of the eruption for the Old Faithful geyser in Yellowstone National Park, Wyoming, USA.
class(faithful)
str(faithful)
dim(faithful)
head(faithful)

y=eruptions
x=waiting
data1=data.frame(y,x)

cor.test(y,x)
# cor=0.9008112,  p-value < 2.2e-16, since p-value < 0.05 we reject H0:rho=0
model1=lm(y~x);model1
ggplot(faithful,aes(x = x,y = y))+geom_point(size=2)+geom_smooth(method = "lm")
summary(model1)
summary(model1)$r.squared
confint(model1)
ggplot(NULL,aes(x = predict(model1),y = model1$residuals))+geom_point(size=2)+geom_hline(yintercept = c(0,0))
shapiro.test(model1$residuals)
#Normality assumption is valid at 5% los
qqnorm(model1$residuals)
qqline(model1$residuals)

nrow(data1)
samp_size=floor(0.75*nrow(data1))#;samp_size
set.seed(123)
train_ind=sample(seq_len(nrow(data1)),size = samp_size)#;train_ind
train=data1[train_ind,]
test=data1[-train_ind,]
trainm=lm(train$y~train$x)#;trainm
#summary(trainm)
Res_train=resid(trainm)
b0=coefficients(trainm)[1]
b1=coefficients(trainm)[2]
test_y=b0+b1*test$x#;test_y
#fitted values of the test set
Res_test=test$y-test_y
#Actual value - fitted value
var.test(Res_train,Res_test)
#H0: sigma1sq=sigma2sq <=> sigma1sq/sigma2sq=1
#Since p-value > 0.05 we do not reject H0
#Therefore, the model is a good fit.


############ Pract6
############ MLR 3
#1
rm(list = ls())
library(tidyverse)
library(datarium)
data("marketing",package = "datarium")
attach(marketing)
summary(marketing)
model1=lm(sales~youtube+facebook+newspaper)
summary(model1)$r.squared
summary(model1)$adj.r.squared
summary(model1)
#Since p-value<0.05 we reject H0:b1=b2=0
#selecting the best model
step(model1,direction = "both")
#newspaper is not significant. Therefore the new fitted model is sales ~ youtube + facebook
#New model
M1=lm(sales~youtube+facebook)
summary(M1)$r.squared
summary(M1)$adj.r.squared
summary(M1)
#Since p-value<0.05 we reject H0:b1=b2=0
ggplot(marketing,aes(x=youtube,y=sales,col=facebook))+geom_point(size=2)+geom_smooth(method = "lm")


y=sales
x1=youtube
x2=facebook
data1=data.frame(y,x1,x2)


#nrow(data1)
samp_size=floor(0.8*nrow(data1))#;samp_size
set.seed(123)
train_ind=sample(seq_len(nrow(data1)),size = samp_size)#;train_ind
train=data1[train_ind,]
test=data1[-train_ind,]
trainm=lm(train$y~train$x1+train$x2)#;trainm
#summary(trainm)
Res_train=resid(trainm)
b0=coefficients(trainm)[1]
b1=coefficients(trainm)[2]
b2=coefficients(trainm)[3]
test_y=b0+b1*test$x1+b2*test$x2#;test_y
#fitted values of the test set
Res_test=test$y-test_y
#Actual value - fitted value
var.test(Res_train,Res_test)
#H0: sigma1sq=sigma2sq <=> sigma1sq/sigma2sq=1
#Since p-value > 0.05 we do not reject H0
#Therefore, the model is a good fit.


library(car)
vif(M1)
#since VIFs are <5 Multicollinearity does not exist

########### Pract7
############ MLR 2 Ridge Regression
rm(list = ls())
data("mtcars")
attach(mtcars)
mtcars$am <- NULL
mtcars$vs <- NULL
model1 <- lm(mpg~.,data = mtcars)
model1
#The fitted MLR model is

library(corrplot)
cor1 <- cor(mtcars)
corrplot.mixed(cor1,lower.col = "black",number.cex=0.7)
library(car)
vif(model1)

#Ridge Regression#
library(glmnet)
x_var <- data.matrix(mtcars[,c("cyl", "disp","hp","drat", "wt","qsec","gear","carb")])
y_var <- mtcars[,"mpg"]
K=seq(0.01,10,0.1)
fit <- glmnet(x_var,y_var,alpha = 0,lambda = K)
ridge_cv <- cv.glmnet(x_var,y_var,alpha=0, lambda = K)
best_K <- ridge_cv$lambda.min
best_K

#Rebuilding the model using optimum value of K.
best_ridge <- glmnet(x_var,y_var,alpha = 0,lambda = best_K)
coef(best_ridge)
#The fitted Ridge Regression model is


######### Pract-8 ##############
######### Cluster Analysis ################
#1
rm(list = ls())
par(mfrow=c(1,2))
data("iris")
na.omit(iris)
dat=iris[-5]
wss=(nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:10) wss[i]=sum(kmeans(dat,centers = i)$withinss)
plot(1:10,wss,type = "b",xlab = "No. of clusters",ylab = "Within groups sum of squares")
#Ward hierarchical clustering
d=dist(dat,method = "euclidean")
fit=hclust(d,method="ward.D")
plot(fit,labels = iris$Species)#cluster dendrogram
groups=cutree(fit,k=3)
rect.hclust(fit,k=3,border = "red")
#k-means clustering
fit_kmeans=kmeans(dat,3)#3-cluster solution
aggregate(dat,by=list(fit_kmeans$cluster),FUN = mean)

CLNO=data.frame(iris,fit_kmeans$cluster);CLNO
#Appending the cluster no.


#2
rm(list = ls())
par(mfrow=c(1,2))
Ht=c(185,170,168,179,182,188,180,183,180)
Wt=c(72,56,60,68,72,77,70,84,88)
Gender=c("M","F","F","M","M","M","F","M","M")
Gend=c(1,0,0,1,1,1,0,1,1)
dat=data.frame(Ht,Wt,Gend)

wss=(nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:nrow(dat)-1) wss[i]=sum(kmeans(dat,centers = i)$withinss)
plot(1:length(wss),wss,type = "b",xlab = "No. of clusters",ylab = "Within groups sum of squares")
#Ward hierarchical clustering
d=dist(dat,method = "euclidean")
fit=hclust(d,method="ward.D")
plot(fit,labels = Gender)#cluster dendrogram
groups=cutree(fit,k=3)
rect.hclust(fit,k=3,border = "red")
#k-means clustering
fit_kmeans=kmeans(dat,3)#3-cluster solution
aggregate(dat,by=list(fit_kmeans$cluster),FUN = mean)

CLNO=data.frame(dat,fit_kmeans$cluster);CLNO
#Appending the cluster no.

######### Pract-9 ##############
######### Cluster Analysis-2 ################
#1
rm(list = ls())
data("mtcars")
#na.omit(mtcars)
dat=mtcars
wss=(nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:10) wss[i]=sum(kmeans(dat,centers = i)$withinss)
plot(1:10,wss,type = "b",xlab = "No. of clusters",ylab = "Within groups sum of squares")
#Ward hierarchical clustering
d=dist(dat,method = "euclidean")
fit=hclust(d,method="ward.D")
plot(fit)#cluster dendrogram
groups=cutree(fit,k=3)
rect.hclust(fit,k=3,border = "red")
#k-means clustering
fit_kmeans=kmeans(dat,3)#3-cluster solution
aggregate(dat,by=list(fit_kmeans$cluster),FUN = mean)

CLNO=data.frame(mtcars,fit_kmeans$cluster);CLNO
#Appending the cluster no.


#2
rm(list = ls())
data("USArrests")
#na.omit(mtcars)
dat=USArrests
wss=(nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:10) wss[i]=sum(kmeans(dat,centers = i)$withinss)
plot(1:10,wss,type = "b",xlab = "No. of clusters",ylab = "Within groups sum of squares")
#Ward hierarchical clustering
d=dist(dat,method = "euclidean")
fit=hclust(d,method="ward.D")
plot(fit)#cluster dendrogram
groups=cutree(fit,k=3)
rect.hclust(fit,k=3,border = "red")
#k-means clustering
fit_kmeans=kmeans(dat,3)#3-cluster solution
aggregate(dat,by=list(fit_kmeans$cluster),FUN = mean)

CLNO=data.frame(USArrests,fit_kmeans$cluster);CLNO
#Appending the cluster no.


#3
rm(list = ls())
library(readxl)
data_for_cluster <- read_excel("workspace/data_for_cluster.xlsx")

dat=data_for_cluster
wss=(nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:10) wss[i]=sum(kmeans(dat,centers = i)$withinss)
plot(1:10,wss,type = "b",xlab = "No. of clusters",ylab = "Within groups sum of squares")
#Ward hierarchical clustering
d=dist(dat,method = "euclidean")
fit=hclust(d,method="ward.D")
plot(fit)#cluster dendrogram
groups=cutree(fit,k=5)
rect.hclust(fit,k=5,border = "red")
#k-means clustering
fit_kmeans=kmeans(dat,3)#3-cluster solution
aggregate(dat,by=list(fit_kmeans$cluster),FUN = mean)

CLNO=data.frame(data_for_cluster,fit_kmeans$cluster);CLNO
#Appending the cluster no.


######## Pract10
######## kNN Classification
#1
rm(list = ls())
data("iris")
dat=iris
n=nrow(dat);n
set.seed(100)
sa=sample(1:n,floor(0.75*n),replace = FALSE)
train=dat[sa,]
cl=train$Species
train=train[,-5]
test=dat[-sa,]
true_lab=test$Species
test=test[,-5]
k_nn=0
library(class)
for(k in 1:10){
  pred=knn(train,test,cl,k)
  incorrect=sum(true_lab != pred)
  mis_rate=sum(incorrect)/length(true_lab)
  cat(k,mis_rate,"\n")
  k_nn[k]=mis_rate
}
k1=which.min(k_nn);k1
test=c(6.5,3.2,4.5,2.1)
knn(train,test,cl,k = k1)
#We conclude that the species is versicolor

#2
rm(list = ls())
library(readxl)
data_for_cluster <- read_excel("data_for_cluster.xlsx")
dat=data_for_cluster
n=nrow(dat);n
set.seed(100)
sa=sample(1:n,floor(0.75*n),replace = FALSE)
train=dat[sa,]
cl=train$copd
train=train[,-13]
test=dat[-sa,]
true_lab=test$copd
test=test[,-13]
k_nn=0
library(class)
for(k in 1:10){
  pred=knn(train,test,cl,k)
  incorrect=sum(true_lab != pred)
  mis_rate=sum(incorrect)/length(true_lab)
  cat(k,mis_rate,"\n")
  k_nn[k]=mis_rate
}
k1=which.min(k_nn);k1
test1=c(1,60,1.67,67,24,0,1,350,1,250,0,0)
test2=c(0,65,1.64,78,29,0,1,50,0,0,0,1)
knn(train,test1,cl,k = k1)
#We conclude that the first person has copd
knn(train,test2,cl,k = k1)
#We conclude that the second person does not have copd

####### Pract11
######## kNN Classification 2
#1
rm(list = ls())
library(ggplot2)
data("diamonds")
dat=diamonds
n=nrow(dat);n
set.seed(100)
sa=sample(1:n,floor(0.75*n),replace = FALSE)
train=dat[sa,]
cl=train$cut
train=train[,-c(2,3,4)]
test=dat[-sa,]
true_lab=test$cut
test=test[,-c(2,3,4)]
#k_nn=0
library(class)
#for(k in 1:200){
#  pred=knn(train,test,cl,k)
#  incorrect=sum(as.character(true_lab) != as.character(pred))
#  mis_rate=sum(incorrect)/length(true_lab)
#  cat(k,mis_rate,"\n")
#  k_nn[k]=mis_rate
#}
#k1=which.min(k_nn);k1
test=c(0.29,57.1,66.2,338,3.99,4.24,2.65)
knn(train,test,cl,k = 200)
#We conclude that the cut is Very Good


#2
rm(list = ls())
data("chickwts")
dat=chickwts
n=nrow(dat);n
set.seed(100)
sa=sample(1:n,floor(0.75*n),replace = FALSE)
train=dat[sa,]
cl=train$feed
train=train[,-2]
train=data.frame(train)
test=dat[-sa,]
true_lab=test$feed
test=test[,-2]
test=data.frame(test)
k_nn=0
library(class)
for(k in 1:10){
  pred=knn(train,test,cl,k)
  incorrect=sum(true_lab != pred)
  mis_rate=sum(incorrect)/length(true_lab)
  cat(k,mis_rate,"\n")
  k_nn[k]=mis_rate
}
k1=which.min(k_nn);k1
test1=225
test2=340
test3=500
knn(train,test1,cl,k = k1)
#We conclude that the feed is linseed
knn(train,test2,cl,k = k1)
#We conclude that the feed is sunflower
knn(train,test3,cl,k = k1)
#We conclude that the feed is casein


####### Pract-12
####### Spam Filtering
#1
rm(list = ls())
HamTot=3768
HamWord=145
SpamTot=1276
SpamWord=15
Tot=HamTot+SpamTot
Word=HamWord+SpamWord
SpamGivenWord=((SpamWord/SpamTot)*(SpamTot/Tot))/(Word/Tot)
HamGivenWord=1-SpamGivenWord
SpamGivenWord
HamGivenWord

#2
rm(list = ls())
HamTot=1978
HamWord=30
SpamTot=595
SpamWord=10
Tot=HamTot+SpamTot
Word=HamWord+SpamWord
SpamGivenWord=((SpamWord/SpamTot)*(SpamTot/Tot))/(Word/Tot)
HamGivenWord=1-SpamGivenWord
SpamGivenWord
HamGivenWord

#3
rm(list = ls())
HamTot=6325-1650
HamWord=162
SpamTot=1650
SpamWord=19
Tot=HamTot+SpamTot
Word=HamWord+SpamWord
SpamGivenWord=((SpamWord/SpamTot)*(SpamTot/Tot))/(Word/Tot)
HamGivenWord=1-SpamGivenWord
SpamGivenWord
HamGivenWord

########pract13
######## Logistic Regression ########
#1
rm(list = ls())
data("mtcars")
help("mtcars")
model1=glm(mtcars$vs~mtcars$hp,family = binomial)
b=coefficients(model1);b
#Logit Transformation:
#log(p/(1-p))= 8.3780240 + (-0.0685607)*x
summary(model1)
#Since p-value < 0.05 we reject H0:b1=0. Therefore, the regression is significant.
ggplot(mtcars,aes(x = hp,y = vs))+geom_point()+stat_smooth(method="glm",se=FALSE,method.args = list(family=binomial))
#as hp increases probability of engine being straight reduces

#2
rm(list = ls())
x=c(0.5,0.75,1.0,1.25,1.5,1.75,1.75,2.0,2.25,2.5,
    2.75,3.0,3.25,3.5,4.0,4.25,4.5,4.75,5.0,5.5)
y=c(rep(0,6),rep(c(1,0),4),rep(1,6))
model1=glm(y~x,family = binomial)
b=coefficients(model1);b
#Logit Transformation:
#log(p/(1-p))= -4.077713  + (1.504645)*x
summary(model1)
#Since p-value < 0.05 we reject H0:b1=0. Therefore, the regression is significant.
ggplot(NULL,aes(x,y))+geom_point()+stat_smooth(method="glm",se=FALSE,method.args = list(family=binomial))
#as Hours of Study increases probability of Passing increases
phat=1/(1+exp(-(b[1]+b[2]*3)));phat
phat=1/(1+exp(-(b[1]+b[2]*4.5)));phat


########pract14
######## Logistic Regression-2 ########
#1
rm(list = ls())
x=c(0.5,0.75,1.0,1.25,1.5,1.75,1.75,2.0,2.25,2.5,
    2.75,3.0,3.25,3.5,4.0,4.25,4.5,4.75,5.0,5.5)
y=c(rep(0,6),rep(c(1,0),4),rep(1,6))
library(pROC)
par(pty="s")
model1=glm(y~x,family = binomial)
roc_curve = roc(y,model1$fitted.values,plot=TRUE,legacy.axes=TRUE)
auc(roc_curve)
#Since auc=0.895 the Logistic Regression model performance is excellent


#2
rm(list = ls())
library(mlbench)
library(ggplot2)
library(pROC)
data("PimaIndiansDiabetes2")
d=na.omit(PimaIndiansDiabetes2)
str(d$diabetes)
levels(d$diabetes)
d$diabetes=as.numeric(d$diabetes)
str(d$diabetes)
d$diabetes=d$diabetes-1
str(d$diabetes)
modela=glm(d$diabetes~d$glucose,family = binomial)
summary(modela)
#Since p-value < 0.05 we reject H0:b1=0. Therefore, the regression is significant.
modelb=glm(d$diabetes~d$pressure,family = binomial)
summary(modelb)
#Since p-value < 0.05 we reject H0:b1=0. Therefore, the regression is significant.
ggplot(d,aes(x = glucose,y = diabetes))+geom_point()+stat_smooth(method="glm",se=FALSE,method.args = list(family=binomial))
ggplot(d,aes(x = pressure,y = diabetes))+geom_point()+stat_smooth(method="glm",se=FALSE,method.args = list(family=binomial))

par(pty="s")

roc_curve_a = roc(d$diabetes,modela$fitted.values,plot=TRUE,legacy.axes=TRUE)
auc(roc_curve_a)
#Since auc=0.8058 the Logistic Regression model performance is very good

roc_curve_b = roc(d$diabetes,modelb$fitted.values,plot=TRUE,legacy.axes=TRUE)
auc(roc_curve_b)
#Since auc=0.6213 the Logistic Regression model performance is good

