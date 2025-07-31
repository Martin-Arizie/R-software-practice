##REGRESSION 

library(GGally)
library(car)
library(caret)
data1<-read.csv(file.choose(),header=T)
data1
data1
head(data1)
tail(data1)
summary(data1[,3:9])
windows(width=20,height=10)
ggpairs(data1[,3:9])
##tranforming data using log transform##
y<-log(data.matrix(data1[,3]))
data_sample<-log(data.frame(data.matrix(data1[,4:9])))
y
####droping correlated columns###
data_sample$B1<-NULL
data_sample$B<-NULL
###train and test set###
set.seed(2016)
N=nrow(data_sample)
train<-sample(1:N,550,FALSE)
train
y_train<-y[train]
y_test<-y[-train]
data_train<-data_sample[train,]
data_test<-data_sample[-train,]
###50 to test the model
nrow(data_test)
### use 550 to the model##
nrow(data_train)
##Train model using trainset##
fit<-lm(y_train~.,data=data_train)
fit
summary(fit)
fit1<-lm(y_train~L1+B2+T,data=data_train)
fit1
summary(fit1)
### evalute model performance##
## check for non-linear pattern in the residuals ##
plot(fit1,which=1)
plot(fit)
#### check the distribution of the residuals##
par(mfrow=c(2,1))
plot(density(fit1$residuals),main="Residuals",xlab="value")
plot(fit1,which=2)
### shapiro normality test##
shapiro.test(fit1$residuals)
### homoscadascity##
plot(fit1,which=3)
library(lmtest)
library(car)
###Breuch pagan test##
bptest(fit1)
pred<-predict(fit1,data_test)
pred
r2<-round(cor(y_test,pred)^2,3)
r2

