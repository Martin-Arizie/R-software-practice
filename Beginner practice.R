

age1 <-c(23,43,31,29)
age2<-c(33,45,355,39)
age3<-c(30,25,15,10)
age4<-c(5,2,7,17)

age1-age2
age1/age2
age1^age2
x1<-matrix(c(4,3,9,2),nrow=2,ncol=2,byrow=T)
x1
x2<-matrix(c(3,5,1,-3),nrow=2,ncol=2,byrow=TRUE)
x3<-matrix(c(5,10,2,9,7),nrow=3,ncol=2,byrow=FALSE)
x1
x2
x3
x1+x2
x1-x2
x1*x2
x1%*%x2
x1%*%x3
x1*x2

t(x1)
diag(x1)
solve(x2)
a = matrix(c(3,-4,2,10),nrow=2,ncol=2,byrow=T)
c = c(-6,34)
b = solve(a)
d = b%*%c
r = solve(a,c)
mat = cbind(age1,age2,age3,age4)
mat1 = rbind(age1,age2,age3,age4)
mat2 = data.frame(age1,age2,age3,age4)
mat3 = list(age1,age2,age3,age4)
mat
mat1
mat2
mat3

###########PACKAGES###########
library(datasets)
data(mtcars)
as = mtcars
data(women)
rho = women
rho
head(as,n = 10)
tail(as,n = 10)

##########IF THE PACKAGES ARE NOT IN THE BASE OR LOCAL CRANE, IMPORT FROM#######
install.packages(" tidyverse")

###########HOW TO GENERATE DATA THROUGH SIMULATIONS#############

#####IF YOU WANT TO GENERATE psuedo random numbers base on NORMAL DISTRIBUTION####
rnorm(10,0,1)

#####IF YOU WANT TO GENERATE psuedo random numbers base on uniform DISTRIBUTION####
runif(10,50,80)

#####IF YOU WANT TO GENERATE psuedo random numbers base on binomial DISTRIBUTION####
rbinom(10,1,0.50)

#####IF YOU WANT TO GENERATE psuedo random numbers base on exponential DISTRIBUTION####
rexp(10,8)

#####IF YOU WANT TO GENERATE psuedo random numbers base on poisson DISTRIBUTION####
rpois(10,50)

ASSIGNMENT 1
Using any suitable function, extract only the weight from the data( posted on the platform)




as[ ,c(1,40)]
as$wt will extract only the weight

subsetting
attach(as)

another way
su = subset(as,disp>=400|disp<100,select=c(hp,disp))


ESTIMATION OF LINEAR MODELS IN R
lm(y~x,data = sa or ta)
lm(y~x,data = ta)

al = aov(y~as.factor(T), data = as)
summary(al)

Name   weight1    weight2      weight3
Sabina   48kg      50kg         79kg
Rhoda     14kg     56kg         43kg
George     60kg    

ASSIGMENT 2
Use the described function to compute the descriptive statistics of the data ( posted on the platform)


GO AND INSTALL A PACKAGE CALL dplyr


MEAN(u)
x<-c(0,7,20,30,17,9,4)
b = sum(x)
a = length(x)
c = b/a
c


VARIANCE
d = sum(x-mean(x))**2  wrong
d = sum((x-mean(x))**2)  ###correct
var = d/(a-1)
var

var(x)


HOW TO SAMPLE IN R

sample(a, n , replacement)
T = c(70,40,30,25,45,47,15)
da =sample(T, 5, replace ="T")
da

set.seed(100)




























setwd("C:/Users/HP G2/Desktop/second semester courses")
getwd()
Data<-read.csv("data2.csv")
Data
su = subset(Data,select=c("wt1","wt2","wt3"))
su
lapply(Data[,c("wt1","wt2","wt3")],FUN=summary)
lapply(Data[,c("wt1","wt2","wt3")],FUN=plot)
lapply(Data[,c("wt1","wt2","wt3")],FUN=fivenum)
lapply(Data[,c("wt1","wt2","wt3")],FUN=mean)
lapply(Data[,c("wt1","wt2","wt3")],FUN=variance)

sapply(Data[,c("wt1","wt2","wt3")],FUN=summary)
sapply(Data[,c("wt1","wt2","wt3")],FUN=plot)
sapply(Data[,c("wt1","wt2","wt3")],FUN=fivenum)
sapply(Data[,c("wt1","wt2","wt3")],FUN=mean)

tapply(Data[,c("wt1","wt2","wt3")],INDEX = Data$Tdelivery, FUN=summary)
ctapply(INDEX = Data$Tdelivery, FUN=mean(Data[,c("wt1","wt2","wt3")],na.rm = FALSE))







############STS612 14/06/2024##############

#####WHEN A PARTICULAR ACTION IS REPEATED FOR A NUMBER OF TIMES IT IS CALLED LOOP
When a avariable is set to zero,it is a vector
############## THIS THE "for LOOP"

k=c(1,2,3,4,5,6,7,8,9,0)
sa=0
for(i in 1:1000){
s = sample(k,7,replace = F)
sa[i] = mean(s)
}
mean(sa)
mean(k)

NOTE: IT IS THE MEAN OF n SAMPLE MEANS THAT IS UNBIASED ESTIMATOR OF THE POPULATION MEANS#########

#############  while loop #######
d = 9
while(d>0){
print(d)
d = d-1
}


 ALTENATIVE
f = 9:1
print(f)

ASSIGNMENT 1
Use any other function to write a 



##########  GENERATING ODD NUMBERS  #####################
for (i in 1:1000){
if(!i%%2)
print(i)
next
}

######### When the exclamation mark, it will produce odd numbers  ####### 
for (i in 1:1000){
if(i%%2)
print(i)
next
}



  ################## we want to determine the number of elements in a set########
   ############## Conditional execution ################
   ###### Determined the number of elements greater than the mean of vector ja#######
   ############## CONDITIONAL EXECUTION ###########

ja<- c(40,20,30,60,35)
meanja<-mean(ja)
counter<-0
for(i in 1:length(ja)){
if(ja[i]>meanja)
{counter <- counter+1
}
}
counter

########## BRANCHING CONDITIONS #####
SEPARATING THE ELEMENTS LESS THAN AND ELEMENTS MORE THAN MEANS IN TO TWO VECTORS #####

###Separating the elements in ja into different vecter#######

lessmean<-0
greatermean<-0
j<-0
k<-0
for(i in 1:length(ja))
{if(ja[i]<-mean(ja))
{j<-j+1
lessmean[j]<-ja[i]
}else
{k<-k+1
greatermean[k]<-ja[i]
}
}
greatermean
lessmean




setwd("C:/Users/HP G2/Desktop/second semester courses")
getwd()
Data<-read.csv("data2.csv")
Data
su = subset(Data,select=c("wt1","wt2","wt3"))
su
lapply(Data[,c("wt1","wt2","wt3")],FUN=summary)
lapply(Data[,c("wt1","wt2","wt3")],FUN=plot)
lapply(Data[,c("wt1","wt2","wt3")],FUN=fivenum)
lapply(Data[,c("wt1","wt2","wt3")],FUN=mean)
lapply(Data[,c("wt1","wt2","wt3")],FUN=variance)

sapply(Data[,c("wt1","wt2","wt3")],FUN=summary)
sapply(Data[,c("wt1","wt2","wt3")],FUN=plot)
sapply(Data[,c("wt1","wt2","wt3")],FUN=fivenum)
sapply(Data[,c("wt1","wt2","wt3")],FUN=mean)

tapply(Data[,c("wt1","wt2","wt3")],INDEX = Data$Tdelivery, FUN=summary)
ctapply(INDEX = Data$Tdelivery, FUN=mean(Data[,c("wt1","wt2","wt3")],na.rm = FALSE))







############STS612 14/06/2024##############

#####WHEN A PARTICULAR ACTION IS REPEATED FOR A NUMBER OF TIMES IT IS CALLED LOOP
When a avariable is set to zero,it is a vector
############## THIS THE "for LOOP"

k=c(1,2,3,4,5,6,7,8,9,0)
sa=0
for(i in 1:1000){
s = sample(k,7,replace = F)
sa[i] = mean(s)
}
mean(sa)
mean(k)

NOTE: IT IS THE MEAN OF n SAMPLE MEANS THAT IS UNBIASED ESTIMATOR OF THE POPULATION MEANS#########

#############  while loop #######
d = 9
while(d>0){
print(d)
d = d-1
}


 ALTENATIVE
f = 9:1
print(f)

Project 1
Use any other function to write a 



##########  GENERATING ODD NUMBERS  #####################
for (i in 1:1000){
if(!i%%2)
print(i)
next
}

######### When the exclamation mark REMOVED, it will produce odd numbers  ####### 
for (i in 1:1000){
if(i%%2)
print(i)
next
}



  ################## we want to determine the number of elements in a set########
   ############## Conditional execution ################
   ###### Determined the number of elements greater than the mean of vector ja#######
   ############## CONDITIONAL EXECUTION ###########

ja<- c(40,20,30,60,35)
meanja<-mean(ja)
counter<-0
for(i in 1:length(ja)){
if(ja[i]>meanja)
{counter <- counter+1
}
}
counter

########## BRANCHING CONDITIONS #####
SEPARATING THE ELEMENTS LESS THAN AND ELEMENTS MORE THAN MEANS IN TO TWO VECTORS #####

###Separating the elements in ja into different vecter#######

lessmean<-0
greatermean<-0
j<-0
k<-0
for(i in 1:length(ja))
{if(ja[i]<-mean(ja))
{j<-j+1
lessmean[j]<-ja[i]
}else
{k<-k+1
greatermean[k]<-ja[i]
}
}
greatermean
lessmean


setwd("C:/Users/HP G2/Desktop/second semester courses")
getwd()
Data<-read.csv("data2.csv")
Data
su = subset(Data,select=c("wt1","wt2","wt3"))
su
lapply(Data[,c("wt1","wt2","wt3")],FUN=summary)
lapply(Data[,c("wt1","wt2","wt3")],FUN=plot)
lapply(Data[,c("wt1","wt2","wt3")],FUN=fivenum)
lapply(Data[,c("wt1","wt2","wt3")],FUN=mean)
lapply(Data[,c("wt1","wt2","wt3")],FUN=variance)

sapply(Data[,c("wt1","wt2","wt3")],FUN=summary)
sapply(Data[,c("wt1","wt2","wt3")],FUN=plot)
sapply(Data[,c("wt1","wt2","wt3")],FUN=fivenum)
sapply(Data[,c("wt1","wt2","wt3")],FUN=mean)

tapply(Data[,c("wt1","wt2","wt3")],INDEX = Data$Tdelivery, FUN=summary)
ctapply(INDEX = Data$Tdelivery, FUN=mean(Data[,c("wt1","wt2","wt3")],na.rm = FALSE))







############STS612 14/06/2024##############

#####WHEN A PARTICULAR ACTION IS REPEATED FOR A NUMBER OF TIMES IT IS CALLED LOOP
When a avariable is set to zero,it is a vector
############## THIS IS THE "for LOOP"

k=c(1,2,3,4,5,6,7,8,9,0)
sa=0
for(i in 1:1000){
s = sample(k,7,replace = F)
sa[i] = mean(s)
}
mean(sa)
mean(k)

NOTE: IT IS THE MEAN OF n SAMPLE MEANS THAT IS UNBIASED ESTIMATOR OF THE POPULATION MEANS#########

#############  while loop #######
d = 9
while(d>0){
print(d)
d = d-1
}


 ALTENATIVE
f = 9:1
print(f)

ASSIGNMENT 1
Use any other function to write a 



##########  GENERATING ODD NUMBERS  #####################
for (i in 1:1000){
if(!i%%2)
print(i)
next
}

######### When the exclamation mark, it will produce odd numbers  ####### 
for (i in 1:1000){
if(i%%2)
print(i)
next
}



  ################## we want to determine the number of elements in a set########
   ############## Conditional execution ################
   ###### Determined the number of elements greater than the mean of vector ja#######
   ############## CONDITIONAL EXECUTION ###########

ja<- c(40,20,30,60,35)
meanja<-mean(ja)
counter<-0
for(i in 1:length(ja)){
if(ja[i]>meanja)
{counter <- counter+1
}
}
counter

########## BRANCHING CONDITIONS #####
SEPARATING THE ELEMENTS LESS THAN AND ELEMENTS MORE THAN MEANS IN TO TWO VECTORS #####

###Separating the elements in ja into different vector#######

ja<- c(40,20,30,60,35)
lessmean<-1
greatermean<-1
l<-1
t<-1
for(i in 1:length(ja))
{if(ja[i]<mean(ja))
{l<-l+1
lessmean[l]<-ja[i]
}else
{t<-t+1
greatermean[t]<-ja[i]
}
}
A<-greatermean
A
B<-lessmean
B
c("A,B")


project 2
X~EXP(0,5)
e~N(0,1)
y = 5=2x+e , n = 100
f(x) = y + 20, if y<mean(y)
f(x) = y^2 +y/4 +2, otherwise

find;
mean (f(y1))
mean (f(y2))

###################  21/06/2024 ###########
################# Lecture three ###########

me = function(s){
s=c(1:100)
ab = sum(s)
b = length(s)
a = ab/b
return(a)
}
print(a)
c(0,1,2,3,4,5,6,7,8,9)->x
me(x)

sum(1:100)
5050/100
###############variance of a dataset################3
x > c(0,1,2,3,4,5,6,7,8,9)
me(x)
va=function(x){
disp=sum((x-mean(x))^2)
n=length(x)-1
v=disp/n
return(v)
}

assign("geogina",c(75,45,1,14,30,29))
mean(geogina)
var(geogina)
v = sum((x-mean(x))^2)/length(x)-1

sum((x-mean(x))^2)/length(x)-1
standard deviation 

####### FUNCTION TO COMPUTE BIAS####

Rhoda = function(x,n){
h = mean(x)
s = sample(x,n,replace=F)
t = mean(s)
d = h-t
return(d)
}
rnorm(500,1,5)
p=rnorm(500,1,5)
q=300
q
Rhoda(p,q)


########## HOW TO DO MULTIPLE SAMPLES ######
Rhoda = function(x,n,r){
h = mean(x)
k=0
for(i in 1:r){
s = sample(x, n, replace=F)
k[i] = mean(s)

}
t = mean(k)
d = h-t
return(d)
}
rnorm(500,1,5)
p=rnorm(500,1,5)
q=300
r=100
print(q)
Rhoda(p,q,r)



########### WRITE A FUNCTION IN R THAT ADDS NUMBERS ########

martin = function(x,n){
s = sample(x,n,replace=F)
t = mean(s)
y = var(s)
cv = y/t
return(cv)
}
rnorm(3000,0,1)
