#ML WS6 
#Ridge Regression
install.packages("glmnet")
library("glmnet")
library("ISLR")

fix(Hitters)
names(Hitters)
dim(Hitters)
#is.na returns a vector of the same length as the input vector, with a TRUE
#for any elements that are missing
is.na(Hitters)
#sum()+is.na count all of the missing elements
sum( is.na(Hitters))
#Omit observations with NA in the dataset
Hitters=na.omit(Hitters)
dim(Hitters)
#Get to know the data 
hist(Hitters$Salary)
plot(Hitters$Hits, Hitters$Salary)
plot(Hitters$Years, Hitters$Salary)
names(Hitters)

#6.6 James 
#Ridge regression
library(glmnet)
#Define x and y 
x=model.matrix(Salary~ .,Hitters )[,-1]
y=Hitters$Salary
#Grid of values ranging from λ = 10^10 to λ = 10^−2,
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
#Fit with lambda=50
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

#Fit at lambda= 705.4802
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
#Sum of sqrt is greater than when lambda 

#Predict with lambda=50
predict(ridge.mod,s=50,type="coefficients")[1:20 ,]

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train ,],y[train],alpha =0, lambda =grid ,thresh =1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test ,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test ,])
mean((ridge.pred-y.test)^2)


