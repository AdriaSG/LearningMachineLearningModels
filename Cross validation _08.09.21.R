#ML1 WS5 
#Ex1.1 - Cross validation 
install.packages("boot")
library("boot")
library("ISLR")
#Understand the data 
?Auto
names(Auto)
plot(Auto$cylinders , Auto$mpg)
#The as.factor() function converts quantitative variables into qualitative
Auto$cylinders = as.factor(Auto$cylinders)
attach(Auto)

plot(cylinders, mpg, col=2, varwidth=T, xlab="cylinders",
     ylab ="MPG ")
hist(mpg,col=2,breaks=15)
pairs(Auto)
?pairs()
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower,mpg)
#Use identify function to select data points to be shown 
identify(horsepower,mpg,name)
summary(Auto)
savehistory() #Next time we can load commands history with loadhistory()


#Ex1.2. Quadratic regression 
plot(mpg~horsepower,data=Auto)
lm.hp =lm(mpg~horsepower ,data=Auto)
abline(lm.hp)
lm.quad.hp =lm(mpg~horsepower+I(horsepower^2) ,data=Auto)
summary(lm.quad.hp)
#define the predictor function and call it fq
fq<-function(x)
  lm.quad.hp$coefficients[1]+lm.quad.hp$coefficients[2]*x+
  lm.quad.hp$coefficients[3]*x^2
curve(fq,40,230,add=TRUE)
#fq is a R function coding the quadratic predictor function fq(x) = b0 + b1x + b2x2
#The curve command plots this function between the values x = 40 and x = 230.

#Ex1.3 
set.seed(1)
#Sample data, 196 observations will be our training set 
train=sample(392,196)
#Apply linear model in the train data using subset
lm.fit=lm(mpg ~ horsepower, data=Auto ,subset =train )
#Calculate MSE of the validation set
#-train select only observations out of the training set 
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#Use poly() to repeat the same with quadratic and cubic regression 
lm.fit2=lm(mpg ~ poly(horsepower ,2), data=Auto ,subset =train )
mean((mpg-predict(lm.fit2 ,Auto))[-train ]^2)
lm.fit3=lm(mpg ~ poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg-predict(lm.fit3 ,Auto))[-train ]^2)
#If we apply the same on different data MSE values will change 
set.seed(2)
train=sample(392,196)

#By this we can conclude that the quadratic reg(20.43036) fits better than cubic (20.38533)
#and linear regression (25.72651)

#Leave one out Cross Validation (LOOCV)
#Fit simple linear regression but using glm so it can be used then with cv from boot library 
glm.fit=glm(mpg ~ horsepower, data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
#Cross-validation results are the same:24.23151, 24.23114
#Let's repeat regression adding more complex polynomials
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg ~ poly(horsepower ,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
  }
cv.error
# By the error calculated we see a sharp drop in the estimated test MSE between
# the linear and quadratic fits, but then no clear improvement from using
# higher-order polynomials.

#K-folds cross-validation
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg ~ poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
  }
cv.error.10

#Ex2. Model selection using cross-validation 
#Use the leave-one-out cross-validation MSE score for model selection.
#Step 1. Define the minimal model 
glm.fit=glm(mpg~horsepower +I(horsepower^2) , data=Auto)
cv.err =cv.glm(Auto ,glm.fit)
cv.err$delta

cv.error=rep (0,6)
cv.error[1] =cv.glm(Auto ,glm.fit)$delta[1]
glm.fit=glm(mpg~horsepower +I(horsepower ^2)+ year, data=Auto)
cv.error[2] =cv.glm(Auto ,glm.fit)$delta[1]
plot(cv.error,type="b")

detach(Auto)