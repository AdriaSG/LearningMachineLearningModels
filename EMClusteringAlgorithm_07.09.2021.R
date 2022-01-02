#ML1 WS4
#EM-Clustering Algorithm
find.package("EMCluster")
install.packages("EMCluster")
library("EMCluster")
find.package("FNN")
install.packages("FNN")
library("FNN")
library("rgl")

#Ex1. Soft clustering using EM 
ScaleUSA <- scale(USArrests)
#Apply k-means with 4 clusters
km.out <- kmeans(ScaleUSA, centers=4, nstart=20)
#Get principal components 
pr.out <- prcomp(ScaleUSA); pr.out
#Plot clusters 
plot(pr.out$x[,1:2], type= "n")
text(pr.out$x[,1:2], labels=state.abb, col=km.out$cluster)

#Now with EM 
#Step 1. Set up an initial colution using init
emobj <- init.EM(USArrests, nclass = 4)
#Step 2. Run EM to get an optimal solution 
emclobj <- emcluster(USArrests, emobj, assign.class = TRUE)
#Step 3. Obtain the probabilities of the optimal solution 
emprobs <- round(e.step(USArrests, emobj = emclobj)$Gamma, 3)

#Plot data 
plotem(emclobj,USArrests,lwd=2) # <- not very helpful 
plot(pr.out$x[,1:2], type="n")
text(pr.out$x[,1:2], labels=state.abb, col=km.out$cluster)
#????
round(emprobs[km.out$cluster==1,],2)
round(apply(emprobs[km.out$cluster==1,],2,mean),2)
round(emprobs[km.out$cluster==2,],2)
round(apply(emprobs[km.out$cluster==2,],2,mean),2)
round(emprobs[km.out$cluster==3,],2)
round(apply(emprobs[km.out$cluster==3,],2,mean),2)
round(emprobs[km.out$cluster==4,],2)
round(apply(emprobs[km.out$cluster==4,],2,mean),2)

# Ex2. KNN with one explanatory variable
x<-1:20
y<-x+rnorm(length(x),mean=10)
#Specifies the points where we want to predict our regression function 
#into a data frame.
xgrid<-data.frame(x)
#Then apply KNN using knn.reg from FNN library
knnr.out1 <-  knn.reg(x,y = y , k= 1, test = xgrid)
round(cbind(x,y, fitted.1=knnr.out1$pred),2)

knnr.out3 <-  knn.reg(x,y = y , k= 3, test = xgrid)
round(cbind(x,y, fitted.1=knnr.out3$pred),2)

#What happend if we increase k, which cases correspond to under and over fitting?
xgrid<-data.frame(x=seq(0,21,0.05))
knnr.out <-  knn.reg(x,y=y, k = 4, test = xgrid)
plot(x,y)
lines(xgrid$x, knnr.out$pred)

#Linear regression of the data:
lm.obj <- lm(y~x)
summary(lm.obj)
plot(x,y)
abline(lm.obj, col=2)
knnr.out <- knn.reg(x, test = xgrid, y = y, k = 19)
lines(xgrid[,1], knnr.out$pred, col = 4)
#We can see that linear regression fits data very good 

#Ex3. KNN for multiple explanatory variables 
fitdata<-as.data.frame(matrix(c(1,87, 42,6, 73, 43,7, 66, 44,15,62,54,12,
                                68,45,4,92,46,12,60,50,13,70,46,14,71,54,10,64,47),byrow=T,ncol=3))
names(fitdata)<-c("fitness","weight","lungvol")
summary(fitdata)
#Fitness variable depends of weight and lungvol
lm.fitness <- lm(fitness~weight+lungvol,data=fitdata)
summary(lm.fitness)

m1<-seq(55,95,length=20)
m2<-seq(40,55,length=20)
Xgrid<-expand.grid(weight=m1,lungvol=m2)
pred.grid<-predict(lm.fitness,newdata=Xgrid)
tt<-cbind(Xgrid,pred.grid)
#Perspective plot
res<-persp(m1,m2,matrix(pred.grid,nrow=length(m1)),border=grey(0.6),
           xlab="Weight",ylab="Lung Volume",zlab="fitness",theta=15,phi=20)
points(trans3d(fitdata$weight,fitdata$lungvol,fitdata$fitness,pmat=res),
       pch=16,col = (2:3)[1.5+.5*sign(lm.fitness$residuals)])

#Create a 3D plot for the data 
with(tt, plot3d(lungvol,weight,pred.grid,col="grey80",size=3,zlab="fitness"))
with(fitdata, plot3d(lungvol,weight,fitness,add=T,size=2,col="red",type="s"))

#Fit a KNN model 
X <-  fitdata[,c("weight", "lungvol")]; X
knnr.out <- knn.reg(X, y= fitdata$fitness, k = 3, test = Xgrid)
res<-persp(m1,m2,matrix(pred.grid,nrow=length(m1)),border=grey(0.6),
           xlab="Weight",ylab="Lung Volume",zlab="fitness",theta=15,phi=20)
points(trans3d(fitdata$weight,fitdata$lungvol,fitdata$fitness,pmat=res),
       pch=16,col = c("DarkRed")[1.5+.5*sign(lm.fitness$residuals)])


with(tt, plot3d(lungvol,weight,knnr.out$pred,col="grey80",size=3,zlab="fitness"))
with(fitdata, plot3d(lungvol,weight,fitness,add=T,size=2,col="red",type="s"))

#Ex4. MSE and bias variance trade-off
#R-code to investigate the bias variance trade off

#a) Re-run the $K$ nearest neighbour regression model from Section 1 with k01.
x<-1:20
truey<-x+10
obsy<-truey+rnorm(length(x))
xgrid<-data.frame(x); xgrid

knnr.out<-knn.reg(x,y=obsy,k=1,test=xgrid)

#b) Estimated MSE for this model
plot(x,obsy) #Data points as circles
points(xgrid$x,knnr.out$pred,pch=3) #fitted values as crosses
mean((obsy-knnr.out$pred)^2) #is the model MSE

#mse-true 
mean((truey-knnr.out$pred)^2)

#c) Repeat 10 times 
plot(x,truey,ylim=c(8,33)) 
for(i in 1:10){
  yinloop<-truey+rnorm(length(x))
  knnr.out<-knn.reg(x,y=yinloop,k=1,test=xgrid)
  points(xgrid$x,knnr.out$pred,pch=3) #fitted values as crosses
}
#the estimate have a high variance

#d) Repeat 50 times and store the errors in a matrix
error.mat<-matrix(NA,20,50)
plot(x,truey,ylim=c(8,33)) 
for(i in 1:50){
  yinloop<-truey+rnorm(length(x))
  knnr.out<-knn.reg(x,y=yinloop,k=1,test=xgrid)
  error.mat[,i]<-knnr.out$pred-truey
  points(xgrid$x,knnr.out$pred,pch=3) #fitted values as crosses
}
#estimated bias
apply(error.mat,1,mean)
#squared bias over all points
sum((apply(error.mat,1,mean))^2)
#variance
sum((apply(error.mat,1,var)))

#Repeat the last part increasing k from 1 to 18 

#now we will store the squared bias and variance for k from 1 to 19 
sqbk<-vark<-rep(NA,19)
for(k in 1:19){
  error.mat<-matrix(NA,20,50)
  for(i in 1:50){
    yinloop<-truey+rnorm(length(x))
    knnr.out<-knn.reg(x,y=yinloop,k=k,test=xgrid)
    error.mat[,i]<-knnr.out$pred-truey
  }
  #squared bias over all points
  sqbk[k]<-sum((apply(error.mat,1,mean))^2)
  vark[k]<-sum((apply(error.mat,1,var)))
}
#plot the squared bias against k
plot(sqbk,type="b")
#plot the variance against k
plot(vark,type="b")
#plot the sum of the two 
plot(sqbk+vark,type="b")
#zoom in to find a minimum
plot(sqbk[1:6]+vark[1:6],type="b")



