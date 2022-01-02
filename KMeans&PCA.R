#ML WS2 
#K-Means and PCA to determine clusters number
install.packages("rgl", dependencies=TRUE)
find.package("rgl")
library(rgl)

#Ex1
#a) Understand the code 
set.seed(11) #set seed to get same results over diff. trials 
x=matrix(rnorm(75*3),ncol=3)
x[1:25,1]=x[1:25,1]+5
x[51:75,2]=x[51:75,2]-6; x 
truth<-rep(1:3,c(25,25,25)); truth 
pairs(x,col=truth+1)
#3d plot:
plot3d(x,size=5)

#c) Run k-means for data with 3 clusters 
km.out <- kmeans(x, centers=3, nstart=20)
#nstart defines how many times algorithm will be repeated using random feature configurations 
#or random initial points 

#d) Compare the output clusters with the known clusters 
table (km.out$cluster, truth)

#e) Plot with colors per cluster 
plot3d(x,col=km.out$cluster+1,size=5)
#and add centroids 
plot3d(km.out$centers,add=TRUE,col=2:4,type="s")

#f) Repeat above with just 2 clusters 
km.out <- kmeans(x, centers=2, nstart=20)
plot3d(x,col=km.out$cluster+1,size=5)
plot3d(km.out$centers,add=TRUE,col=2:4,type="s")

#Idk how to describe it 

#g) Generate a new matrix called y with 10 columns instead of 3, define the clusters in exactly the
#same way as above. Run kmeans with 3 centres on x and on y. Use the R function table to
#compare how many rows have been correctly and incorrectly assigned.
set.seed(11) #set seed to get same results over diff. trials 
y=matrix(rnorm(75*10),ncol=10)
y[1:25,1]=y[1:25,1]+5
y[51:75,2]=y[51:75,2]-6; y 
truth<-rep(1:10,c(25,25,25)); truth 
pairs(x,col=truth+1)
#3d plot:
plot3d(x,size=5)

#Apply k means 
km.out <- kmeans(x, centers=3, nstart=20)
plot3d(x,col=km.out$cluster+1,size=5)
plot3d(km.out$centers,add=TRUE,col=2:4,type="s")
table (km.out$cluster, truth)

#What is the difference???

#Ex 2. USA arrests 
help("USArrests")
#Data Exploration 
#a)
names(USArrests)
#b)
summary[mean(USArrests)]
summary$mean(USArrests)
#Mean and sd for all variables 
colMeans(USArrests)
apply(USArrests, 2, sd)
apply(USArrests, 2, sd)
#c) Get principal components for this dataset 
pr.out <- prcomp(USArrests, scale = TRUE)
#d) 
names(pr.out)
pr.out$scale 
pr.out$sdev
pr.out$rotation
pr.out$center

dim(pr.out$x)
pr.out$x
mean(pr.out$x[,1])
apply(pr.out$x, 2, sd)

#f)
biplot(pr.out, xlabs=state.abb, scale = 0)
pr.var=pr.out$sdev^2
plot(pr.var, type = "b")
abline(h=1)
sum(pr.var)
pve=pr.var/sum (pr.var)
plot (pve, ylim = c( 0,1), type = "b")
plot(cumsum(pve), ylim= c(0,1), type = "b")
abline(h=0.8)
#Then we know that the first two principal
#components explain 87% of the variance, so we will use just the first two.

#g) K means in PCA data 
km.out <- kmeans(pr.out$x, center = 2, nstart= 20)
plot(pr.out$x[,1:2], type = "n")
text(pr.out$x[,1],pr.out$x[,2],labels=state.abb,col=km.out$cluster)

km.out<-kmeans(pr.out$x,centers=3,nstart=20)
plot(pr.out$x[,1:2],type="n")
text(pr.out$x[,1],pr.out$x[,2],labels=state.abb,col=km.out$cluster)

#h)
wss.vec<-rep(NA,10)
for(k in 1:10){
  km.out<-kmeans(pr.out$x,centers=k,nstart=20)
  wss.vec[k]<-km.out$tot.withinss
}
plot( wss.vec, type="b") 
#Elbow at 4 

#i) Plot principal components coulored by cluster 
km.out <- kmeans(pr.out$x[,1:2], centers=4, nstart=20)
plot(pr.out$x[,1:2], type= "n")
text(pr.out$x[,1],pr.out$x[,2],labels=state.abb,col=km.out$cluster)
pairs(USArrests,col=km.out$cluster)
#Size of clusters 
km.out$size
km.out$withinss

#j) Apply clustering without PCA to compare:
wss.vec<-rep(NA,10)
for(k in 1:10){
  km.out<-kmeans(USArrests,centers=k,nstart=20)
  wss.vec[k]<-km.out$tot.withinss
}
plot( wss.vec, type="b") #Indicates elbow at 3 

km.out <- kmeans(USArrests, centers=3, nstart=20)
km.out$size
km.out$withinss
pairs(USArrests,col=km.out$cluster)






