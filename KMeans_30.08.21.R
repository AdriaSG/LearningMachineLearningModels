#ML WS1
#K-Means 
#setwd("/Users/adrsanchez/Google Drive/Beuth/Machine Learning I /ML1")

#EX. 1 Key Means 
set.seed(2)
x = matrix(rnorm(50*2), ncol = 2)
x [1:25, 1] <-x[1:25, 1]+2
x [1:25, 1] <-x[1:25, 1]-2
plot(x,pch=16)

#Create two clusters using kmeans
km.out <- kmeans(x, centers = 2, nstart=1) 
km.out$cluster #a vector specifying which cluster each row belongs to
names(km.out) #different output for k means
km.out$totss #the sum of squares without clustering
km.out$tot.withinss #the sum of squares with this clustering
km.out$withinss #the sum of squares within each cluster
km.out$centers #Matrix with the center coordinates
plot(x,col=km.out$cluster+1,pch=16) #plot the poiunts colorfed by cluster
#why +1???
points(km.out$centers,col=2:3,pch=3) #add the cluster centers

#nstart indicates how many times the algorith will repeat using random starting feature configurations
#(points)
km.out <- kmeans(x, centers = 2, nstart=20) 
lot(x,col=km.out$cluster+1,pch=16)
points(km.out$centers,col=2:3,pch=3)
#Measure solution using the sum of squares within each cluster (W)
km.out$tot.withinss #in this case is amller than before ( 80.86996 vs 82.2454 )

#Let's evaluate the same nstart with 3 clusters
set.seed(4)
km.out<-kmeans(x,centers=3,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
km.out<-kmeans(x,centers=4,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
km.out$tot.withinss
# W is not much lower, so increasing clustering leads to a better solution.
#However, the best number of clusters to fit cannot be found by simply by minimising W.

x<-matrix(rnorm(50*3),ncol=2)
x[1:25,1]<-x[1:25,1]+2
x[1:25,2]<-x[1:25,2]-2
x[50+1:25,1]<-x[50+1:25,1]+2
x[50+1:25,2]<-x[50+1:25,2]+2
km.out<-kmeans(x,3,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
points(km.out$centers,col=2:4,pch=3)

#Ex2. Data with 3 clusters 
library(dplyr)
install.packages("dplyr")
library(mdsr)
install.packages("mdsr")

data(world.cities)

names(world_cities)
dim(world_cities)
#Filtering cities with population greater than 100,000
BigCities<-filter(world_cities,population >= 100000)
#And select just longitud and latitude 
BigCities<-select(BigCities,longitude, latitude)
dim(BigCities)

#Now cluster data in 6.
set.seed(15)
city.km<-kmeans(BigCities,centers = 6)
with(BigCities,plot(longitude,latitude,col=city.km$cluster,pch=16,cex=0.6))


