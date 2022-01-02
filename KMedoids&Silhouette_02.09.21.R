#ML1 WS3
#K Medoids and Silhouette Graph
install.packages("cluster")
library("cluster")

install.packages("ISLR")
library("ISLR")

#Ex1. PAM Clustering (K Medoids)
#Use abbreviations while ploting:
row.names(USArrests)<-state.abb
pairs(USArrests)
#Scale data...but why?
clustmat<-scale(USArrests)
#Apply PAM in scaled data with 4 clusters
pam.out<-pam(clustmat,k=4)
#Plot clusters 
clusplot(pam.out,labels=3,col.p=pam.out$clustering)

#Silhouette 
#The silhouette width measures the similarity of each point to its cluster.
sp <- silhouette(pam.out)
plot (sp, col=1:4)
mean(sp[,"sil_width"])
abline(v=mean( sp[,"sil_width"]))

#Silhouette to determine best number of clusters (like elbow method)
avesw.vec <- rep(NA, 7)
for ( k in 2:7)
  avesw.vec[k] <- mean(silhouette(pam(USArrests, k=k))[,"sil_width"])
plot(1:7, avesw.vec, type="b", ylim=c(0,0.6))

#The larger the silhouette width the better.
#Which number of clusters gives the largest mean silhouette width?
#Largest mean silhouette is at 2 clusters 

#Now repeat PAM at 2 clusters instead of 4 
pam.out <- pam(USArrests, k = 2)
clusplot(pam.out, labels = 3, col.p=pam.out$clustering)
sp <- silhouette(pam.out)
plot (sp, col=1:2)
mean(sp[,"sil_width"])
abline(vmean( sp[,"sil_width"]))
#Avergae silhoute width has increased to .59

#Compare PAM clustering with K means result 
km.out <- kmeans(USArrests, centers = 2 , nstart=20)
table(km.out$cluster, pam.out$clustering)
#Using pam with manhattan distance 
pam.out<-pam(scale(USArrests),k=4,metric="manhattan")
pairs(USArrests,col=pam.out$clustering)

#Ex2. Silhouette plot for k-means
#The first argument to silhouette should be the cluster vector km.out$cluster. 
#The second argument should be a matrix containing the pairwise euclidean distance between each pair of points
sp<-silhouette(km.out$cluster, dist(USArrests))
plot(sp,col=1:2)




x1 <- c(4, 5, 6, 10, 14, 15)
x2 <- c(2, 5, 11, 2, 7, 9)
X <- cbind(x1,x2); X
dist(X)