# Spatial Clustering

require(sp) # Spatial methods
require(cluster) # Clustering
require(e1071) # Support Vector Machines, The Interface to libsvm in package e1071
require(RColorBrewer) # Color palettes

source("optK.R")

## First aspatial clustering

x <- rbind(cbind(rnorm(100,0,0.5), rnorm(100,0,0.5)), cbind(rnorm(150,5,0.5), rnorm(150,5,0.5))) 
( clust <- OptK(x, 20, plot=TRUE, cluster=TRUE) )

clust$medoids
pam(x[,1], 1)$medoids

x <- data.frame(x, CLUSTER=clust$clustering)
x <- data.frame(x, SIL=clust$silinfo$widths[,"sil_width"])
head(x)

par(mfrow=c(2,2))
plot(x$X1, x$X2, pch=19, col=factor(x$CLUSTER), main="Cluster membership",    
     xlab="Cluster 1", ylab="Cluster 2")
legend("topleft", legend=c("Cluster 1", "Cluster 2"),      
       col=c("black","red"), pch=c(19,19), bg="white")
plot(silhouette(clust), col = c("red", "green"), main="Cluster mean silhouette's")
plot(clust, which.plots=1, main="K-MEDIOD FIT")

n=20
wss <- (ncol(x)-1)*sum(apply(x,2,var))
for (i in 2:n) wss[i] <- sum(kmeans(x,centers=i)$withinss)
plot(1:n, wss, type="b", main=paste("Optimal cluster", 
     which(wss == max(wss)), sep=" N="), xlab="Number of Clusters", ylab="Within groups sum of squares")

fit <- kmeans(x, 2)

clusplot(x, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

x <- rbind(cbind(rnorm(100,0,0.5), rnorm(100,0,0.5)), cbind(rnorm(150,5,0.5), rnorm(150,5,0.5)))
x <- rbind(x, cbind(runif(100,min(x),max(x)), runif(100,min(x),max(x))))
plot(x,pch=19)

library(e1071)
( fcm <- cmeans(x, centers=2, dist="euclidean", method="cmeans") )
fcm$withinerror

x <- data.frame(x, CLUSTER=fcm$cluster, fcm$membership)
names(x) [4:5] <- c("ProbsC1","ProbsC2")

par(mfrow=c(1,2))
plot(x$X1, x$X2, pch=19, col=factor(x$CLUSTER), main="Cluster membership", xlab="Cluster 1", ylab="Cluster 2")
legend("topleft", legend=c("Cluster 1", "Cluster 2"), col=c("black","red"), pch=c(19,19), bg="white")
pRamp <- colorRampPalette(c("red","yellow","blue"))
x$Col <- pRamp(20)[as.numeric(cut(x$ProbsC1, breaks=20))]
plot(x$X1, x$X2, pch=19, col=x$Col, main="Cluster probabilities", xlab="Cluster 1", ylab="Cluster 2")

## Second spatial clustering
library(sp)
data(meuse)
coordinates(meuse) <- ~x+y
cdat <- data.frame(x=coordinates(meuse)[,1],y=coordinates(meuse)[,2])
rownames(cdat) <- rownames(meuse@data)

chc <- hclust(dist(cdat))
## KNN
chc.n <- cutree(chc, k=6)

chc.d200 <- cutree(chc, h=200)

meuse@data <- data.frame(meuse@data, KNN=as.factor(chc.n), DClust=chc.d200)

par(mfcol=c(1,2))      
plot(meuse, col=factor(meuse@data$KNN), pch=19)
box(col="black")
title(main="KNN Clustering")
legend("topleft", legend=paste("Cluster", 1:6,sep=""), col=palette()[1:6], pch=rep(19,6), bg="white")
pRamp <- colorRampPalette(c("blue","yellow","red"))
Col <- pRamp(20)[as.numeric(cut(meuse@data$DClust, breaks=20))]
plot(meuse, col=Col, pch=19)
box(col="black")
title(main="Distance Clustering k=70")


