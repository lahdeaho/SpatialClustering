# OptK
## Finds optimal supported number of clusters (k) based on silhouettes
## Original source from http://evansmurphy.wix.com/evansspatial#!clustering-using-r/c1c97 by Jeffrey

OptK <- function(x, nk=10, plot=TRUE, cluster=TRUE, clara=FALSE, ...) {   
     if (!require(cluster)) stop("cluster PACKAGE MISSING")
     asw <- numeric(nk)
     for (k in 2:nk) {   
          if(clara==TRUE) { asw[k] <- clara(x, k, ...)$silinfo$avg.width }
          if(clara==FALSE) { asw[k] <- pam(x, k, ...)$silinfo$avg.width }
          k.best <- which.max(asw)
     }
     
     print(paste("OPTIMAL-K", k.best, sep=": "))
     if(plot==TRUE) {      
          plot(1:nk, asw, type="s", main="Clustering Optimization using K-Mediods",
               xlab="K (number of clusters)", ylab = "mean silhouette width")
          axis(1, k.best, paste("best",k.best,sep="\n"), col="red",
               col.axis="red")
     }
     
     if(cluster==TRUE) {
          if(clara==TRUE) { return(clara(x, k.best, ...)) }
          if(clara==FALSE) { return(pam(x, k.best, ...)) }  
     }   
}
