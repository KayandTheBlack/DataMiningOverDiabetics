base_path <- "~/Documents/DataMiningOverDiabetics"
setwd(file.path(base_path))
diabetic_data <- read.csv("processed_data.csv", na.strings = c("?"))

names(diabetic_data)
dim(diabetic_data)
summary(diabetic_data)

attach(diabetic_data)

#hierarchical clustering

library(cluster)
#install.packages("fpc")
library(fpc)

#dissimilarity matrix
actives<-c(4:11,13:28,30) #exclude row identifiers, non significant variables and response variable
dissimMatrix <- daisy(diabetic_data[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D")

K<-10 #we want to see 10 partitions
CHIndexes <- array(dim=10)
Silhouettes <- array(dim=10)
for (k in 2:K) {
  ck <- cutree(h1,k)
  stats <- cluster.stats(distMatrix, ck)
  CHIndexes[k] <- stats$ch
  Silhouettes[k] <- stats$avg.silwidth
}
plot(CHIndexes, type="o", xlab="Number of clusters", ylab="CH index")
plot(Silhouettes, type="o", xlab="Number of clusters", ylab="Average silhouette")

#The number of clusters is the max of CH indexes and Silhouette (excluding the 2 clusters partition)
n_clusters = 4

c1 <- cutree(h1,n_clusters)

plot(h1, labels = FALSE)
rect.hclust(h1, k = n_clusters)

