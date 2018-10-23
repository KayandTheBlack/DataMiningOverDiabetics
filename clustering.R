base_path <- "C:/Users/danie/Documents/MD/diab/DataMiningOverDiabetics"
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

#insert again the response variable


dcon <- data.frame (race, gender, age, weight, adm_type_id, disch_id, adm_source_id, time_in_hpt, specialty, n_lab_proc, n_proc, n_med, n_outp, n_emerg, n_inp, diag_1, diag_2, diag_3, A1Cresult, metformin, insulin, change, diabetesMed, readmitted, other_meds)
png("all_vars_pairs.png", width=20, height=20, units="in", res=500)
pairs(dcon[,1:25], col=c1)
dev.off()

dcon <- data.frame (age, n_lab_proc, n_med, time_in_hpt, n_outp, disch_id)
png("some_vars_pairs2.png", width=20, height=20, units="in", res=500)
pairs(dcon[,1:6], col=c1)
dev.off()


plot(n_med, n_lab_proc,col=c1,main="Clustering of credit data in 3 classes")
plot(n_med, age,col=c1,main="Clustering of credit data in 3 classes")
plot(n_med, time_in_hpt,col=c1,main="Clustering of credit data in 3 classes")
plot(weight, adm_type_id,col=c1,main="Clustering of credit data in 3 classes")
plot(n_med, disch_id,col=c1,main="Clustering of credit data in 3 classes")

#trying to display the discrete variables as continuous to avoid problems
for (row in 1:nrow(diabetic_data)) {
  diabetic_data[row, "disch_id"] <- diabetic_data[row, "disch_id"] + runif(1, -1.0, 1.0)
  #disch_id <- disch_id + runif(1, -1.0, 1.0)
}



