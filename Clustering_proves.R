base_path <- "C:/Users/danie/Documents/MD/diab/DataMiningOverDiabetics"
setwd(file.path(base_path))
diabetic_data <- read.csv("processed_data.csv", na.strings = c("?"))

names(diabetic_data)
dim(diabetic_data)
summary(diabetic_data)


attach(diabetic_data)

#set a list of numerical variables
names(diabetic_data)


#hierarchical clustering

#euclidean distance si totes son numeriques
dcon<-data.frame (X,adm_type_id,disch_id,adm_source_id,time_in_hpt,n_lab_proc,n_proc,n_med,n_outp,n_emerg,n_inp,n_diag)

d  <- dist(dcon[1:10,])

#move to Gower mixed distance to deal 
#simoultaneously with numerical and qualitative data

library(cluster)

#dissimilarity matrix
#do not include in actives the identifier variables nor the potential response variable

actives<-c(2:16)
dissimMatrix <- daisy(diabetic_data[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST

#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot

plot(h1)

k<-4

c2 <- cutree(h1,k)
c3 <- cutree(h1,2)

#class sizes 
table(c2)


table(c3, c2)

c1<-c2
par(xpd=TRUE)
plot(n_lab_proc,n_diag,col=c1,main="Clustering of credit data in 3 classes")
legend(90,6,c("class1","class2","class3","class4"),pch=1,col=c(1:k), cex = 0.5)

plot(n_diag,time_in_hpt)
plot(n_diag,time_in_hpt,col=c1,main="Clustering of credit data in 3 classes")
legend(0.8, 15,c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)


pairs(dcon[,1:7], col=c1)



cdg <- aggregate(as.data.frame(dcon),list(c1),mean)
cdg

plot(cdg[,1], cdg[,7])


par(ask=FALSE)
