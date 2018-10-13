base_path <- "~/Documents/DataMiningOverDiabetics"
setwd(file.path(base_path))
diabetic_data <- read.csv("diabetic_data.csv", na.strings = c("?"))

class(diabetic_data)
dim(diabetic_data)
n<-dim(diabetic_data)[1]
n
K<-dim(diabetic_data)[2]
K

names(diabetic_data)

listOfColors<-rainbow(14)

par(ask=TRUE)

dir.create(file.path("PreDescriptive"))
setwd(file.path("PreDescriptive"))
for(k in 1:K){
  if (is.factor(diabetic_data[,k])){ 
    frecs<-table(diabetic_data[,k], useNA="ifany")
    proportions<-frecs/n
    
    # Create new directory
    dir.create(file.path(names(diabetic_data)[k]))
    setwd(file.path(names(diabetic_data)[k]))
    
    #Save pie plot image
    png(filename=paste(names(diabetic_data)[k],"_Pie",".png",sep=""), width=800, height=800)
    pie(frecs, cex=0.6, main=paste("Pie of", names(diabetic_data)[k]))
    dev.off()
    
    #Save bar plot image
    png(filename=paste(names(diabetic_data)[k],"_Barplot",".png",sep=""), width=800, height=800)
    barplot(frecs, las=3, cex.names=0.7, main=paste("Barplot of", names(diabetic_data)[k]), col=listOfColors)
    dev.off()
    
    print(frecs)
    print(proportions)
    
    setwd(file.path(".."))
  }else{
    # Create new directory
    dir.create(file.path(names(diabetic_data)[k]))
    setwd(file.path(names(diabetic_data)[k]))
    
    # Save histogram image
    png(filename=paste(names(diabetic_data)[k],"_Histogram",".png",sep=""), width=800, height=800)
    hist(diabetic_data[,k], main=paste("Histogram of", names(diabetic_data)[k]))
    dev.off()
    
    # Save boxplot image
    png(filename=paste(names(diabetic_data)[k],"_Boxplot",".png",sep=""), width=800, height=800)
    boxplot(diabetic_data[,k], horizontal=TRUE, main=paste("Boxplot of", names(diabetic_data)[k]))
    dev.off()
    
    print(summary(diabetic_data[,k]))
    print(paste("sd: ", sd(diabetic_data[,k])))
    print(paste("vc: ", sd(diabetic_data[,k])/mean(diabetic_data[,k])))
    
    setwd(file.path(".."))
  }
}

par(ask=FALSE)
