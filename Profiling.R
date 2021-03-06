#  READING CREDSCO_BIN
# load("d:/karina/docencia/DataMiningEI/Practiques/2CredscoProfiling/credscok_bin")

setwd("E:/Marc/Cole/Uni/7eQ/MD/DataMiningOverDiabetics");
dd <- read.csv("processed_data.csv", na.strings = c("?"))
names(dd)

attach(dd)

#Dictamen    <- as.factor(Dictamen)
#levels(Dictamen) <- c(NA, "positiu","negatiu")

actives<-c(4:11,13:30)

#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}




ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2]);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  zkj <- dpf/dvt; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}


dades<-dd[,actives]
#dades<-df
K<-dim(dades)[2]
par(ask=TRUE)

P<-c1
nc<-length(levels(as.factor(P)))
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"Class"
n<-dim(dades)[1]


setwd(file.path("./Profiling"))
for(k in 1:K){
  dir.create(file.path(names(dades)[k]))
  setwd(file.path(names(dades)[k]))
  
  if (is.numeric(dades[,k])){ 
    print(paste("An�lisi per classes de la Variable:", names(dades)[k]))
    
    png(filename=paste(names(dades)[k],"_Boxplot",".png",sep=""), width=800, height=800)
    boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE, cex=1.2)
    dev.off()
    
    
    png(filename=paste(names(dades)[k],"_Barplot",".png",sep=""), width=800, height=800)
    barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
    
    abline(h=mean(dades[[k]]))
    legend(0,mean(dades[[k]]),"global mean",bty="n")
    dev.off()
    
    print("Estad�stics per groups:")
    for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
    o<-oneway.test(dades[,k]~P)
    print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(dades[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[,k]<-ValorTestXnum(dades[,k], P)
    print("p-values ValorsTest: ")
    print(pvalk[,k])      
  }else{
    #qualitatives
    print(paste("Variable", names(dades)[k]))
    table<-table(P,dades[,k])
    #   print("Cross-table")
    #   print(table)
    rowperc<-prop.table(table,1)
    
    colperc<-prop.table(table,2)
    #  print("Distribucions condicionades a files")
    # print(rowperc)
    
    marg <- table(as.factor(P))/n
    print(append("Categories=",levels(dades[,k])))
    plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(dades[,k])))
    for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
    
    #with legend
    png(filename=paste(names(dades)[k],"_pos&neg",".png",sep=""), width=800, height=800)
    plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(dades[,k])))
    for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
    legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=1.2)
    dev.off()
    
    #condicionades a classes
    print(append("Categories=",levels(dades[,k])))
    plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(dades[,k])))
    for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
    
    #with legend
    png(filename=paste(names(dades)[k],"_pos&negCondClasse",".png",sep=""), width=800, height=800)
    plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(dades[,k])))
    for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
    legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=1.2)
    dev.off()
    
    #amb variable en eix d'abcisses
    marg <-table(dades[,k])/n
    print(append("Categories=",levels(dades[,k])))
    plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(as.factor(P))))
    for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }
    
    #with legend
    png(filename=paste(names(dades)[k],"_pos&negGirat",".png",sep=""), width=800, height=800)
    plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
    legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=1.2)
    dev.off()
    
    #condicionades a columna 
    plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    paleta<-rainbow(length(levels(as.factor(P))))
    for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
    
    #with legend
    png(filename=paste(names(dades)[k],"_pos&negCondColumn",".png",sep=""), width=800, height=800)
    plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
    for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
    legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=1.2)
    dev.off()
    
    table<-table(dades[,k],P)
    print("Cross Table:")
    print(table)
    print("Distribucions condicionades a columnes:")
    print(colperc)
    
    #diagrames de barres apilades                                         
    
    paleta<-rainbow(length(levels(dades[,k])))
    barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
    
    png(filename=paste(names(dades)[k],"_barplotApilades",".png",sep=""), width=800, height=800)
    barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
    legend("topright",levels(as.factor(dades[,k])),pch=1,cex=1.2, col=paleta)
    dev.off()
    
    #diagrames de barres adosades
    barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta )
    
    png(filename=paste(names(dades)[k],"_barplotAdosades",".png",sep=""), width=800, height=800)
    barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta)
    legend("topright",levels(as.factor(dades[,k])),pch=1,cex=1.2, col=paleta)
    dev.off()
    
    print("Test Chi quadrat: ")
    print(chisq.test(dades[,k], as.factor(P)))
    
    print("valorsTest:")
    print( ValorTestXquali(P,dades[,k]))
  }
  setwd(file.path(".."))
}#endfor
setwd(file.path(".."))
################ Arreglar pvalues


for (c in 1:length(levels(as.factor(P)))) { if(!is.na(levels(as.factor(P))[c])){print(paste("P.values per class:",levels(as.factor(P))[c])); print(sort(pvalk[c,]), digits=3) }}

#afegir la informacio de les modalitats de les qualitatives a la llista de pvalues i fer ordenacio global

#saving the dataframe in an external file
#write.table(dd, file = "credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)


#createCPG(dd[,active], Tipo.trabajo)

#Fer gran la finestra del R
#createCPG(dd[,active], Dictamen)

#comparar una variable amb les altres

detach(dd)
detach(dades)
detach(diabetic_data)

dades<-dd[,actives]
attach(dades)
plotConditionalTable(dades[,1:2], readmitted)


#cada numero es la columna


#fer creixer la finestra de plots
#control - per fer menor el tipus de lletra en R
#cambiar els valors d'active per mostrar les diferents variables
active<-c(24:25)
createCPG(dades[,active], as.factor(c1))

#falta jugar amb els marges