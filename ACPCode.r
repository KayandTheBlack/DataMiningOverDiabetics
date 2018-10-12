#  READING CREDSCO_BIN
# load("d:/karina/docencia/sreferenciesppt/16.AssociatiusVisualitzacio/MultivariateAnalysis/PracticaR/credscok_bin")

# setwd("D:/karina/docencia/areferenciesPPT/DadesPractiques/CREDSCO")
# dd <- read.table("credscoClean.csv",header=T, sep=";");
setwd("C:/Users/guill/Documents/GitHub/DataMiningOverDiabetics")
dd <- read.csv("processed_data.csv")

objects()
attributes(dd)

#
# VISUALISATION OF DATA
#
# PRINCIPAL COMPONENT ANALYSIS OF CONTINcUOUS VARIABLES, WITH Dictamen PROJECTED AS ILLUSTRATIVE
#

# CREATION OF THE DATA FRAME OF CONTINUOUS VARIABLES

attach(dd)
names(dd)


#set a list of numerical variables


dcon <- data.frame (n_diag,n_inp,n_outp,n_emerg,n_med,n_proc,n_lab_proc,time_in_hpt)



# PRINCIPAL COMPONENT ANALYSIS OF dcon

pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)

print(pc1)




# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
png(filename="ACP/InertiaBarplot.png", width=1920, height=1080,units="px")
barplot(pinerEix)
dev.off()

#Cummulated Inertia in subspaces, from first principal component to the 8th dimension subspace
png(filename="ACP/Inertia2x2.png", width=1920, height=1080,units="px")
par(mfrow=c(2,2))
plot(pinerEix,xlab="Principal component", ylab="% of inertia", ylim=c(0,100), type='b')
plot(cumsum(inerProj/sum(inerProj)*100),xlab="Principal component", ylab="Cumulative % of inertia", ylim=c(0,100), type='b')
screeplot(pc1)
screeplot(pc1,type="l")
dev.off()
png(filename="ACP/Inertia1x1.png", width=1920, height=1080,units="px")
par(mfrow=c(1,1))

tmp <- 100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
names(tmp) <- c(1,2,3,4,5,6,7,8)
barplot(tmp)
plot(c(1:8),tmp,type='o',xlab="Principal component", ylab="Cumulative % of inertia")
dev.off()
percInerAccum<-tmp
percInerAccum


# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

nd = 6

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS


Psi = pc1$x[,1:nd]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS





















# PLOT OF INDIVIDUALS

#select your axis
eje1<-1
eje2<-2
eje3<-3
png(filename="ACP/Individual1.png", width=1920, height=1080,units="px")
plot(Psi[,eje1],Psi[,eje2])
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
dev.off()
png(filename="ACP/Individual2.png", width=1920, height=1080,units="px")
plot(Psi[,eje1],Psi[,eje3])
text(Psi[,eje1],Psi[,eje3],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
dev.off()
png(filename="ACP/Individual3.png", width=1920, height=1080,units="px")
plot(Psi[,eje2],Psi[,eje3])
text(Psi[,eje2],Psi[,eje3],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
dev.off()

#Projection of variables

Phi = cor(dcon,Psi)


plot.with.zooms <- function(minx,maxx,miny,maxy, axis1, axis2){
  X<-Phi[,axis1]
  Y<-Phi[,axis2]
  #all qualitative together with zooms
  plot(Psi[,axis1],Psi[,axis2],type="n",xlim=c(minx,maxx), ylim=c(miny,maxy))
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  
  arrows(ze, ze, X, Y, length = 0.07,col="blue")
  text(X,Y,labels=etiq,col="darkblue", cex=0.7)
  
  #nominal qualitative variables
  dcat<-c(4,5,12,13,20,21,22,24,25,26,27,28,29,30)
  #divide categoricals in several graphs if joint representation saturates
  #build a palette with as much colors as qualitative variables 
  colors<-rainbow(length(dcat))
  c<-1
  for(k in dcat){
    seguentColor<-colors[c]
    fdic1 = tapply(Psi[,axis1],dd[,k],mean)
    fdic2 = tapply(Psi[,axis2],dd[,k],mean) 
    
    text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
    c<-c+1
  }
  # ordinal
  dordi<-c(6,7)
  dd[,dordi[2]] <- factor(dd[,dordi[2]], ordered=TRUE,  levels= c("[0-25)", "[25-50)", "[50-75)", "[75-100)", "[100-125)", "[125-150)", "[150-175)", "[175-200)",">200"))
  c<-1
  col <- 1
  for(k in dordi){
    seguentColor<-colors[col]
    fdic1 = tapply(Psi[,axis1],dd[,k],mean)
    fdic2 = tapply(Psi[,axis2],dd[,k],mean) 
    
    #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
    #connect modalities of qualitative variables
    lines(fdic1,fdic2,pch=16,col=seguentColor)
    text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
    c<-c+1
    col<-col+1
  }
  legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)
  legend("bottomright",names(dd)[dordi],pch=1,col=colors[1:length(dordi)], cex=0.6)
  
}


# Axis 1,2
png(filename="ACP/PlotWithZooms1-2(1).png", width=1920, height=1080,units="px")
plot.with.zooms(-4,4,-4,1,1,2)
dev.off()
png(filename="ACP/PlotWithZooms1-2(2).png", width=1920, height=1080,units="px")
plot.with.zooms(-1,1,-1,1,1,2)
dev.off()
png(filename="ACP/PlotWithZooms1-2(3).png", width=1920, height=1080,units="px")
plot.with.zooms(-2,2,-1,1,1,2)
dev.off()

# Axis 1,3
png(filename="ACP/PlotWithZooms1-3(1).png", width=1920, height=1080,units="px")
plot.with.zooms(-5,4,-5,2,1,3)
dev.off()
png(filename="ACP/PlotWithZooms1-3(2).png", width=1920, height=1080,units="px")
plot.with.zooms(-1,1,-1,1,1,3)
dev.off()

# Axis 2,3
png(filename="ACP/PlotWithZooms2-3(1).png", width=1920, height=1080,units="px")
plot.with.zooms(-4,3,-3,2,2,3)
dev.off()
png(filename="ACP/PlotWithZooms2-3(2).png", width=1920, height=1080,units="px")
plot.with.zooms(-2,2,-2,2,2,3)
dev.off()
png(filename="ACP/PlotWithZooms2-3(3).png", width=1920, height=1080,units="px")
plot.with.zooms(-1,1,-1,1,2,3)
dev.off()
