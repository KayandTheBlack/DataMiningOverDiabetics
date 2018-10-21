
createCPG<- function(data, response)
{
  if (!is.factor(response)) 
  {
    cat("The variable ", names(response), " must be a factor" )	
  }
  else
  {
    #alerta! el maxim es 7 per fila
    #sembla que 3 per columna ho fa amb numeriques. Mes ja no se. Qualis donen problemes
    plotConditionalTable(data, response)
  }#end else
}#endcreateCPG



plotConditionalTable<-function(data, res)
{
  if(ncol(data)==0)
  {
    cat("Number of columns of dataset is 0")	
    return()
  }#endif
  if(nrow(data)==0) 
  {
    cat("Number of rows of dataset is 0")  
    return()
  }#endif
  #proceed only if data frame is non empty
  
  #transform response variable into a suitable string for printing purposes
  response<-factor(res)
  
  #create an auxiliary matrix with as much rows as classes to keep the position of figures in the CPG
  nc<-length(levels(response))
  K<-dim(data)[2]
  ncells<-nc*K
  
  mat<- matrix(data=c(1:ncells),nrow= nc, ncol=K, byrow=FALSE)
  
  #ojo, que si esta buit el panell peta
  dev.off()
  layout(mat, widths= rep.int(1, K), heights= rep.int(1,nc))
  
  for (k in 1:K){
    Vnum<-data[,k]
    for(niv in levels(response)){
      print(niv)
      s<-subset(Vnum, response==niv)
      if(is.numeric(data[,k]))
      {  hist(s, main=paste(names(data)[k], niv))
        #evenctually add other summary statistics, like vc
      }else{
        barplot(table(s), las=3, cex.names=0.5, main=paste("Barplot of", names(data)[k]))
      }#endifelse
    }#end for niv       
  }#end for k
}#end plot conditional table      
