


## Complete with sequencees {1,1,1,1,...}

meanrep <- function(data) {
  mean.rep<-data.frame()
  data<-data.frame(data)
  for (ic in seq(1:6)) {
    df.meanrep<-data[data$cell==ic,]
    mean.temp<-mean(df.meanrep$y*1)
    mean.rep<-rbind(mean.rep,mean.temp)  
  }
  #mean.rep
  names(mean.rep)<-"mean"
  mean.rep.mat<-matrix(mean.rep$mean,nrow=2)
  mean.rep.mat<-t(mean.rep.mat)
  mean.rep.bl<-mean.rep.mat
            
  return(mean.rep.bl)
}