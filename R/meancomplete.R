


## Complete with sequencees {1,1,1,1,...}
 
meancomplete <- function(data) {
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
det<-(sum(mean.rep[c(2,4,6),])-sum(mean.rep[c(1,3,5),]))/3
names(det)<-"mean"
var<-var(mean.rep)
names(var)<-"var"
result<-rbind(mean.rep,det,var)                   
return(result)
}