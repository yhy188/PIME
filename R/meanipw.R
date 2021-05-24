


meanipw <- function(data) {
mean.rep<-data.frame()
for (ic in seq(1:6)) {
  df.meanrep<-data[data$cell==ic,]
  mean.temp<-sum(df.meanrep$y*df.meanrep$sw)/sum(df.meanrep$sw)
  mean.rep<-rbind(mean.rep,mean.temp)  
}
#mean.rep
names(mean.rep)<-"mean"
mean.rep.mat<-matrix(mean.rep$mean,nrow=2)
mean.rep.mat<-t(mean.rep.mat)
mean.rep.mat
#return(mean.rep)
det<-(sum(mean.rep[c(2,4,6),])-sum(mean.rep[c(1,3,5),]))/3
names(det)<-"mean"
var<-var(mean.rep)
names(var)<-"var"
result<-rbind(mean.rep,det,var)                   
return(result)
}
