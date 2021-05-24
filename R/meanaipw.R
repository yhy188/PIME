

meanaipw <- function(data,mean.rep.bl) {
  pat<-3
df <- data.frame()
for (k in seq(1:pat)) {
  n1 = nrow(data[data$block == k,])
  data <-
    data[complete.cases(data),]# max likelihood with complete case
  mu1 <-sum(data[data$block == k, "a"] * data[data$block == k, "y"] * data[data$block ==
                                                                     k, "sw"]
            - (data[data$block == k, "a"]) * mean.rep.bl[k, 2] * data[data$block ==k, "sw"]
            #+(data[data$block == k, "a"])  *mean.rep.bl[k, 2])/n1
            +mean.rep.bl[k, 2])/n1
  ## all i of n1; not  n0 * mean
  mu0 <-
    mean((1 - data[data$block == k, "a"]) * data[data$block == k, "y"] * data[data$block ==
                                                                        k, "sw"]
         - (1 - data[data$block == k, "a"]) * mean.rep.bl[k, 1]* data[data$block ==
                                                                    k, "sw"]
         + mean.rep.bl[k, 1]) # * y.tx.t3.aipwt3.mice$sw
  
  det <- mu1 - mu0
  df <- rbind(df, data.frame(k, mu1, mu0, det))
}

#det <- aipw_exp(datay, r)
aipw.mean<-df
mean.rep.mat<-aipw.mean[,c(3,2)]
mean.rep.mat
mean.rep.vec<-c(mean.rep.mat[1,],mean.rep.mat[2,],mean.rep.mat[3,])
mean.rep.vec<-data.frame(mean.rep.vec)
mean.rep<-t(mean.rep.vec)
#return(mean.rep)

det<-(sum(mean.rep[c(2,4,6),])-sum(mean.rep[c(1,3,5),]))/3
names(det)<-"mean"
var<-var(mean.rep)
names(var)<-"var"
result<-rbind(mean.rep,det,var)                   
return(result)
}