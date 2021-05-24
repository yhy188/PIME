




pmm <- function(data) {
  dfpm<-data.frame()
 # for (it in c(0, 1))
    {
#    dft <- data[data$a == it,]
      dft <- data#[data$a == it,]
    df <- dft
    df$y3b2p <- df$y3b
    df$y2b3p <- df$y2b
    df$y3b3p <- df$y3b2p
    #    df$pbcell<-(df$pattern-1)*3+df$
    head(df)
    for (ip in c(2, 3)) {
      if (ip == 2) {#& (b2==1|b2==0)
        #ip==2& (b2==1|b2==0)
        ina <- df[df$pattern == ip & is.na(df$y3b), "id"]
        ina <- data.frame(ina)
        nr <- dim(ina)
        for (ind in seq(1:nr[1])) {
          irow <- ina[ind,]
          recipient <- df[df$id == irow ,]
          donor <- df[complete.cases(df$y3b),]
          #        donor.den<-density(donor$y3b2p)
          #          p.table<-prop.table(table(df$y1b,df$y3b3p))

          ####marignal of y3b
          muri<-mean(as.numeric(donor$y3b))
          sigmari<-var(donor$y3b)
          ########Yiobs
          lm.yiobs<-lm(y2b~y1b,data=donor)
          yiobs<-predict(lm.yiobs, data=recipient[,"y1b"])#,interval = "confidence")
          yiobsthe<-c(mean(yiobs),var(yiobs))
          #######yit+1
          lm.obs<-lm(y3b~y1b+y2b,data=donor)
          #yt1<-predict(lm.obs, newdata = recipient[,c("y1b","y2b")])
          yt1<-predict(lm.obs, data = recipient[,c("y1b","y2b")])#,interval = "confidence")
          yt1the<-c(mean(yt1),var(yt1))
          #ycovariance
          ycov<-cov(yiobs, yt1)
          ycov
          ######conditional paramethers
          mucond<-yt1the[1]+ycov*(1/(yiobsthe[2]))*(recipient[,"y2b"]-yiobsthe[1])
          sigmacond<-yt1the[2]-ycov*(1/(yt1the[1]))*ycov
          #########
          df[df$id == irow , "y3b2p"] <- mucond
          df$y3b3p <- df$y3b2p
          dft<-df
        }

      } else{
        for (b2 in c(1, 0)) {
          if (b2 == 1& (ip==3)) {
            #ip==3&b2==1
            #  dfb <- df
            dfb2 <- df[df$b2 == 1, ]
            #          ina <- df[df$pattern == ip & (df$b2 == 1), "id"]
            ina <- dfb2[, "id"]
            ina <- data.frame(ina)
            nr <- dim(ina)
            for (ind in seq(1:nr[1])) {
              irow <- ina[ind,]
              recipient <- df[df$id == irow ,]
              donor <- df[complete.cases(df$y2b),]
              ########Yiobs
          #    lm.yiobs<-lm(y2b~y1b,data=donor)
           #   yiobs<-predict(lm.yiobs, data=recipient[,"y1b"])#,interval = "confidence")
              yiobs<-donor$y1b
              yiobs<-as.numeric(yiobs)
               yiobsthe<-c(mean(yiobs),var(yiobs))
              #######yit+1
              lm.obs<-lm(y2b~y1b,data=donor)
              #yt1<-predict(lm.obs, newdata = recipient[,c("y1b","y2b")])
              yt1<-predict(lm.obs, data = recipient[,c("y1b")])#,interval = "confidence")
              yt1the<-c(mean(yt1),var(yt1))
              #ycovariance
              ycov<-cov(yiobs, yt1)
              ycov
              ######conditional paramethers
              mucond<-yt1the[1]+ycov*(1/(yiobsthe[2]))*(recipient[,"y1b"]-yiobsthe[1])
              sigmacond<-yt1the[2]-ycov*(1/(yt1the[1]))*ycov
              #########
              df[df$id == irow , "y2b3p"] <- mucond
#              df[df$id == irow , "y2b3p"] <- ytheta[1]
              #       df$y2b3p<-df$y3b2p
            }
            #  }
            #else if (b2 == 1& (ip==3))

            #   {
            #ip==2&b2==0
            ina <- df[df$pattern == ip , "id"]#& df$b3 == 1
            ina <- data.frame(ina)
            nr <- dim(ina)
            for (ind in seq(1:nr[1])) {
              irow <- ina[ind,]
              recipient <- df[df$id == irow ,]
              donor <- df[complete.cases(df$y3b3p),]
              ########Yiobs
              lm.yiobs<-lm(y2b~y1b,data=donor)
              yiobs<-predict(lm.yiobs, data=recipient[,"y1b"])#,interval = "confidence")
              yiobsthe<-c(mean(yiobs),var(yiobs))
              #######yit+1
              lm.obs<-lm(y3b~y1b+y2b3p,data=donor)
              #yt1<-predict(lm.obs, newdata = recipient[,c("y1b","y2b")])
              yt1<-predict(lm.obs, data = recipient[,c("y1b","y2b3p")])#,interval = "confidence")
              yt1the<-c(mean(yt1),var(yt1))
              yt1df<-as.numeric(yt1)
              yiobsdf<-as.numeric(yiobs)
              #ycovariance
              size<-min(length(yt1),length(yiobs))
              yt1<-sample(yt1df, size, replace=FALSE)
              yiobs<-sample(yiobsdf, size, replace=FALSE)
              ycov<-cov(yiobs, yt1)
              ycov
              ######conditional paramethers
              mucond<-yt1the[1]+ycov*(1/(yiobsthe[2]))*(recipient[,"y2b3p"]-yiobsthe[1])
              sigmacond<-yt1the[2]-ycov*(1/(yt1the[1]))*ycov
              #########
              df[df$id == irow , "y3b3p"] <- mucond
              #df$y3b3p<-df$y3b2p
            }
          }
        }
      }
    }
    dfpm<-rbind(dfpm,df)
  }
  df<-dfpm
  df.out<-df[,c(1:6,14,15,9)]
#  head(df.outb1)
  df.outb1<-df.out[,c(1:6,9)]
  df.outb1$block<-1
  names(df.outb1)<-c("id","x1","x2","x3","a","y","pattern","block")
  df.outb2<-df.out[c(1:5,7,9)]
  df.outb2$block<-2
  names(df.outb2)<-c("id","x1","x2","x3","a","y","pattern","block")
  df.outb3<-df.out[c(1:5,8,9)]
  df.outb3$block<-3
  names(df.outb3)<-c("id","x1","x2","x3","a","y","pattern","block")
  result<-rbind(df.outb1,df.outb2,df.outb3)
  return(result)
}
