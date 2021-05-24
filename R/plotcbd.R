
plotcbd <- function(bfdfgroup) {
library(ggplot2)
head(bfdfgroup)
ggdf<-bfdfgroup
df<-data.frame()
for (i in seq(1:6)) {
  temp<-ggdf[c(i,7)]
  temp$cell<-i
  names(temp)<-c("y","group","cell")
  df<-rbind(df,temp)
}
nrow(ggdf)
df<-data.frame(df)
nrow(df)
head(df)
table(df$group,df$cell)
#ggplot(aes(y = y, x = factor(cell)),  fill = factor(group), data = df) + geom_boxplot()
#a<-0.1
#yupper<-max(df$y)*(1+a)
#ylower<-min(df$y)*(1-a)
yupper<-12.5
ylower<-3.5
{
  bp<-ggplot(df, aes(y, x=factor(cell), fill=factor(group))) +
    geom_boxplot()+
    xlab("Cell")+
    ylab("Mean responses(mmol/L)")+
    ylim(ylower,yupper)
  bp + scale_fill_discrete(name="Methods",
                           breaks=seq(1:3),
                           labels=c("BL", "IPW", "AIPW"))

  }
}
