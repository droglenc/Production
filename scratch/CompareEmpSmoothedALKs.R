# ======================================================================
## Compare emperical and smoothed ALKs
SM_res <- read.csv("results/PB_07Aug2017_0827_smoothed.csv",stringsAsFactors=FALSE) %>%
  filterD(use=="yes")
EM_res <- read.csv("results/PB_07Aug2017_0831_empirical.csv",stringsAsFactors=FALSE) %>%
  filterD(use=="yes")
PBres <- inner_join(EM_res[,c("wbic_year","P","B")],
                    SM_res[,c("wbic_year","P","B")],
                    by="wbic_year",suffix=c(".E",".S"))

plot(B.E~B.S,data=PBres,pch=19,col=col2rgbt("black",1/5),
     xlab="Smoothed B",ylab="Empirical B")
abline(a=0,b=1,lty=2,col="red")
plot(I(B.E-B.S)~B.S,data=PBres,pch=19,col=col2rgbt("black",1/5),
     xlab="Smoothed B",ylab="Empirical-Smoothed B")
abline(h=0,lty=2,col="red")

plot(P.E~P.S,data=PBres,pch=19,col=col2rgbt("black",1/5),
     xlab="Smoothed P",ylab="Empirical P")
abline(a=0,b=1,lty=2,col="red")
plot(I(P.E/B.E)~I(P.S/B.S),data=PBres,pch=19,col=col2rgbt("black",1/5),
     xlab="Smoothed P/B",ylab="Empirical P/B")
abline(a=0,b=1,lty=2,col="red")

