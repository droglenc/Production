# ======================================================================
## Compare emperical and smoothed ALKs
EM_res <- read.csv("results/PB_04Aug2017_2327.csv",stringsAsFactors=FALSE) %>%
  filterD(use=="yes")
SM_res <- read.csv("results/PB_05Aug2017_2149.csv",stringsAsFactors=FALSE) %>%
  filterD(use=="yes")
PBres <- inner_join(EM_res[,c("wbic_year","P","B")],
                    SM_res[,c("wbic_year","P","B")],by="wbic_year")

plot(B.x~B.y,data=PBres,pch=19,col=col2rgbt("black",1/5),
     xlab="Smoothed B",ylab="Empirical B")
abline(a=0,b=1,lty=2,col="red")
plot(I(B.x-B.y)~B.y,data=PBres,pch=19,col=col2rgbt("black",1/5),
     xlab="Smoothed B",ylab="Empirical-Smoothed B")
abline(h=0,lty=2,col="red")

plot(P.x~P.y,data=PBres,pch=19,col=col2rgbt("black",1/5),
     xlab="Smoothed P",ylab="Empirical P")
abline(a=0,b=1,lty=2,col="red")
plot(I(P.x/B.x)~I(P.y/B.y),data=PBres,pch=19,col=col2rgbt("black",1/5),
     xlab="Smoothed P/B",ylab="Empirical P/B")
abline(a=0,b=1,lty=2,col="red")

