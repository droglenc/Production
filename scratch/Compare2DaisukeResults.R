# ======================================================================
## Compare to Daisuke's results
DO_res <- read.csv("results/PB_04Aug2017_2137.csv",stringsAsFactors=FALSE)
xtabs(~use,data=DO_res)
DO_res %<>% filterD(use=="yes")
library(readxl)
DK_res <- read_xlsx("scratch/zzzOld_FromDaisuke/Daisuke Production Estimates.xlsx") %>%
  rename(wbic=WBIC,year=Year,wbic_year=WBIC_Year,P="P (kg/ha/y)",B="B (kg/ha)")
DO_DK <- inner_join(DO_res[,c("wbic_year","n","P","B")],
                    DK_res[,c("wbic_year","P","B")],by="wbic_year")

plot(B.x~B.y,data=DO_DK,pch=19,col=col2rgbt("black",1/5),
     xlab="Daisuke's B",ylab="Derek's B")
abline(a=0,b=1,lty=2,col="red")
plot(I(B.x-B.y)~B.y,data=DO_DK,pch=19,col=col2rgbt("black",1/5),
     xlab="Daisuke's B",ylab="Derek's B - Daisuke's B",ylim=c(-10,10))
abline(h=0,lty=2,col="red")


plot(P.x~P.y,data=DO_DK,pch=19,col=col2rgbt("black",1/5),
     xlab="Daisuke's P",ylab="Derek's P")
abline(a=0,b=1,lty=2,col="red")
plot(I(P.x/B.x)~I(P.y/B.y),data=DO_DK,pch=19,col=col2rgbt("black",1/5),
     xlab="Daisuke's P/B",ylab="Derek's P/B")
abline(a=0,b=1,lty=2,col="red")

