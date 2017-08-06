## Clear workspace and console
rm(list=ls())
cat("\014")
## Load required packages
library(FSA)
library(dplyr)
library(magrittr)



# ======================================================================
# Load prepped ALK data
alk_res <- read.csv("data/prepped/LWRegs.csv")
                                 



# ======================================================================
# Exploring the regression results
## Number of regressions excluded by reason
addmargins(xtabs(~type+reason,data=filterD(lw_res,use=="NO")))
## Distributions
boxplot(n~reason,data=filterD(lw_res,use=="NO"),ylab="n")
boxplot(rsq~reason,data=lw_res,ylab="r^2")
boxplot(b~reason,data=lw_res,ylab="b")

clrs <- col2rgbt(c("red","black"),1/4)
plot(loga~b,data=lw_res,pch=19,col=clrs[as.numeric(use)])
abline(v=b.cut,lty=2,col="red")
legend("topright",c("NO","yes"),pch=19,col=clrs,bty="n")
plot(rsq~n,data=lw_res,pch=19,col=clrs[as.numeric(use)],log="x")
abline(v=n.cut,lty=2,col="red"); abline(h=rsq.cut,lty=2,col="red")
legend("bottomright",c("NO","yes"),pch=19,col=clrs,bty="n")
with(lw_res,plot(n,maxLen-minLen,log="x",pch=19,col=clrs[as.numeric(use)]))
legend("bottomright",c("NO","yes"),pch=19,col=clrs,bty="n")
