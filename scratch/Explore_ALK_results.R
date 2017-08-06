## Clear workspace and console
rm(list=ls())
cat("\014")
## Load required packages
library(FSA)
library(dplyr)
library(magrittr)



# ======================================================================
# Load prepped ALK data
alk_res <- read.csv("data/prepped/ALKInfo.csv")
                                 



# ======================================================================
# Exploring the ALK results
## Number of ALKs excluded by reason
addmargins(xtabs(~type+reason,data=filterD(alk_res,use=="NO")))
xtabs(~use,data=filterD(alk_res,sname=="NA"))
## Distributions
boxplot(n~reason,data=filterD(alk_res,use=="NO"),ylab="n")

clrs <- col2rgbt(c("red","black"),1/6)
plot(numLens~numAges,data=alk_res,pch=19,col=clrs[as.numeric(use)])
abline(v=ages.cut,lty=2,col="red"); abline(h=lens.cut,lty=2,col="red")
legend("topleft",c("NO","yes"),pch=19,col=clrs,bty="n")
plot(numAges~n,data=alk_res,pch=19,col=clrs[as.numeric(use)],log="x")
abline(v=n.cut,lty=2,col="red"); abline(h=ages.cut,lty=2,col="red")
legend("topleft",c("NO","yes"),pch=19,col=clrs,bty="n")
plot(numLens~n,data=alk_res,pch=19,col=clrs[as.numeric(use)],log="x")
abline(v=n.cut,lty=2,col="red"); abline(h=lens.cut,lty=2,col="red")
legend("topleft",c("NO","yes"),pch=19,col=clrs,bty="n")
