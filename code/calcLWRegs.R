# ######################################################################
# ======================================================================
# This script computes weight-length regressions by WBIC_YEAR, WBIC,
# lake classification, and overall (no grouping) and writes a file with
# the regression coefficients and other regression information. This 
# script need not be run again unless the underlying weight-length data
# are modified. The file created will be used to predict weights from
# lengths for fish that were not weighed.
#
# THIS SCRIPT SHOULD NOT NEED TO BE RUN AGAIN AS IT WILL OVERWRITE
# THE PREPPED FILE IN 'data/prepped/'.

# However, as a safeguard, the file will only be overwritten if the
# writePreppedFiles object below is set to TRUE.
#
# ======================================================================
# ######################################################################



# ======================================================================
# Setup
## Clear workspace and console
rm(list=ls())
cat("\014")
## Load required packages
library(FSA)
library(dplyr)
library(magrittr)
library(nlme)
## Set whether old prepped files should be over-written (see above)
writePreppedFiles <- FALSE
## A new function to extract some information from the lists returned
## by lmList that contains each length-weight regression by group
LWINFO <- function(lms,type) {
  ##  Got summary of each regression ... this is a BIG list
  tmp <- lapply(lms,summary)
  ##  Got model.frame for each regression ... to get min and max len below
  tmp2 <- sapply(lapply(lms,model.frame),'[[','logl')
  ##  Extracted coefficients, r-squared, and sample size (df_reg+2)
  ##  Renamed coefficients
  res <- data.frame(coef(lms),rsq=sapply(tmp,'[[','r.squared'),
                    n=sapply(tmp,'[[','df')[2,]+2,
                    minLen=exp(sapply(tmp2,min)),
                    maxLen=exp(sapply(tmp2,max))) %>%
    rename(loga=X.Intercept.,b=logl)
  ##  Added 'which' variable that lists the group/level for the regression
  ##  Added 'type' variable for level of regression (e.g., at WBIC_YEAR)
  res %<>% mutate(which=rownames(res),type=type)
  res
}


# ======================================================================
# Load length-weight data
##  Added logged versions of length and weight
lw <- read.csv("data/prepped/len_wt.csv") %>%
  mutate(logl=log(len.mm),logw=log(wt))


# ======================================================================
# Calculate length-weight regressions
## For each WBIC_YEAR
##   na.action needed because of NAs in other variables in data.frame
lw_wy <- nlme::lmList(logw~logl|wbic_year,data=lw,na.action=na.pass)
wy_res <- LWINFO(lw_wy,"WBIC_YEAR")
## Try this to see an individual regression plot (can change number)
fitPlot(lw_wy[[names(lw_wy)[1]]],main=names(lw_wy)[1])

## For each WBIC
lw_w <- nlme::lmList(logw~logl|wbic,data=lw,na.action=na.pass)
w_res <- LWINFO(lw_w,"WBIC")
fitPlot(lw_w[[names(lw_w)[1]]],main=names(lw_w)[1])

## For each lake class
lw_c <- nlme::lmList(logw~logl|class,data=lw,na.action=na.pass)
c_res <- LWINFO(lw_c,"CLASS")
fitPlot(lw_c[[names(lw_c)[1]]],main=names(lw_c)[1])

## For all fish
lw_a <- lm(logw~logl,data=lw)
tmp <- summary(lw_a)
tmp2 <- model.frame(lw_a)$logl
a_res <- data.frame(coef(lw_a)[1],coef(lw_a)[2],rsq=tmp$r.squared,
                    n=tmp$df[2]+2,minLen=exp(min(tmp2)),
                    maxLen=exp(max(tmp2)),which="ALL",type="ALL") %>%
  rename(loga=coef.lw_a..1.,b=coef.lw_a..2.)
fitPlot(lw_a)


# ======================================================================
# Combine all regressions into one data.frame
## Add a "use" variable
##    NO for "small" n, "low" r-squared, weird b ("far" from 3)
##    as defined by these "cut"offs
n.cut <- 25; rsq.cut <- 0.85; b.cut <- c(2,4)
## Add exponentiated intercept (a) variable
lw_res <- rbind(wy_res,w_res,c_res,a_res) %>%
  mutate(use=case_when(n < n.cut ~ "NO",rsq < rsq.cut ~ "NO",
                       b < b.cut[1] | b > b.cut[2] ~ "NO",
                       TRUE ~ "yes"),
         reason=case_when(n < n.cut ~ paste("n<",n.cut),
                          rsq < rsq.cut ~ paste("r^2<",rsq.cut),
                          b < b.cut[1] | b > b.cut[2] ~ "b far from 3",
                          TRUE ~ ""),
         use=factor(use),reason=factor(reason),
         a=exp(loga)) %>%
  select(use,type,which,loga,a,b,n,rsq,minLen,maxLen,reason)
headtail(lw_res)

# ======================================================================
# Output results to a file in data/prepped/
if (writePreppedFiles) write.csv(lw_res,"data/prepped/LWregs.csv",
                                 quote=FALSE,row.names=FALSE)

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
