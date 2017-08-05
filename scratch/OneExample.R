# ######################################################################
# ======================================================================
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
# Source local file
source("code/calcPB_function.R")
source("code/productionHelpers.R")



# ======================================================================
# Load the data.frames and do initial wranglings
## Load WBIC characteristics
wbic <- read.csv("data/prepped/wbicInfo.csv",stringsAsFactors=FALSE)
## Load length data
fmdb <- read.csv("data/prepped/fmdb_WAE.csv",stringsAsFactors=FALSE)
## Population estimates
### Removed WBIC_YEARs for which no FMDB data existed
pe <- read.csv("data/prepped/PE.csv")
rows2delete <- which(!pe$wbic_year %in% unique(fmdb$wbic_year))
pe <- pe[-rows2delete,]
cat(length(rows2delete),"WBIC_YEARs were removed from PE data.frame
    because no matching data in FMDB data.frame.")
## Load weight-length regression results
### Only retain regressions results that are valid to use
### Remove variables that defined use and reason for not using
LWRegs <- read.csv("data/prepped/LWregs.csv",stringsAsFactors=FALSE) %>%
  filterD(use=="yes") %>%
  select(-use,-reason)
## Load age-length-key information
### Only retain information for ALKs that are valid to use
### Retain only variables that are need when using the ALK
ALKInfo <- read.csv("data/prepped/ALKInfo.csv",stringsAsFactors=FALSE) %>%
  filterD(use=="yes") %>%
  select(type,which,ename,sname)





# ======================================================================

# Choose a wbic
WBIC_YEAR <- "1001300_1999"
# Isolate length data
fmdb_1 <- filterD(fmdb,wbic_year==WBIC_YEAR)
# get PE and size data
PE <- getPE(fmdb_1,pe)
HA <- getSize(fmdb_1,wbic)
# Find WL regression, Predict and add weights
tmp <- doLWReg(fmdb_1,LWRegs)
fmdb_1 <- tmp$df
# Find ALK
tmp <- doALK(fmdb_1,ALKInfo,"empirical")
fmdb_1 <- tmp$df
headtail(fmdb_1)

if (nrow(fmdb_1)>1) {
  sum_1 <- group_by(fmdb_1,age) %>%
    summarize(snum=n(),mwt=mean(wt)/1000) %>%
    mutate(pnum=snum/sum(snum)*PE,twt=pnum*mwt) %>%
    calcPB(age.c="age",num.c="pnum",twt.c="twt",area=HA,adjAgeGaps=TRUE)
  P <- sum_1$PperA
  B <- sum_1$BperA
} else P <- B <- NA
P
B
round(sum_1$df,2)
