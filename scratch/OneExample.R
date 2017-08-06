# Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr)
# Source local file
source("code/helpers/calcPB.R")
source("code/helpers/productionHelpers.R")

# ======================================================================
# Analysis choices that can be made (at this stage)
## Type of ALK to use ("empirical" or "smoothed")
alk2use <- "smoothed"
## WBIC_YEAR
WBIC_YEAR <- "2962400_2009"

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
## Load age-length-key information
### Only retain information for ALKs that are valid to use
### Retain only variables that are need when using the ALK
ALKInfo <- read.csv("data/prepped/ALKInfo.csv",stringsAsFactors=FALSE) %>%
  filterD(use=="yes") %>%
  select(type,which,ename,sname)
## Load weight-length regression results
### Only retain regressions results that are valid to use
### Remove variables that defined use and reason for not using
LWRegs <- read.csv("data/prepped/LWregs.csv",stringsAsFactors=FALSE) %>%
  filterD(use=="yes") %>%
  select(-use,-reason)

# ======================================================================
# Isolate length data
fmdb_1 <- filterD(fmdb,wbic_year==WBIC_YEAR)
# get PE and size data
PE <- getPE(fmdb_1,pe)
HA <- getSize(fmdb_1,wbic)
# Find ALK
tmp <- doALK(fmdb_1,ALKInfo,alk2use)
fmdb_1 <- tmp$df
headtail(fmdb_1)
# Find WL regression, Predict and add weights
if (nrow(fmdb_1)>0) {
  tmp <- doLWReg(fmdb_1,"len.mm","wt",LWRegs)
  fmdb_1 <- tmp$df  
}

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
