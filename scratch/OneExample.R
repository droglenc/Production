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
source("code/calcPB.R")
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
pe <- pe[which(pe$wbic_year %in% unique(fmdb$wbic_year)),]
## Load weight-length regression results
### Only retain regressions results that are valid to use
### Remove variables that defined use and reason for not using
LWRegs <- read.csv("data/prepped/LWregs.csv",stringsAsFactors=FALSE) %>%
  filterD(use=="yes") %>%
  select(-use,-reason)
## Load age-length-key results





# ======================================================================
## Lists all WBIC_YEARS in PE, AL, and LW files - JUST FOR TESTING
wys <- as.character(pe$wbic_year)
# Choose a wbic 
( WBIC_YEAR <- wys[601] )

# Isolate length data
fmdb_1 <- filterD(fmdb,wbic_year==WBIC_YEAR)
# Find WL regression, Predict and add weights
tmp <- getLWReg(fmdb_1,LWRegs)
fmdb_1 %<>% mutate(wt=tmp$a*len.mm^tmp$b)

## Put in a loop to see if it errors
ttl.wys <- length(wys)
reg.src <- character(ttl.wys)
reg.type <- character(ttl.wys)
for (i in 1:ttl.wys) {
  print(paste0("i=",i))
  fmdb_1 <- filterD(fmdb,wbic_year==wys[i])
  tmp <- getLWReg(fmdb_1,LWRegs)
  fmdb_1 %<>% mutate(wt=tmp$a*len.mm^tmp$b)
  reg.src[i] <- tmp$which
  reg.type[i] <- tmp$type
}

xtabs(~reg.type)
# Predict and add ages (use alkIndivAge, but need to find appropriate ALK)
#fmdb_1 %<>%
  
#alkIndivAge(alk,~len.mm,data=.)


getPE(fmdb_1,pe,verbose=TRUE)

getSize(fmdb_1,wbic,verbose=TRUE)  
  


# Summarize
## get number and mean weight (kg) in each age-class
## expand number in sample to number in popn (with PE value)
## add a total weight
## send to calcPB to compute P and B
sum_1 <- group_by(fmdb_1,age) %>%
  summarize(num=n(),mwt=mean(wt)/1000) %>%
  mutate(num=num/sum(num)*getPE(fmdb_1,pe),twt=num*mwt) %>%
  calcPB(age.c=1,num.c=2,twt.c=4,area=getSize(fmdb_1,wbic))
round(sum_1$df,2)
sum_1$PperA
sum_1$BperA





