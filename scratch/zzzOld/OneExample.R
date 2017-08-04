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



# Load the data.frames and do initial wranglings
## Population estimates
pe <- read.csv("data/prepped/PE.csv")
## Load WBIC characteristics data
ROW <- read.csv("data/prepped/ROW.csv")
## Load length-age data
###  Created 20-mm length categories (from pseudocode doc)
###  Filtered to just age-3 and older, Removed unknown sexed fish
lenAge <- read.csv("data/prepped/len_age.csv") %>%
  mutate(lcat=lencat(len.mm,w=20)) %>%
  filterD(age>=3,sex %in% c("F","M"))
## Load length-weight data
###  Added logged versions of length and weight
###  Removed unknown sexed fish
lenWt <- read.csv("data/prepped/len_wt.csv") %>%
  mutate(logl=log(len.mm),logw=log(wt)) %>%
  filterD(sex %in% c("F","M"))
## Load length data
fmdb <- read.csv("data/prepped/fmdb_WAE.csv")



# Choose a wbic 
WBIC <- 2353600
# Choose a year
pe$year[grepl(WBIC,pe$wbic)]
YEAR <- 2002
WBIC_YEAR <- paste(WBIC,YEAR,sep="_")

# Isolate PE for that WBIC_YEAR
( pe_1 <- filterD(pe,wbic_year==WBIC_YEAR)$PE )
# Isolate lake size (ha) for that WBIC
( area_1 <- filterD(ROW,wbic==WBIC)$size )
# Create ALK
lenAge_1 <- filterD(lenAge,wbic_year==WBIC_YEAR)
alk <- prop.table(xtabs(~lcat+age,data=lenAge_1),margin=1)
# Create LW regression
lenWt_1 <- filterD(lenWt,wbic_year==WBIC_YEAR)
lw <- lm(logw~logl,data=lenWt_1)
# Isolate length data, add ages from ALK, predict weights
fmdb_1 <- filterD(fmdb,wbic_year==WBIC_YEAR) %>%
  alkIndivAge(alk,~len.mm,data=.) %>%
  mutate(wt=exp(predict(lw,data.frame(logl=log(len.mm)))))
# Examine data.frame
head(fmdb_1)

# Summarize
## get number and mean weight (kg) in each age-class
## expand number in sample to number in popn (with PE value)
## add a total weight
## send to calcPB to compute P and B
sum_1 <- group_by(fmdb_1,age) %>%
  summarize(num=n(),mwt=mean(wt)/1000) %>%
  mutate(num=num/sum(num)*pe_1,twt=num*mwt) %>%
  calcPB(age.c=1,num.c=2,twt.c=4,area=area_1)
round(sum_1$df,2)
sum_1$PperA
sum_1$BperA





## Lists all WBIC_YEARS in PE, AL, and LW files
wys <- as.character(pe$wbic_year)
wys_al <- as.character(unique(lenAge$wbic_year))
wys_lw <- as.character(unique(lenWt$wbic_year))

wys_all <- wys[wys %in% wys_al]
wys_all <- wys_all[wys_all %in% wys_lw]
wys_all
