## Clear workspace and console
rm(list=ls()); cat("\014")
# Load required packages
library(FSA); library(dplyr); library(magrittr)
# Source local helper files
source("code/helpers/calcPB.R")
source("code/helpers/productionHelpers.R")

# ######################################################################
# ======================================================================
# This script will loop through all valid WBIC_YEARs (i.e., has a PE and
# has some measured fish in the fmdb) and calculate P and B for each.
# Those values, along with some other information are then output to a
# file in the results/ folder for further analysis.
#
# This script differs from calcPB_loop.R in that it converts mean lengths
# to mean weights, instead of assigning a weight to individual fish.
#
# This script requires that the Data_Prepper, calcLWRegs, and calcALKs
# scripts have all been successfully run (i.e., their resultant files
# were created and stored in the data/prepped/ folder.)
# ======================================================================
# ######################################################################

# ======================================================================
# Analysis choices that can be made (at this stage)
## Minimum size of sample when computing P & B
n.cut <- 30
## Minimum number of ages in sample when computing P & B
ages.cut <- 5
## Type of ALK to use ("empirical" or "smoothed")
alk2use <- "smoothed"
## A name to add as a suffix to the results file (if NULL, use date-time)
results.suffix <- NULL



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
# Compute P & B (et al.) for each WBIC_YEAR
## Prepare for loop
wys <- as.character(pe$wbic_year)
ttl.wys <- length(wys)
reg.src <- reg.type <- alk.src <- alk.type <- alk.note <- character(ttl.wys)
PE <- HA <- numAges <- n <- P <- B <- numeric(ttl.wys)
## Loop through each WBIC_YEAR
for (i in 1:ttl.wys) {
  print(paste0("i=",i," of ",ttl.wys," (",wys[i],")"))
  fmdb_1 <- filterD(fmdb,wbic_year==wys[i])
  # Get WBIC_YEAR PE and WBIC size
  PE[i] <- getPE(fmdb_1,pe)
  HA[i] <- getSize(fmdb_1,wbic)
  # Apply ALKs
  tmp <- doALK(fmdb_1,ALKInfo,alk2use)
  fmdb_1 <- tmp$df
  alk.src[i] <- tmp$which; alk.type[i] <- tmp$type; alk.note[i] <- tmp$note
  # Get overall sample size
  n[i] <- nrow(fmdb_1)
  # Get number of ages present
  numAges[i] <- length(unique(fmdb_1$age))
  # Calculate P and B (if more than one age-class)
  ## get number and mean weight (kg) in each age-class
  ## expand number in sample to number in popn (with PE value)
  ## add a total weight
  ## send to calcPB to compute P and B
  if (numAges[i]>1) {
    sum_1 <- group_by(fmdb_1,age,wbic_year,wbic,year,class) %>%
      summarize(snum=n(),mlen=mean(len.mm)) %>%
      ungroup() %>% as.data.frame()
    tmp <- doLWReg(sum_1,"mlen","mwt",LWRegs)
    reg.src[i] <- tmp$reg$which; reg.type[i] <- tmp$reg$type    
    sum_1 <- tmp$df %>%
      select(-wbic_year,-wbic,-year,-class) %>%
      mutate(mwt=mwt/1000) %>%
      mutate(pnum=snum/sum(snum)*PE[i],twt=pnum*mwt) %>%
      calcPB(age.c="age",num.c="pnum",twt.c="twt",area=HA[i],adjAgeGaps=TRUE)
    P[i] <- sum_1$PperA
    B[i] <- sum_1$BperA
    ## Write out the calculation table so it can be examined later
    write.csv(sum_1$df,paste0("results/CalcPB_Tables/PB2_",wys[i],".csv"),
              quote=FALSE,row.names=FALSE)
  } else P[i] <- B[i] <- NA
}


# ======================================================================
# Put Results together
split.wy <- do.call(rbind,strsplit(wys,"_"))
PB_res <- data.frame(wbic_year=wys,wbic=split.wy[,1],year=split.wy[,2],
                     PE,HA,n,numAges,P,B,
                     reg.src,reg.type,alk.src,alk.type,alk.note)
## Add use and reason variables
PB_res %<>% mutate(use=case_when(n<n.cut ~ "NO",
                                 numAges<ages.cut ~ "NO",
                                 TRUE ~ "yes"),
                   reason=case_when(n<n.cut ~ paste0("n<",n.cut),
                                    numAges<ages.cut ~ paste0("numAges<",ages.cut),
                                    TRUE ~ "yes"))
## Write the file out to the results folder with a time stamp in name
if (is.null(results.suffix)) results.suffix <- format(Sys.time(),"%d%b%Y_%H%M")
write.csv(PB_res,paste0("results/PB2_",results.suffix,".csv"),
          quote=FALSE,row.names=FALSE)
