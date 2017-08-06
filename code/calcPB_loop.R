## Clear workspace and console
rm(list=ls()); cat("\014")

# ######################################################################
# ======================================================================
# This script will loop through all valid WBIC_YEARs (i.e., has a PE and
# has some measured fish in the fmdb) and calculate P and B for each.
# Those values, along with some other information are then output to a
# file in the results/ folder for further analysis.
#
# This script requires that the Data_Prepper, calcLWRegs, and calcALKs
# scripts have all been successfully run (i.e., their resultant files
# were created and stored in the data/prepped/ folder.)
# ======================================================================
# ######################################################################

# ======================================================================
# Setup
## Load required packages
library(FSA)
library(dplyr)
# Source local file
source("code/helpers/calcPB.R")
source("code/helpers/productionHelpers.R")



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
## Put in a loop
wys <- as.character(pe$wbic_year)
ttl.wys <- length(wys)
reg.src <- reg.type <- alk.src <- alk.type <- alk.note <- character(ttl.wys)
PE <- HA <- numAges <- n <- P <- B <- numeric(ttl.wys)
for (i in 1:ttl.wys) {
  print(paste0("i=",i," of ",ttl.wys," (",wys[i],")"))
  fmdb_1 <- filterD(fmdb,wbic_year==wys[i])
  # Get WBIC_YEAR PE and WBIC size
  PE[i] <- getPE(fmdb_1,pe)
  HA[i] <- getSize(fmdb_1,wbic)
  tmp <- doLWReg(fmdb_1,LWRegs)
  fmdb_1 <- tmp$df
  reg.src[i] <- tmp$reg$which; reg.type[i] <- tmp$reg$type
  tmp <- doALK(fmdb_1,ALKInfo,"smoothed")
  fmdb_1 <- tmp$df
  alk.src[i] <- tmp$which; alk.type[i] <- tmp$type
  alk.note[i] <- tmp$note
  # Calculate P and B
  ## get number and mean weight (kg) in each age-class
  ## expand number in sample to number in popn (with PE value)
  ## add a total weight
  ## send to calcPB to compute P and B
  n[i] <- nrow(fmdb_1)
  numAges[i] <- length(unique(fmdb_1$age))
  if (numAges[i]>1) {
    sum_1 <- group_by(fmdb_1,age) %>%
      summarize(snum=n(),mwt=mean(wt)/1000) %>%
      mutate(pnum=snum/sum(snum)*PE[i],twt=pnum*mwt) %>%
      calcPB(age.c="age",num.c="pnum",twt.c="twt",area=HA[i],adjAgeGaps=TRUE)
    P[i] <- sum_1$PperA
    B[i] <- sum_1$BperA
    ## Print out the calculation table so it can be examined later
    write.csv(sum_1$df,paste0("results/CalcPB_Tables/PB_",wys[i],".csv"),
              quote=FALSE,row.names=FALSE)
  } else P[i] <- B[i] <- NA
}


# ======================================================================
## Put Results together
split.wy <- do.call(rbind,strsplit(wys,"_"))
PB_res <- data.frame(wbic_year=wys,wbic=split.wy[,1],year=split.wy[,2],
                     PE,HA,n,numAges,P,B,
                     reg.src,reg.type,alk.src,alk.type,alk.note)
## Add use and reason variables
n.cut <- 30; ages.cut <- 5
PB_res %<>% mutate(use=case_when(n<n.cut ~ "NO",
                                 numAges<ages.cut ~ "NO",
                                 TRUE ~ "yes"),
                   reason=case_when(n<n.cut ~ paste0("n<",n.cut),
                                    numAges<ages.cut ~ paste0("numAges<",ages.cut),
                                    TRUE ~ "yes"))
## Write the file out to the results folder with a time stamp in name
write.csv(PB_res,paste0("results/PB_",format(Sys.time(),"%d%b%Y_%H%M"),".csv"),
          quote=FALSE,row.names=FALSE)
