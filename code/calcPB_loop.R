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
  tmp <- doALK(fmdb_1,ALKInfo,"empirical")
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
      calcPB(age.c=1,num.c=4,twt.c=5,area=HA[i])
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
head(PB_res)




# ======================================================================
## Compare to Daisuke's results
library(readxl)
DK_res <- read_xlsx("scratch/zzzOld_FromDaisuke/Daisuke Production Estimates.xlsx") %>%
  rename(wbic=WBIC,year=Year,wbic_year=WBIC_Year,P="P (kg/ha/y)",B="B (kg/ha)")
str(DK_res)

PB_DK <- inner_join(PB_res[,c("wbic_year","n","P","B")],
                    DK_res[,c("wbic_year","P","B")],by="wbic_year")

plot(B.x~B.y,data=PB_DK,pch=19,col=col2rgbt("black",1/5),
     xlab="Daisuke's B",ylab="Derek's B")
abline(a=0,b=1,lty=2,col="red")
plot(I(B.x-B.y)~B.y,data=PB_DK,pch=19,col=col2rgbt("black",1/5),
     xlab="Daisuke's B",ylab="Derek's B - Daisuke's B",ylim=c(-10,10))
abline(h=0,lty=2,col="red")


plot(P.x~P.y,data=PB_DK,pch=19,col=col2rgbt("black",1/5),
     xlab="Daisuke's P",ylab="Derek's P")
abline(a=0,b=1,lty=2,col="red")
plot(I(P.x/B.x)~I(P.y/B.y),data=PB_DK,pch=19,col=col2rgbt("black",1/5),
     xlim=c(0,0.7),ylim=c(0,0.7),
     xlab="Daisuke's P/B",ylab="Derek's P/B")
abline(a=0,b=1,lty=2,col="red")
