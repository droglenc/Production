# ######################################################################
# ======================================================================
# These scripts read the original data files as delivered to us from the
# WiDNR database and located in 'data/originals/'. The data files are 
# then manipulated, mostly to reduce the number of unneeded variables
# (columns), remove unneeded records (rows), and create new veriables
# that will be needed for the full analysis. The results of these
# wranglings are output to appropriate CSV files in 'data/prepped/'.
# The prepped files are read and their data further wrangled as 
# appropriate for the larger analysis.
#
# THESE SCRIPTS SHOULD NOT NEED TO BE RUN AGAIN AS THEY WILL OVERWRITE
# THE PREPPED FILES IN 'data/original/'.

# However, as a safeguard, the files will only be overwritten if the
# writePreppedFiles object below is set to TRUE.
#
# ======================================================================
# ######################################################################



# ======================================================================
# Setup
## Clear workspace and console
rm(list=ls())
cat("\014")
##  Load required packages
library(FSA)
library(dplyr)
library(magrittr)
## Set whether old prepped files should be over-written (see above)
writePreppedFiles <- TRUE
## Set the random number seed. Expanding the lengths of the fish
## generates a random length between the lower and upper lengths of the
## length bin. This help keep some constancy in that randomization.
set.seed(82340934)



# ======================================================================
# Read and prep the PE data file.
## Renamed variables (preference and consistency)
## Added wbic-year combination variable
## Reordered variables and sorted rows by wbic and year (preference)
PE <- read.csv("data/original/walleye_pe_data.csv") %>%
  rename(wbic=WBIC,year=Year) %>%
  mutate(wbic_year=paste(wbic,year,sep="_")) %>%
  select(wbic_year,wbic,year,PE) %>%
  arrange(wbic,year)
## Removed wbics that ended in 1 (these are lake chains) ... check this
##   by running first two lines below and PE[(PE$wbic %% 10)==1,]
tmp <- nrow(PE)
PE %<>% filterD(!(wbic %% 10)==1)
cat("Removing lake chains deleted",tmp-nrow(PE),"WBIC_YEAR PE values.\n")
if (writePreppedFiles) write.csv(PE,"data/prepped/PE.csv",row.names=FALSE)

# Found unique wbic and wbic_year codes in the PE file.
## We must have a PE to estimate P and B. Thus, these wbic codes can be
## used to filter down the other (ROW, FMDB, etc.) files so that we
#  limit memory issues
wbics <- unique(PE$wbic)
wbic_years <- unique(PE$wbic_year)
rm(PE)



# ======================================================================
# Read and prep the FMDB data file
## Selected only needed columns, renamed those columns
## Reduced to only WBIC_YEARs for which we have a PE
fmdb <- read.csv("data/original/walleye_raw_2012.csv",
                 stringsAsFactors=FALSE,na.strings=c("-","NA","")) %>%
  select(WBIC,Survey.Year,Length.or.Lower.Length.IN,Length.Upper.IN,
         Number.of.Fish,Gender) %>%
  rename(wbic=WBIC,year=Survey.Year,sex=Gender) %>%
  mutate(wbic_year=paste(wbic,year,sep="_")) %>%
  filterD(wbic_year %in% wbic_years)
## Handling database errors or issues (per AR e-mail 1-Aug-17)
## Don't use filterD() because it removes NAs which causes issues
## Removed rows where a number of fish is given, but no lengths
rows2delete <- which(fmdb$Number.of.Fish>0 & is.na(fmdb$Length.or.Lower.Length.IN))
if (length(rows2delete)>0) fmdb <- fmdb[-rows2delete,]
cat("Deleted",length(rows2delete),"rows from the FMDB with fish but no length.\n")
## Removed rows with "lower" length greater than "upper" length
rows2delete <- which(fmdb$Length.or.Lower.Length.IN>fmdb$Length.Upper.IN)
if (length(rows2delete)>0) fmdb <- fmdb[-rows2delete,]
cat("Deleted",length(rows2delete),"rows from the FMDB with lower length > upper length.\n")
### Removed rows with negative lengths
rows2delete <- which(fmdb$Length.or.Lower.Length.IN<0 | fmdb$Length.Upper.IN<0)
if (length(rows2delete)>0) fmdb <- fmdb[-rows2delete,]
cat("Deleted",length(rows2delete),"rows from the FMDB with negative lengths.\n")
## Removed rows where a number of fish is given, but the lower length
## is zero and the upper length is positive. I think this is for fish
## that were counted as less than the upper length. Two of the upper
## lengths were 50 which were likely typos, one was 12, two were 9,
## and the rest were <6.9. Only the 12 would likely come close to
## affecting P calculations, but there is no way to assign realistic
## lengths to these fish (because will be between 0 and 12).
rows2delete <- which(fmdb$Length.or.Lower.Length.IN==0 & 
                       fmdb$Length.Upper.IN>0 & fmdb$Number.of.Fish>0)
if (length(rows2delete)>0) fmdb <- fmdb[-rows2delete,]
cat("Deleted",length(rows2delete),"rows from the FMDB in a 'small fish' bin.\n")
## If no or zero numbers of fish but there are lengths then 
## assume there is one fish.
fmdb$Number.of.Fish[is.na(fmdb$Number.of.Fish) & 
                      !is.na(fmdb$Length.or.Lower.Length.IN)] <- 1
fmdb$Number.of.Fish[fmdb$Number.of.Fish==0 & 
                      !is.na(fmdb$Length.or.Lower.Length.IN)] <- 1
## Generated individual lengths for fish recorded in length bins
## Added a length in mm variable
## Rearranged the columns and sorted the rows by wbic, year, and length
fmdb %<>% expandCounts(~Number.of.Fish,
                       ~Length.or.Lower.Length.IN+Length.Upper.IN,
                       new.name="len.in") %>%
  mutate(len.mm=len.in*25.4) %>%
  select(wbic_year,wbic,year,len.in,len.mm,sex,lennote) %>%
  arrange(wbic,year,len.mm)
str(fmdb)
headtail(fmdb)
if (writePreppedFiles) write.csv(fmdb,"data/prepped/fmdb_WAE.csv",row.names=FALSE)
rm(fmdb)



# ======================================================================
# Read and prep the age-length-weight data file
## Reduced to only WBICs for which we have a PE
## Selected only needed columns, renamed those columns -- note that the
##   Number.of.Fish was not selected because all 1s
## Added length in mm and wbic-year combination variables
## Converted weight to numeric (was character because of commas)
## Rearranged columns and sorted rows by wbic, year, length, and age
lwa <- read.csv("data/original/walleye_length_weight_age.csv",
                stringsAsFactors=FALSE,na.strings=c("-","NA","")) %>%
  select(WBIC,Survey.Year,Length.IN,Weight.Grams,
         Age..observed.annuli.,Age.Structure.,Gender) %>%
  rename(wbic=WBIC,year=Survey.Year,sex=Gender,
         len.in=Length.IN,wt=Weight.Grams,
         age=Age..observed.annuli.,strux=Age.Structure.) %>%
  mutate(len.mm=len.in*25.4,
         wt=as.numeric(gsub(',','',wt)),
         wbic_year=paste(wbic,year,sep="_")) %>%
  filterD(wbic_year %in% wbic_years) %>%
  select(wbic_year,wbic,year,len.in,len.mm,wt,age,sex,strux) %>%
  arrange(wbic,year,len.mm,age)

# Isolate those fish that have both length and weight
## Remove age and strux variables
lw <- filterD(lwa,!is.na(len.mm),!is.na(wt)) %>%
  select(-age,-strux)
str(lw)
headtail(lw)
if (writePreppedFiles) write.csv(lw,"data/prepped/len_wt.csv",row.names=FALSE)

# Isolate those fish that have both length and age
## Remove wt variable
la <- filterD(lwa,!is.na(len.mm),!is.na(age)) %>%
  select(-wt)
str(la)
headtail(la)
if (writePreppedFiles) write.csv(la,"data/prepped/len_age.csv",row.names=FALSE)
rm(lwa,lw,la)



# ======================================================================
# Read and prep the WBIC characteristics file (ROW)
## Reduced to only WBICs for which we have a PE
## Selected only needed columns, renamed those columns
## Converted lake size and depths to metrics units (ha, m)
## Rearranged columns and sorted rows by wbic
## !! NEED TO CREATE SOME SORT OF REGION VARIABLE (for ALK and LW)
ROW <- read.csv("data/original/ROW.csv") %>%
  filterD(WBIC %in% wbics) %>%
  select(WBIC,WBIC_TYPE_CODE,OFFICIAL_NAME,LOCAL_NAME,
         OFFICIAL_SIZE_ACRES,MAX_DEPTH_FT,MEAN_DEPTH_FT,WATERBODY_TYPE_DESC,
         CENTROID_LL_LAT_DD_AMT,CENTROID_LL_LONG_DD_AMT) %>%
  rename(wbic=WBIC,wbic_type=WBIC_TYPE_CODE,name=OFFICIAL_NAME,
         locname=LOCAL_NAME,wb_type=WATERBODY_TYPE_DESC,
         size=OFFICIAL_SIZE_ACRES,max_depth=MAX_DEPTH_FT,
         mean_depth=MEAN_DEPTH_FT,lat=CENTROID_LL_LAT_DD_AMT,
         long=CENTROID_LL_LONG_DD_AMT) %>%
  mutate(size=size*0.404686,max_depth=max_depth*0.3048,
         mean_depth=mean_depth*0.3048) %>%
  select(wbic,wbic_type,name,locname,wb_type,lat,long,size,max_depth,mean_depth) %>%
  arrange(wbic)
str(ROW)
headtail(ROW)
# which WBICs have PEs but nothing in ROW (can delete when this is fixed)
wbics[which(!wbics %in% ROW$wbic)]
if (writePreppedFiles) write.csv(ROW,"data/prepped/ROW.csv",row.names=FALSE)
rm(ROW)


