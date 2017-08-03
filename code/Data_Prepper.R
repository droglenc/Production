# ######################################################################
# ======================================================================
# This script read the original data files as delivered to us from the
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
# writePreppedFiles object below is set to TRUE. If writePreppedFiles=
# TRUE, then a log will be written in the prepped files folder. The log
# should be carefully compared to old logs to determine if changes
# caused any major issues (too few variables, too few records, etc.)
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
library(plyr)
library(dplyr)
library(magrittr)
## Set whether old prepped files should be over-written (see above)
writePreppedFiles <- TRUE
## Initialize log variable
log <- c(paste0("Ran Data Prepper to create new prepped files on ",
                date(),".","\n","\n"))
## Set the random number seed. Expanding the lengths of the fish
## generates a random length between the lower and upper lengths of the
## length bin. This help keep some constancy in that randomization.
set.seed(82340934)



# ======================================================================
# Read and prep the PE data file.
## Renamed variables (preference and consistency)
## Added wbic-year combination variable
## Reordered variables and sorted rows by wbic and year (preference)
PE <- read.csv("data/original/walleye_pe_data.csv")
log <- c(log,paste("Loaded PE.csv with",ncol(PE),"variables and",
                 nrow(PE),"records."))
PE %<>% rename(wbic=WBIC,year=Year) %>%
  mutate(wbic_year=paste(wbic,year,sep="_")) %>%
  select(wbic_year,wbic,year,PE) %>%
  arrange(wbic,year)
## Removed wbics that ended in 1 (these are lake chains) ... check this
##   by running first two lines below and PE[(PE$wbic %% 10)==1,]
tmp <- nrow(PE)
PE %<>% filterD(!(wbic %% 10)==1)
log <- c(log,paste("Removing lake chains deleted",tmp-nrow(PE),
                 "WBIC_YEAR PE values."))

# Found unique wbic and wbic_year codes in the PE file.
## We must have a PE to estimate P and B. Thus, these wbic codes can be
## used to filter down the other (ROW, FMDB, etc.) files so that we
#  limit memory issues
wbics <- unique(PE$wbic)
wbic_years <- unique(PE$wbic_year)
log <- c(log,paste("There are",length(wbic_years),"unique WBIC_YEARS and",
                   length(wbics),"unique WBICs in PE.csv."))
## Write out the file ... PE.csv
if (writePreppedFiles) {
  write.csv(PE,"data/prepped/PE.csv",row.names=FALSE)
  log <- c(log,paste("Saved prepped PE.csv with",ncol(PE),"variables and",
                   nrow(PE),"records.\n\n"))
}
rm(PE,tmp)




# ======================================================================
# Read and prep the WBIC characteristics file (ROW)
## Reduced to only WBICs for which we have a PE
## Selected only needed columns, renamed those columns
##   Did not include area, lat, long, or depths as those values seemed
##   more complete in the lake classification file that AR sent (below)
## Converted mean depth to meters
wbicInfo <- read.csv("data/original/ROW.csv")
log <- c(log,paste("Loaded ROW file with",ncol(wbicInfo),"variables and",
                   nrow(wbicInfo),"records."))
wbicInfo %<>% filterD(WBIC %in% wbics) %>%
  select(WBIC,OFFICIAL_NAME,WATERBODY_TYPE_DESC,MEAN_DEPTH_FT) %>%
  rename(wbic=WBIC,name=OFFICIAL_NAME,wb_type=WATERBODY_TYPE_DESC,
         mean_depth=MEAN_DEPTH_FT) %>%
  mutate(mean_depth=mean_depth*0.3048)
headtail(wbicInfo)

## Read AR's lake classifications
## Selected only needed columns, renamed those columns
## Made lake class names simpler
wbicInfo2 <- read.csv("data/original/Supplementary Dataset 8 - Final Lake Class List.csv",stringsAsFactors=FALSE)
log <- c(log,paste("Loaded Rypel's Lake Class List file with",
                   ncol(wbicInfo2),"variables and",nrow(wbicInfo2),"records."))
wbicInfo2 %<>% select(WBIC,County,Area..ha.,Max.Depth..m.,
                      Latitude,Longitude,Final.Lake.Class) %>%
  rename(wbic=WBIC,county=County,size=Area..ha.,max_depth=Max.Depth..m.,
         lat=Latitude,long=Longitude,class=Final.Lake.Class) %>%
  mutate(class=mapvalues(class,
                         from=c("Complex - Cool - Clear","Complex - Cool - Dark",
                                "Complex - Riverine","Complex - Two Story",
                                "Complex - Warm - Clear","Complex - Warm - Dark",
                                "Simple - Cool - Clear","Simple - Cool - Dark",
                                "Simple - Harsh - Has Fishery","Simple - Harsh - No Fishery",
                                "Simple - Riverine","Simple - Trout Pond",
                                "Simple - Two Story","Simple - Warm - Clear",
                                "Simple - Warm - Dark"),
                         to=c("CCC","CCD","CR","C2S","CWC","CWD","SCC","SCD",
                              "SHF","SHN","SR","STP","S2S","SWC","SWD")))
headtail(wbicInfo2)

## Joined the county names and lake class variables to the ROW file
## Rearranged columns and ordered rows by WBIC
wbicInfo <- plyr::join(wbicInfo,wbicInfo2,by="wbic") %>%
  select(wbic,name,county,class,wb_type,lat,long,size,max_depth,mean_depth) %>%
  arrange(wbic)
## Note that only 9 WBICs were in three "simple" lake classifications --
##   collapsed these three classes into one called "SIM" for simple.
xtabs(~class,data=wbicInfo)
wbicInfo %<>% mutate(class=mapvalues(class,from=c("SCC","SWC","SWD"),
                                     to=c("SIM","SIM","SIM")))
xtabs(~class,data=wbicInfo)
headtail(wbicInfo)
## Write out the file ... wbicInfo.csv
if (writePreppedFiles) {
  write.csv(wbicInfo,"data/prepped/wbicInfo.csv",row.names=FALSE)
  log <- c(log,paste("Saved prepped wbicInfo.csv with",ncol(wbicInfo),
                     "variables and",nrow(wbicInfo),"records.\n\n"))
}



# ======================================================================
# Read and prep the FMDB data file
## Selected only needed columns, renamed those columns
## Reduced to only WBIC_YEARs for which we have a PE
fmdb <- read.csv("data/original/walleye_raw_2012.csv",
                 stringsAsFactors=FALSE,na.strings=c("-","NA",""))
log <- c(log,paste("Loaded walleye FMDB file with",ncol(fmdb),
                   "variables and",nrow(fmdb),"records."))
fmdb %<>% select(WBIC,Survey.Year,Length.or.Lower.Length.IN,
                 Length.Upper.IN,Number.of.Fish,Gender) %>%
  rename(wbic=WBIC,year=Survey.Year,sex=Gender) %>%
  mutate(wbic_year=paste(wbic,year,sep="_")) %>%
  filterD(wbic_year %in% wbic_years)
## Handling database errors or issues (per AR e-mail 1-Aug-17)
## Don't use filterD() because it removes NAs which causes issues!!!!!
## Removed rows where a number of fish is given, but no lengths
rows2delete <- which(fmdb$Number.of.Fish>0 & is.na(fmdb$Length.or.Lower.Length.IN))
if (length(rows2delete)>0) fmdb <- fmdb[-rows2delete,]
log <- c(log,paste("Deleted",length(rows2delete),"rows with fish but no length."))
## Removed rows with "lower" length greater than "upper" length
rows2delete <- which(fmdb$Length.or.Lower.Length.IN>fmdb$Length.Upper.IN)
if (length(rows2delete)>0) fmdb <- fmdb[-rows2delete,]
log <- c(log,paste("Deleted",length(rows2delete),"rows with lower length > upper length."))
### Removed rows with negative lengths
rows2delete <- which(fmdb$Length.or.Lower.Length.IN<0 | fmdb$Length.Upper.IN<0)
if (length(rows2delete)>0) fmdb <- fmdb[-rows2delete,]
log <- c(log,paste("Deleted",length(rows2delete),"rows with negative lengths."))
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
log <- c(log,paste("Deleted",length(rows2delete),
                   "rows from the FMDB in a 'small fish' bin."))
## If no or zero numbers of fish but there are lengths then 
## assume there is one fish.
fmdb$Number.of.Fish[is.na(fmdb$Number.of.Fish) & 
                      !is.na(fmdb$Length.or.Lower.Length.IN)] <- 1
fmdb$Number.of.Fish[fmdb$Number.of.Fish==0 & 
                      !is.na(fmdb$Length.or.Lower.Length.IN)] <- 1
## Generated individual lengths for fish recorded in length bins
tmp <- capture.output(fmdb %<>% expandCounts(~Number.of.Fish,
                         ~Length.or.Lower.Length.IN+Length.Upper.IN,
                         new.name="len.in"),type="message")
log <- c(log,tmp[-5])
## Added a length in mm variable
## Rearranged the columns and sorted the rows by wbic, year, and length
fmdb %<>% mutate(len.mm=len.in*25.4) %>%
  select(wbic_year,wbic,year,len.in,len.mm,sex,lennote) %>%
  arrange(wbic,year,len.mm)
## Join on the county and lake class variables
##   Note that only 3 vars are kept in wbicInfo so that only county
##     and class (and not mean_depth, etc.) will be added
##   Note that plyr::join() must be used rather than dplyr::left_join()
##     because of memory issues with left_join()
fmdb <- plyr::join(fmdb,wbicInfo[,c("wbic","county","class")],by="wbic")
headtail(fmdb)
## Write out the file ... fmdb_WAE.csv
if (writePreppedFiles) {
  write.csv(fmdb,"data/prepped/fmdb_WAE.csv",row.names=FALSE)
  log <- c(log,paste("Saved prepped fmdb_WAE.csv with",ncol(fmdb),
                     "variables and",nrow(fmdb),"records.\n\n"))
}
rm(fmdb,rows2delete,tmp)




# ======================================================================
# Read and prep the age-length-weight data file
## Reduced to only WBICs for which we have a PE
## Selected only needed columns, renamed those columns -- note that the
##   Number.of.Fish was not selected because all 1s
## Added length in mm and wbic-year combination variables
## Converted weight to numeric (was character because of commas)
## Rearranged columns and sorted rows by wbic, year, length, and age
lwa <- read.csv("data/original/length_weight_age_raw_data_8_1_17.csv",
                stringsAsFactors=FALSE,na.strings=c("-","NA",""))
log <- c(log,paste("Loaded length_weight_age file with",
                   ncol(lwa),"variables and",nrow(lwa),"records."))
lwa %<>% select(WBIC,Survey.Year,Length.IN,Weight.Grams,
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
## Join on the county and lake class variables
##   Note that only 3 vars are kept in wbicInfo so that only county
##     and class (and not mean_depth, etc.) will be added
## Rearranged columns
lwa <- plyr::join(lwa,wbicInfo[,c("wbic","county","class")],by="wbic") %>%
  select(wbic_year,wbic,year,county,class,len.in,len.mm,wt,age,sex,strux)
# Isolate those fish that have both length and weight
## Remove age and strux variables
lw <- filterD(lwa,!is.na(len.mm),!is.na(wt)) %>%
  select(-age,-strux)
log <- c(log,paste(nrow(lw),"fish had lengths and weights."))
## Removed fish for which the weight variable was =0
rows2delete <- which(lw$wt==0)
if (length(rows2delete)>0) lw <- lw[-rows2delete,]
log <- c(log,paste("Deleted",length(rows2delete),"rows with weights of zero."))
## Removed fish for which the len.in variable was <1
rows2delete <- which(lw$len.in<1)
if (length(rows2delete)>0) lw <- lw[-rows2delete,]
log <- c(log,paste("Deleted",length(rows2delete),"rows with a length (in) < 1."))
## Removed fish for which the wt (in grams) variable was <1
rows2delete <- which(lw$wt<1)
if (length(rows2delete)>0) lw <- lw[-rows2delete,]
log <- c(log,paste("Deleted",length(rows2delete),"rows with a weight (g) < 1."))
## Write out the file ... len_wt.csv
if (writePreppedFiles) {
  write.csv(lw,"data/prepped/len_wt.csv",row.names=FALSE)
  log <- c(log,paste("Saved prepped len_wt.csv with",ncol(lw),
                     "variables and",nrow(lw),"records."))
}

# Isolate those fish that have both length and age
## Remove wt variable
la <- filterD(lwa,!is.na(len.mm),!is.na(age)) %>%
  select(-wt)
log <- c(log,paste(nrow(la),"fish had lengths and ages."))
## Write out the file ... len_age.csv
if (writePreppedFiles) {
  write.csv(la,"data/prepped/len_age.csv",row.names=FALSE)
  log <- c(log,paste("Saved prepped len_age.csv with",ncol(la),
                     "variables and",nrow(la),"records.\n\n"))
}
rm(lwa,lw,la)
rm(wbicInfo,wbicInfo2,wbic_years,wbics)

# Write out log file
logconn <- file(paste0("data/prepped/dataPrepper_logs/dataPrepperLog_",
                       format(Sys.time(),"%d%b%Y_%H%M"),".txt"))
writeLines(log,logconn)
close(logconn)
