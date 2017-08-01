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
# following object is set to TRUE.
writePreppedFiles <- TRUE
# ======================================================================
# ######################################################################



# ======================================================================
# clear workspace and console and load required packages
library(FSA)
library(dplyr)
rm(list=ls())
cat("\014")



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
str(PE)
headtail(PE)
if (writePreppedFiles) write.csv(PE,"data/prepped/PE.csv",row.names=FALSE)

# Found unique wbic codes in the PE file.
## We must have a PE to estimate P and B. Thus, these wbic codes can be
## used to filter down the other (ROW, FMDB, etc.) files so that we
#  limit memory issues
wbics <- unique(PE$wbic)

rm(PE)



# ======================================================================
# Read and prep the FMDB data file
## Reduced to only WBICs for which we have a PE
## Selected only needed columns, renamed those columns
## Removed fish for which lengths were not recorded
## Generated lengths for each fish that was recorded in a bin of lengths
## Added length in mm and wbic-year combination variables
## Rearranged columns and sorted rows by wbic, year, and length
fmdb <- read.csv("data/original/walleye_fmdb_raw_2012.csv",
                 stringsAsFactors=FALSE,na.strings=c("-","NA","")) %>%
  filterD(WBIC %in% wbics) %>%
  select(WBIC,Station.Name,Survey.Year,Sample.Date,
         Length.or.Lower.Length.IN,Length.Upper.IN,Number.of.Fish,Gender) %>%
  rename(wbic=WBIC,station=Station.Name,year=Survey.Year,
         date=Sample.Date,sex=Gender) %>%
  #### Short-term work-around for data entry error, ultimately delete this??
  filterD(!is.na(Number.of.Fish),!Number.of.Fish<1) %>%
  ####
  #### Does this make sense ... what to do with fish with no lengths
  filterD(!is.na(Length.or.Lower.Length.IN)) %>%
  ####
  expandCounts(~Number.of.Fish,
               ~Length.or.Lower.Length.IN+Length.Upper.IN,
               new.name="len.in") %>%
  mutate(len.mm=len.in*25.4,
         wbic_year=paste(wbic,year,sep="_")) %>%
  select(wbic_year,wbic,year,station,date,len.in,len.mm,sex,lennote) %>%
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
  filterD(WBIC %in% wbics) %>%
  select(WBIC,Waterbody.Name,Survey.Year,Sample.Date,Length.IN,
         Weight.Grams,Age..observed.annuli.,Age.Structure.,Gender) %>%
  rename(wbic=WBIC,name=Waterbody.Name,year=Survey.Year,
         date=Sample.Date,sex=Gender,len.in=Length.IN,wt=Weight.Grams,
         age=Age..observed.annuli.,strux=Age.Structure.) %>%
  mutate(len.mm=len.in*25.4,
         wt=as.numeric(gsub(',','',wt)),
         wbic_year=paste(wbic,year,sep="_")) %>%
  select(wbic_year,wbic,year,date,name,len.in,len.mm,wt,age,sex,strux) %>%
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


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Possibly use this extract wbic,county pair information that you may ultimately join with the ROW data.frame.
#tmp <- read.csv("data/original/walleye_fmdb_raw_2012.csv",
#                stringsAsFactors=FALSE,na.strings=c("-","NA","")) %>%
#  filterD(WBIC %in% wbics) %>%
#  select(WBIC,County) %>%
#  unique()

