# Load required packages
library(FSA)
library(dplyr)
library(magrittr)

# Load length-age data
##  Created 20-mm length categories (from pseudocode doc)
##  Changed sex code to words
##  Filtered to just age-3 and older, Removed unknown sexed fish
lenAge <- read.csv("scratch/data_OldFromDaisuke/WIwalleye_age_len.csv") %>%
  mutate(lcat=lencat(length,w=20),
         sex=mapvalues(sex,from=c(0,1,2),to=c("male","female","unknown"))) %>%
  filterD(age>=3,sex!="unknown")

# Isolate for Two Sisters Lake
TSL_lenAge <- filterD(lenAge,wbic==1588200)


# Load length-weight data
##  Changed sex code to words
##  Added logged versions of length and weight
##  Removed unknown sexed fish
lenMass <- read.csv("scratch/data_OldFromDaisuke/WIwalleye_len-mass.csv") %>%
  mutate(sex=mapvalues(sex,from=c(0,1,2),to=c("male","female","unknown")),
         logL=log(length),logW=log(mass)) %>%
  filterD(sex!="unknown")

# Isolate for Two Sisters Lake
TSL_lenMass <- filterD(lenAge,wbic==1588200)


# Prep the ALKs and WL regression
source("code/prep4ALK.R")
source("code/prep4WL.R")
