# Load required packages
library(FSA)
library(dplyr)
library(magrittr)


# Get the WBIC characteristics
##  Convert numeric codes to strings
WBICchar <- read.csv("data/WIwalleye_WBIC_characteristics.csv") %>%
  mutate(lakesize=mapvalues(lakesize_code,from=c(0,1),to=c("<500 ha",">500 ha")),
         region=mapvalues(region_code,from=1:3,to=c("Northeast","Northwest","South"))) %>%
  select(-lakesize_code,-region_code)
