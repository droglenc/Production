# Load length-age data
##  Created 20-mm length categories (from pseudocode doc)
##  Changed sex code to words
##  Filtered to just age-3 and older, Removed unknown sexed fish
lenAge <- read.csv("data/WIwalleye_len_age.csv") %>%
  mutate(lcat=lencat(length,w=20),
         sex=mapvalues(sex,from=c(0,1,2),to=c("male","female","unknown"))) %>%
  filterD(age>=3,sex!="unknown")

##  Added on region codes
##  !!!! This produces many NAs because not all WBICs in lenAge are in WBICchar
lenAge <- left_join(lenAge,select(WBICchar,wbic,region),by="wbic")

##  Created codes that combined wbic and sex; and region; and year
lenAge %<>% mutate(wbic_sex=paste(wbic,sex,sep="_"),
                   wbic_sex_region=paste(wbic_sex,region,sep="_"),
                   wbic_sex_year=paste(wbic_sex,year,sep="_"))
  

# Cross-Tabs by wbic, sex, year
##  Only for those with n>=15
ALKbyWSY <- xtabs(~lcat+age+wbic_sex_year,data=lenAge)
ALKbyWSY <- ALKbyWSY[,,apply(ALKbyWSY,FUN=sum,MARGIN=3)>=15]
##  Make row proportions ... the ALK
ALKbyWSY <- prop.table(ALKbyWSY,margin=c(3,1))
##  An example
round(ALKbyWSY[,,dimnames(ALKbyWSY)$wbic_sex_year[1]],2)
##  Will need to compute ALK later as needed

# Cross-Tabs by wbic, sex, region (Only for those with n>=15)
ALKbyWSR <- xtabs(~lcat+age+wbic_sex_region,data=lenAge)
ALKbyWSR <- ALKbyWSR[,,apply(ALKbyWSR,FUN=sum,MARGIN=3)>=15]
ALKbyWSR <- prop.table(ALKbyWSR,margin=c(3,1))
ALKbyWSR[,,dimnames(ALKbyWSR)$wbic_sex_region[1]]

# Cross-Tabs by wbic, sex (Only for those with n>=15)
ALKbyWS <- xtabs(~lcat+age+wbic_sex,data=lenAge)
ALKbyWS <- ALKbyWS[,,apply(ALKbyWS,FUN=sum,MARGIN=3)>=15]
ALKbyWS <- prop.table(ALKbyWS,margin=c(3,1))
ALKbyWS[,,dimnames(ALKbyWS)$wbic_sex[1]]

# Remove data.frame and garbage collect
rm(lenAge)
gc()
