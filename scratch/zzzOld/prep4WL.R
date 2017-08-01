# note that nlme package must be installed to use lmList below

# Load length-age data
##  Changed sex code to words
##  Added logged versions of length and weight
##  Removed unknown sexed fish
lenMass <- read.csv("data/WIwalleye_len_mass.csv") %>%
  mutate(sex=mapvalues(sex,from=c(0,1,2),to=c("male","female","unknown")),
         logL=log(length),logW=log(mass)) %>%
  filterD(sex!="unknown")

##  Added on region codes
##  !!!! This produces many NAs because not all WBICs in lenAge are in WBICchar
lenMass <- left_join(lenMass,select(WBICchar,wbic,region),by="wbic")

##  Created codes that combined wbic and sex; and region; and year
lenMass %<>% mutate(wbic_sex=paste(wbic,sex,sep="_"),
                    wbic_sex_region=paste(wbic_sex,region,sep="_"),
                    wbic_sex_year=as.factor(paste(wbic_sex,year,sep="_")))

# Weight-Length by wbic, sex, year
##  Get individual regressions
###   na.action needed because of NAs in other variables in the data.frame
WLbyWSY <- nlme::lmList(logW~logL|wbic_sex_year,data=lenMass,na.action=na.pass)
##  Add on sample sizes, reorder columns, rename columns
WLbyWSY <- data.frame(coef(WLbyWSY),xtabs(~wbic_sex_year,data=lenMass))[,c(3,1,2,4)]
names(WLbyWSY)[2:4] <- c("Intercept","Slope","n")
## Get rid of rownames (aethetically more pleasing)
rownames(WLbyWSY) <- NULL
## Only retain results with n>=15
WLbyWSY %<>% filterD(n>=15)
## Examine first few rows
head(WLbyWSY)

# Weight-Length by wbic, sex, region
WLbyWSR <- nlme::lmList(logW~logL|wbic_sex_region,data=lenMass,na.action=na.pass)
WLbyWSR <- data.frame(coef(WLbyWSR),xtabs(~wbic_sex_region,data=lenMass))[,c(3,1,2,4)]
names(WLbyWSR)[2:4] <- c("Intercept","Slope","n")
rownames(WLbyWSR) <- NULL
WLbyWSR %<>% filterD(n>=15)
head(WLbyWSR)

# Weight-Length by wbic, sex, region
WLbyWS <- nlme::lmList(logW~logL|wbic_sex,data=lenMass,na.action=na.pass)
WLbyWS <- data.frame(coef(WLbyWS),xtabs(~wbic_sex,data=lenMass))[,c(3,1,2,4)]
names(WLbyWS)[2:4] <- c("Intercept","Slope","n")
rownames(WLbyWS) <- NULL
WLbyWS %<>% filterD(n>=15)
head(WLbyWS)

# Weight-Length, completely pooled
WL <- coef(lm(logW~logL,data=lenMass))
names(WL) <- c("Intercept","Slope")

# Remove data.frame and garbage collect
rm(lenMass)
gc()

