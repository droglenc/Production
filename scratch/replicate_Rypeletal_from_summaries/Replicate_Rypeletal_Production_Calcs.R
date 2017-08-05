library(dplyr)
source("code/calcPB_function.R")

## CJFAS2 ... Tenderfoot Lake example ... data from Rypel's Excel Sheet1
TLha <- 176.848
TLpe <- 3268
TLdf <- data.frame(age=3:12,
                   ppop=c(0.159090909,0.102272727,0.181818182,0.159090909,
                          0.113636364,0.079545455,0.045454545,0.056818182,
                          0.034090909,0.068181818),
                   mwt=c(0.233595447,0.368444815,0.632462338,0.840408144,
                         1.183904849,1.496548855,1.864091273,2.134780372,
                         2.936385973,3.405581956))
TLdf <- mutate(TLdf,num=TLpe*ppop,twt=num*mwt)
TLdf

TLres <- calcPB(TLdf,area=TLha,age.c="age",num.c="num",twt.c="twt")
TLres$df
TLres$BperA  # 20.61249
TLres$PperA  #  4.907816
# Matches Rypel's Excel sheet exactly


## CJFAS1 ... Escanaba Lake example ... data from Rypel's Excel Sheet2
ELha <- 118.5728
ELdf <- data.frame(age=c(0,3:15,18),
                   num=c(11983.7,924,745,29,501,170,108,46,58,79,17,4,4,4,4),
                   twt=c(1306.743864,270.71974,325.1945823,18.64802305,
                         436.9521289,188.4361874,157.6588566,81.2310472,
                         120.6657629,194.1304121,45.71067112,12.83364195,
                         13.53191397,13.76769242,15.62439983))

ELres <- calcPB(ELdf,area=ELha,age.c="age",num.c="num",twt.c="twt")
ELres$df
ELres$BperA  # 27.00323
ELres$PperA  #  6.07475
# Matches Rypel's Excel sheet exactly



# test names
ELdf2 <- ELdf
names(ELdf2) <- c("AGES","ABUNDANCE","BIOMASS")
ELres2 <- calcPB(ELdf2,area=ELha,age.c="AGES",num.c="ABUNDANCE",twt.c="BIOMASS")
ELres2$df



#### Hand Calculations ... learning before writing calcPB()
CJdf2 <- CJdf %>%
  mutate(mwt=twt/num,log.mwt=log(mwt),B=twt/CJha,
         mult=c(NA,rep(1,length(age)-1)),
         mB=zoo::rollmean(B,k=2,na.pad=TRUE,align="right")/mult,
         G=c(NA,diff(log.mwt)),
         P=mB*G)
CJdf2
( CJ.P <- sum(CJdf2$P,na.rm=TRUE) )
( CJ.B <- sum(CJdf2$B,na.rm=TRUE) )


ELdf2 <- ELdf %>%
  mutate(mwt=twt/num,log.mwt=log(mwt),B=twt/ELha,
         mult=c(NA,3,rep(1,length(age)-3),3),
         mB=zoo::rollmean(B,k=2,na.pad=TRUE,align="right")/mult,
         G=c(NA,diff(log.mwt)),
         P=mB*G)
ELdf2
( EL.P <- sum(ELdf2$P,na.rm=TRUE) )
( EL.B <- sum(ELdf2$B,na.rm=TRUE) )
