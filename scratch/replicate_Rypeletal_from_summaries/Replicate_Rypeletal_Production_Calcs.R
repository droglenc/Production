library(dplyr)
source("code/helpers/calcPB.R")

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

TLres <- calcPB(TLdf,age.c="age",num.c="num",twt.c="twt",
                area=TLha,lbl="Tenderfoot Example")
TLres$df
TLres$B  # 20.61249
TLres$P  #  4.907816
# Matches Rypel's Excel sheet exactly


## CJFAS1 ... Escanaba Lake example ... data from Rypel's Excel Sheet2
ELha <- 118.5728
ELdf <- data.frame(age=c(0,3:15,18),
                   num=c(11983.7,924,745,29,501,170,108,46,58,79,17,4,4,4,4),
                   twt=c(1306.743864,270.71974,325.1945823,18.64802305,
                         436.9521289,188.4361874,157.6588566,81.2310472,
                         120.6657629,194.1304121,45.71067112,12.83364195,
                         13.53191397,13.76769242,15.62439983))

ELres <- calcPB(ELdf,age.c="age",num.c="num",twt.c="twt",
                area=ELha,lbl="Escanaba Lake Example")
ELres$df
ELres$B  # 27.00323
ELres$P  #  6.07475
# Matches Rypel's Excel sheet exactly
