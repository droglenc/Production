#' Extract the appropriate PE.
#' 
#' This function takes a data.frame of population estimates and returns a single PE that corresponds to the given WBIC_YEAR.
#' 
#' @param df A data.frame filtered from the FMDB file for one WBIC_YEAR.
#' @param PE The PE data.frame
#' @param verbose A logical that indicates whether a message should be printed.
#' 
getPE <- function(df,PE,verbose=FALSE) {
  # Find WBIC_YEAR
  WBIC_YEAR <- paste(as.character(unique(df$wbic)),
                     as.character(unique(df$year)),sep="_")
  # Find corresponding PE value
  res <- PE$PE[PE$wbic_year==WBIC_YEAR]
  # Print message if asked to
  message("The PE for WBIC_YEAR ",WBIC_YEAR," is ",res,".")
  # return result
  res
}

#' Extract the size of the lake.
#' 
#' This function takes a data.frame of lake characteristics (i.e., wbicInfo) and returns a single lake size (in has) that corresponds to the given WBIC.
#' 
#' @param df A data.frame filtered from the FMDB file for one WBIC_YEAR.
#' @param WB The wbicInfo ddata.frame
#' @param verbose A logical that indicates whether a message should be printed.
#' 
getSize <- function(df,WB,verbose=FALSE) {
  # Find WBIC
  WBIC <- as.character(unique(df$wbic))
  # Find corresponding lake size
  res <- WB$size[WB$wbic==WBIC]
  # Print message if asked to
  message("The size of WBIC_YEAR ",WBIC," is ",res," ha.")
  # return result
  res
}



#' Identifies and returns appropriate weight-length regression.
#' 
#' This function takes a data.frame of one WBIC_YEAR and identifies and returns the appropriate regression results. See workflow document for how "appropriate" is defined.
#' 
#' @param df A data.frame filtered from the FMDB file for one WBIC_YEAR.
#' @param LWRegs A data.frame of valid weight-length regressions filtered from the LWRegs file.
#'  
getLWReg <- function(df,LWRegs) {
  # Find WBIC,YEAR,WBIC_YEAR, and CLASS
  WBIC <- as.character(unique(df$wbic))
  YEAR <- as.character(unique(df$year))
  WBIC_YEAR <- paste(WBIC,YEAR,sep="_")
  CLASS <- as.character(unique(df$class))
  # Find the regression to use
  if (WBIC_YEAR %in% LWRegs$which) row <- which(LWRegs$which==WBIC_YEAR)
  else if (WBIC %in% LWRegs$which) row <- which(LWRegs$which==WBIC)
  else if (CLASS %in% LWRegs$which) row <- which(LWRegs$which==CLASS)
  else row <- which(LWRegs$which=="ALL")
  # Return appropriate row
  LWRegs[row,]
}



#' Identifies and returns appropriate age-lengthe key.
#' 
#' This function takes a data.frame of one WBIC_YEAR and identifies and returns the appropriate age-length key. The user may choose either to use emprical or smoothed age-length keys. See workflow document for how "appropriate" is defined.
#' 
#' @param df A data.frame filtered from the FMDB file for one WBIC_YEAR.
#' @param ALKInfo A data.frame of valid information about age-length keys filtered from the ALKInfo file.
#' @param type A string that indicates whether to use "empirical" (observed) or "smoothed" (modeled) age=length keys.
#'  
getALK <- function(df,ALKInfo,type) {
  # Find WBIC,YEAR,WBIC_YEAR, and CLASS
  WBIC <- as.character(unique(df$wbic))
  YEAR <- as.character(unique(df$year))
  WBIC_YEAR <- paste(WBIC,YEAR,sep="_")
  CLASS <- as.character(unique(df$class))
  # Find the name of the ALK to use
  if (WBIC_YEAR %in% ALKInfo$which) row <- which(ALKInfo$which==WBIC_YEAR)
  else if (WBIC %in% ALKInfo$which) row <- which(ALKInfo$which==WBIC)
  else if (CLASS %in% ALKInfo$which) row <- which(ALKInfo$which==CLASS)
  else row <- which(ALKInfo$which=="ALL")
  # Find appropriate alk
  dir <- "data/prepped/ALKs/"
  ext <- ".RData"
  nm <- ifelse(type=="empirical",ALKInfo$ename[row],ALKInfo$sname[row])
  load(paste0(dir,nm,ext))
  # Return a list that has the ALK and some other information
  list(ALK=alk,which=ALKInfo$which[row],type=ALKInfo$type[row])
}
