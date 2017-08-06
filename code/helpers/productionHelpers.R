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
  if (verbose) message("The PE for WBIC_YEAR ",WBIC_YEAR," is ",res,".")
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
  if (verbose) message("The size of WBIC_YEAR ",WBIC," is ",res," ha.")
  # return result
  res
}



#' Identifies appropriate weight-length regression and uses it to predict weight from length.
#' 
#' This function takes a data.frame of one WBIC_YEAR, identifies the appropriate regression, predicts weight from length, and returns those predictions with some other information. See workflow document for how "appropriate" is defined.
#' 
#' @param df A data.frame filtered from the FMDB file for one WBIC_YEAR.
#' @param LWRegs A data.frame of valid weight-length regressions filtered from the LWRegs file.
#'  
doLWReg <- function(df,LWRegs) {
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
  # Get appropriate regression
  reg <- LWRegs[row,]
  # Predict weights from lens with the regression results
  df$wt <- reg$a*df$len.mm^reg$b
  # return data (with new wts) and regression information
  list(df=df,reg=reg)
}



#' Identifies and returns appropriate age-lengthe key.
#' 
#' This function takes a data.frame of one WBIC_YEAR and identifies and returns the appropriate age-length key. The user may choose either to use emprical or smoothed age-length keys. See workflow document for how "appropriate" is defined.
#' 
#' @param df A data.frame filtered from the FMDB file for one WBIC_YEAR.
#' @param ALKInfo A data.frame of valid information about age-length keys filtered from the ALKInfo file.
#' @param type A string that indicates whether to use "empirical" (observed) or "smoothed" (modeled) age=length keys.
#'  
doALK <- function(df,ALKInfo,type) {
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
  # Potentially remove fish that are less than the minimum length on the ALK
  note <- ""
  min.len.on.ALK <- min(as.numeric(rownames(alk)))
  if (min(df$len.mm)<min.len.on.ALK) {
    tmp <- nrow(df)
    df <- filterD(df,len.mm>=min.len.on.ALK)
    tmp <- tmp-nrow(df)
    if (nrow(df)==0) note <- makeNote(note,paste0("Removing ",tmp," fish smaller than minimum length on ALK (",min.len.on.ALK,") resulted in no fish to age."))
    else note <- makeNote(note,paste0("Removed ",tmp," fish smaller than minimum length on ALK (",min.len.on.ALK,")."))
  }
  # Apply that ALK to predict ages from lengths (if n>0)
  if (nrow(df)>0) {
    df <- alkIndivAge(alk,~len.mm,data=df)
    df <- filterD(df,age>=3)
    if (nrow(df)==0) note <- makeNote(note,"All fish were < age-3.")
  }
  # Return data.frame and ALK information
  list(df=df,which=ALKInfo$which[row],type=ALKInfo$type[row],note=note)
}



makeNote <- function(note,addlnote) {
  if (note=="") note <- addlnote
  else note <- paste(note,addlnote)
  note
}