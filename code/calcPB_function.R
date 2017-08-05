#' Calculate production and biomass with instantaneous growth method
#' 
#' Calculate production and biomass with instantaneous growth method when given a data.frame that contains abundance and total biomass for each age-class.
#' 
#' @param df A data.frame that contains variables that identify age-classes and abundance and total biomass for each age-class.
#' @param age.c A single numeric or string for the column in \code{df} that contains the age-classes.
#' @param num.c A single numeric or string for the column in \code{df} that contains the abundances for each age-classe.
#' @param age.c A single numeric or string for the column in \code{df} that contains the biomass for each age-class.
#' @param adjAgeGaps A logical that tells whether to adjust for missing age-classes (e.g, if some age-classes represent more than one age-class; see the Escanaba Lake example). When \code{TRUE} the biomass is divided by number of years between ages to put in a per year basis.
#' @param area The area sampled. This is used to express B and P on a per area basis.
#' 
#' @return A list with the following items
#' \describe{
#'   \item{df}{The original data.frame with mean biomass (\code{mB}), mean weight (\code{mwt}), instantaneous growth rate (\code{G}), and production (\code{P}) columns appended.}
#'   \item{B}{Total biomass for the sampled area (not on a per area basis).}
#'   \item{P}{Total production for the sampled area (not on a per area basis).}
#'   \item{BperA}{Total biomass on a per area basis.}
#'   \item{BperA}{Total biomass on a per area basis.}
#'   \item{PperA}{Total prodouction on a per area per year basis.}
#'   \item{Area}{Area given by the user in \code{area}.}
#' }
#' 
#' @examples 
#' 
calcPB <- function(df,age.c=1,num.c=2,twt.c=3,area=1,adjAgeGaps=TRUE) {
  # Some checks
  if (!(is.data.frame(df) | is.matrix(df)))
    stop("'df' must be data.frame or matrix.",call.=FALSE)
  if (is.character(age.c)) age.c <- which(names(df)==age.c)
  if (is.character(num.c)) num.c <- which(names(df)==num.c)
  if (is.character(twt.c)) twt.c <- which(names(df)==twt.c)
  if (length(age.c)<1 | length(num.c)<1 | length(twt.c)<1)
    stop("One of 'age.c', 'num.c', or 'twt.c' is not in 'df'.")
  if (age.c<1 | num.c<1 | twt.c<1)
    stop("One of 'age.c', 'num.c', or 'twt.c' is not a proper column number.")
  if (area<0) stop("'area' must be positive.")
  # Rename columns to match expectations, save old column names
  onames <- names(df)[c(age.c,num.c,twt.c)]
  names(df)[c(age.c,num.c,twt.c)] <- c("age","num","twt")
  # Adjust for gaps in ages
  if (adjAgeGaps) div <- c(NA,diff(df$age))
  else div <- c(NA,rep(1,length(df$age)))
  # Add mean biomass (B-bar; div is used to adjust for ages that
  #   represent multiple ages)
  df$mB <- zoo::rollmean(df$twt,k=2,na.pad=TRUE,align="right")/div
  # Add mean weight (w-bar)
  df$mwt <- df$twt/df$num
  # Compute instantaneous growth rate (G)
  df$G <- c(NA,diff(log(df$mwt)))
  # Compute age-specific production
  df$P <- df$mB*df$G
  # Put original column names back on data.frame
  names(df)[which(names(df) %in% c("age","num","twt"))] <- onames
  # Create return list
  res <- list(df=as.data.frame(df),
              B=sum(df$twt,na.rm=TRUE),P=sum(df$P,na.rm=TRUE))
  res <- c(res,BperA=res$B/area,PperA=res$P/area,area=area)
  class(res) <- c("data.frame","PB")
  res
}
