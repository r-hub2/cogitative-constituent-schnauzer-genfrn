#' Alpha-cut of trapezoidal fuzzy number
#'
#' This function for calculating alpha-cut of trapezoidal fuzzy number. See detail in references.
#'
#' @param left       the left point of trapezoidal fuzzy number
#' @param lmid       the left-middle point of trapezoidal fuzzy number
#' @param rmid       the right-middle point of trapezoidal fuzzy number
#' @param right      the right point of trapezoidal fuzzy number
#' @param acut.level the alpha-cut level of trapezoidal fuzzy number
#'
#' @return     \code{AL} is alpha-cut level,
#'             \code{XL.AL} is a lower alpha-cut point of trapezoidal fuzzy number,
#'         and \code{XU.AL} is a upper alpha-cut point of trapezoidal fuzzy number.
#'
#' @export
#'
#'@references  Klir, G.J., Yuan, B., & H., S.C.U. (1997). Fuzzy set theory: Foundations and applications. Prentice Hall PTR.
#'
#' @examples
#' acuttrfn(1,2,3,5,acut.level = c(0,0.5,1))
#' acuttrfn(1,2.5,3.5,6,acut.level = c(0.00,0.25,0.50,0.75,1.00))
acuttrfn <- function(left,lmid,rmid,right,acut.level=seq(0,1,by=0.1)){
  XL<- left; XLM <- lmid; XRM <- rmid; XU <- right; A.level <- acut.level
  acutX.lower <-XL+A.level*(XLM-XL)
  acutX.upper <-XU-A.level*(XU-XRM)
  outps <- list(left=XL,lmid=XLM,rmid=XRM,right=XU,AL=A.level,
                XL.AL=acutX.lower,XU.AL=acutX.upper)
  dfouts <- data.frame(AL=outps$AL,XL.AL=outps$XL.AL,XU.AL=outps$XU.AL)
  return(dfouts)
}
