#' Alpha-cut of triangular fuzzy number
#'
#' This function for calculating alpha-cut of triangular fuzzy number. See detail in references.
#'
#' @param left       the left point of triangular fuzzy number
#' @param middle     the middle or mode point of triangular fuzzy number
#' @param right      the right point of triangular fuzzy number
#' @param acut.level the alpha-cut level of triangular fuzzy number
#'
#' @return     \code{AL} is alpha-cut level,
#'             \code{XL.AL} is a lower alpha-cut point of triangular fuzzy number,
#'         and \code{XU.AL} is a upper alpha-cut point of triangular fuzzy number.
#'
#' @export
#'
#'@references  Klir, G.J., Yuan, B., & H., S.C.U. (1997). Fuzzy set theory: Foundations and applications. Prentice Hall PTR.
#'
#' @examples
#' acuttfn(1,2,3,acut.level = c(0,0.5,1))
#' acuttfn(1,2.5,3,acut.level = c(0.00,0.25,0.50,0.75,1.00))
acuttfn <- function(left,middle,right,acut.level=seq(0,1,by=0.1)){
  XL<- left; XM <- middle; XU <- right; A.level <- acut.level
  acutX.lower <-XL+A.level*(XM-XL)
  acutX.upper <-XU-A.level*(XU-XM)
  outps <- list(left=XL,middle=XM,right=XU,AL=A.level,
                XL.AL=acutX.lower,XU.AL=acutX.upper)
  dfouts <- data.frame(AL=outps$AL,XL.AL=outps$XL.AL,XU.AL=outps$XU.AL)
  return(dfouts)
}
