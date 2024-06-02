#' A generating function of trapezoidal fuzzy number via uniform distribution
#'
#' @description A generating function of trapezoidal fuzzy number via uniform distribution based on Souliotis et al. (2022). See detail in references.
#'
#' @param n    total number of random trapezoidal fuzzy number
#' @param a    the left point of trapezoidal fuzzy number
#' @param c    the left-middle point of trapezoidal fuzzy number
#' @param r    the right-middle point of trapezoidal fuzzy number
#' @param b    the right point of trapezoidal fuzzy number
#'
#' @return     A data frame with two variables, that is, x and mf
#' @export
#'
#' @importFrom stats runif punif
#' @importFrom graphics plot
#'
#' @references Souliotis, G., Alanazi, Y., & Papadopoulos, B. (2022). Construction of fuzzy numbers via cumulative distribution function. Mathematics, 10(18), 3350. https://doi.org/10.3390/math10183350
#'
#' @note In general, \code{a}< \code{c}< \code{r} < \code{b} for generating a trapezoidal fuzzy random number.
#' If \code{a}=\code{c}=\code{r}=\code{b}, it will produce a crisp number.
#' If \code{a}<\code{c}=\code{r}<\code{b}, it will produce a triangular fuzzy random number.
#'
#' @examples
#' df2<- rtrfn(500,1,3,4,6)
#' head(df2)
#' plot(df2)  # or plot(df2,type='h')
rtrfn <- function(n,a,c,r,b){
  x <- runif(n,min=a,max=b)
  lpoint <- a; lmid <- c; rmid <- r; rpoint <- b
  mf <- c()
  for(j in 1:n){
    if(x[j]>=lpoint & x[j]<=lmid){
      mf[j] <- punif(x[j],min=a,max=b)/punif(lmid,min=a,max=b)
    }else if(x[j]>=lmid & x[j]<=rmid){
      mf[j] <- 1
    }else if(x[j]>=rmid & x[j]<=rpoint){
      mf[j] <- (1-punif(x[j],min=a,max=b))/(1-punif(rmid,min=a,max=b))
    }
  }
  dfout2<- data.frame(x=x,mf=mf)
  return(dfout2)
}
