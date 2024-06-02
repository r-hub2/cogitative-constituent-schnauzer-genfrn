#' A generating function of triangular fuzzy number via uniform distribution
#'
#' @description A generating function of triangular fuzzy number via uniform distribution based on Souliotis et al. (2022). See detail in references.
#'
#'
#' @param n total number of random triangular fuzzy number
#' @param a the left point of triangular fuzzy number
#' @param b the right point of triangular fuzzy number
#'
#' @return  A data frame with two variables, that is, x and mf
#' @export
#'
#' @importFrom stats runif punif
#' @importFrom graphics plot
#'
#' @references Souliotis, G., Alanazi, Y., & Papadopoulos, B. (2022). Construction of fuzzy numbers via cumulative distribution function. Mathematics, 10(18), 3350. https://doi.org/10.3390/math10183350
#'
#' @examples
#' df <- rtfn(500,1,5)
#' head(df)
#' plot(df) # or plot(df,type='h')
rtfn <- function(n,a,b){
  x <- runif(n,min=a,max=b)
  m <- (a+b)/2 #mu=(a+b)/2
  mf <- c()
  for(j in 1:n){
    if(x[j]<=m){
      mf[j] <- 2*punif(x[j],min=a,max=b)
    }
    else if(x[j]>=m){
      mf[j] <- 2*(1-punif(x[j],min=a,max=b))
    }
  }
  dfout<- data.frame(x=x,mf=mf)
  return(dfout)
}
