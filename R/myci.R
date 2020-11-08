#' MyCI
#'
#' @param d the data
#' @param n the number of data points
#' @param a alpha
#'
#' @return a confidence interval for the population mean
#' @export
#'
#' @examples myci(d=c(1,2,3), n=3, a =0.05) <-95%CI
myci<-function(d, n, a){
  t=qt(1-a/2,n-1)
  ci=c()
  ci[1]=mean(d)-t*sd(d)/sqrt(n)
  ci[2]=mean(d)+t*sd(d)/sqrt(n)
  ci


}
