#' Central limit approximation function for uniform distribution
#'
#' @param n The sample size
#' @param iter The number of iterations
#' @param a The lower limit for the uniform distribution
#' @param b The upper limit for the uniform distribution
#'
#' @return Returns a histogram of the distribution of the sum of uniforms
#' @export
#'
#' @examples w=myclt(n=10,iter=10000)
mycltdangroeneveld=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
