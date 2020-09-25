#' My (Dan Groeneveld) Sample Distribution of 10 Equallly Likely Outcomes
#'
#' @param n = number of data points
#' @param iter = number of iterations
#' @param time = time interval between plotting iterations
#'
#' @return a barplot showing the relative likelihoods of each outcome
#' @export
#'
#' @examples
#' mysample(n=1000, iter=30, time = 1)
dansample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
