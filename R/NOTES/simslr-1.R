myslrsim = function(x,alpha=0.05,iter=10,b0=10,b1=-5,sigma=12)
{
  library(s20x) # Library with cool functions
  n = length(x) # sample size
  yy=b0+b1*x # keep yy for future use
  y=yy # y to be overwritten
  #layout(matrix(c(1,1,2,3),nr=2,nc=2, byrow = TRUE), heights = c(2,1))
  plot(x, y, ylim=range(c(0,y)),xlim = range(c(0,x))) # set 0 in range
  abline(a=b0,b=b1, lwd=2,col = "green3") # Line to be estimated
  Sys.sleep(2) # sleep and see
  maty = matrix(NA, nr=n, nc=iter)
  matci=matrix(NA,nr=3,nc=iter)
  matbeta = matrix(NA,nr=3, nc=iter)
  for(i in 1:iter)
  {
    y = b0+b1*x + rnorm(n,0,sigma)
    maty[,i] = y
    y.lm = lm(y~x)
    abline(y.lm, col=rgb(0.4,0,i/iter,0.3))
    matci[,i] = predict(y.lm,data.frame(x=median(x)),interval="confidence")
    lmout = summary(y.lm)
    matbeta[,i] = c(lmout$coefficients[1:2],lmout$sigma )
  }
  text(1/4*mean(x), 3/4*max(yy), expression(hat(beta)[1]==frac(SS[XY],SS[XX])))
  text(1/4*mean(x), 1/4*max(yy), expression(hat(beta)[0]==bar(Y)-frac(SS[XY],SS[XX])*bar(X)))
  windows()
  hist(matbeta[1,], main = expression(paste("Distribution of ",hat(beta)[0]),sep=""), xlab = expression(hat(beta)[0]),col = "Red")
  windows()
  hist(matbeta[2,], main = expression(paste("Distribution of ",hat(beta)[1])),xlab = expression(hat(beta)[1]), col = "Blue")
  #matci
}
windows(); myslrsim(x=-15:15, b0=6, b1=10,iter=10000,alpha=0.02,sigma=100)
