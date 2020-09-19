#' Creates a quadratic model equation to be used in plots
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
myplot=function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
}
