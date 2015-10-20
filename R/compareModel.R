#' model comparison
#' 
#' @param y objective variable
#' @param x1 first explanatory variable
#' 
#' 
#'
#' @export
compareModel <- function(y, x1) {
  # warningを消す
  options(warn=-1)
  e.lm <- try(model.lm <- gam(y ~ x1))
  e.gam <- try(model.gam <- gam(y ~ s(x1)))
  e.power <- try(model.power <- nls(y ~ a*x1^b, start=list(a=1,b=1)), silent = TRUE)
  # AICを出力する
  if(class(e.lm) != "try-error"){
    cat("Regression AIC:")
    print(AIC(model.lm))
  }
  if(class(e.gam) != "try-error"){
    cat("Smoothing Spline AIC:")
    print(AIC(model.gam))
  }
  if(class(e.power) != "try-error"){
    cat("Power model AIC:")
    print(AIC(model.power))
  }
}