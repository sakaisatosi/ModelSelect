#' model comparison
#' 
#' @param y objective variable
#' @param x1 first explanatory variable
#' 
#' 
#'
#' @export
CompareSplineModel <- function(y, x1) {
  model.lm <- gam(y ~ x1)
  model.gam <- gam(y ~ s(x1))
  # AICの小さい方を返す
  if (AIC(model.lm) > AIC(model.gam)){
    cat("Regression\n AIC: \n")
    print(AIC(model.gam))
    cat("\nSmoothing Spline\n AIC: \n")
    print(AIC(model.lm))
    cat("\nRegression is better")
  } else {
    cat("Regression\n AIC: \n")
    print(AIC(model.lm))
    cat("\nSmoothing Spline\n AIC: \n")
    print(AIC(model.gam))
    cat("\nSmoothing Spline is better")
  }
}