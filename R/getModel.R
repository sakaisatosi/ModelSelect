#' Get good model
#' 
#' @param y objective variable
#' @param x1 first explanatory variable
#' @param x2 second explanatory variable
#' 
#' @return good model
#'
#' @export
CompareModel <- function(y, x1, x2) {
  model.single <- lm(y ~ x1)
  model.multi <- lm(y ~ x1 + x2)
  # AICの小さい方を返す
  if (AIC(model.single) > AIC(model.multi)){
    cat("Single Regression\n AIC: \n")
    print(AIC(model.multi))
    cat("\nMulti Regression\n AIC: \n")
    print(AIC(model.single))
    cat("\nSingle regression is better")
  } else {
    cat("Single Regression\n AIC: \n")
    print(AIC(model.multi))
    cat("\nMulti Regression\n AIC: \n")
    print(AIC(model.single))
    cat("\nMulti regression is better")
  }
}