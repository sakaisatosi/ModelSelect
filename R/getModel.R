#' Get good model
#' 
#' @param y objective variable
#' @param x1 first explanatory variable
#' @param x2 second explanatory variable
#' 
#' @return good model
#'
#' @export
getLmModel <- function(y, x1, x2) {
  model.single <- lm(y ~ x1)
  model.multi <- lm(y ~ x1 + x2)
  
  if (AIC(model.single) > AIC(model.multi)){
    AIC(model.multi)
  } else {
    AIC(model.single)
  }
}