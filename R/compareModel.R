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
  # options(warn = -1)
  # eroorを消す
  # options(show.error.messages = FALSE)
  
  # モデルを構築する
  # 単回帰
  e.lm <- try(model.lm <- gam(y ~ x1))
  # 平滑化スプライン
  e.gam <- try(model.gam <- gam(y ~ s(x1)))
  # 累乗モデル
  e.power <- try(model.power <- nls(y ~ a * x1 ^ b, start = list(a = 1, b = 1)))
  # 指数モデル
  e.index <- try(model.index <- nls(y ~ a * b ^ x1, start = list(a = 1, b = 1)))
  # 漸近指数モデル
  e.asymptotic <- try(model.asymptotic <- nls(y ~ SSasymp(x1, Asymp, RO, lrc)))
  # ゴンペルツ成長モデル
  e.gompertz <- try(model.gompertz <- nls(y ~ SSgompertz(x1, Asym, b2, b3)))
  # ロジスティックモデル
  e.logistic <- try(model.logistic <- glm(y ~ x1, binomial))
  # ポアソン回帰モデル
  e.poisson <- try(model.poisson <- glm(y ~ x1, family = poisson))
  # 遅れS字曲線
  e.delay <- try(model.delay <- nls(y ~ a * (1 - (1 + b * x1) * exp(-b * x1)), start = list(a = 1, b = 1)))
  # 対数近似
  e.logarithm <- try(model.logarithm <- nls(y ~ a * log(x) + b, start = c(a = -15, b= 25)))
  
  # AICを出力する
  if(class(e.lm) != "try-error"){
    cat("Regression model's AIC:")
    print(AIC(model.lm))
  }
  if(class(e.gam) != "try-error"){
    cat("Smoothing spline model's AIC:")
    print(AIC(model.gam))
  }
  if(class(e.power) != "try-error"){
    cat("Power model's AIC:")
    print(AIC(model.power))
  }
  if(class(e.index) != "try-error"){
    cat("Exponential model's AIC:")
    print(AIC(model.index))
  }
  if(class(e.asymptotic) != "try-error"){
    cat("Asymptotic exponential model's AIC:")
    print(AIC(model.asymptotic))
  }
  if(class(e.gompertz) != "try-error"){
    cat("Gompertz curve model's AIC:")
    print(AIC(model.gompertz))
  }
  if(class(e.logistic) != "try-error"){
    cat("Logistic model's AIC:")
    print(AIC(model.logistic))
  }
  if(class(e.poisson) != "try-error"){
    cat("Poisson regression model's AIC:")
    print(AIC(model.poisson))
  }
  if(class(e.delay) != "try-error"){
    cat("Delay S-shaped curve model's AIC:")
    print(AIC(model.delay))
  }
  if(class(e.logarithm) != "try-error"){
    cat("Logarithmic approximation model's AIC:")
    print(AIC(model.logarithm))
  }
  
  # warningを戻す
  # options(warn = 1)
  # eroorを戻す
  # options(show.error.messages = TRUE)
}