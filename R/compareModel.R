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
  
  # 応答変数が離散値か連続値か判定する
  # 応答変数が連続値の場合はcontinuity.flagにTRUEが入る
  continuity.flag <- TRUE
  for (i in 1:length(y)){
    if(round(y[i]) != y[i]) {
      continuity.flag <- TRUE
      break
    }else{
      continuity.flag <- FALSE
    }
  }
  
  # 応答変数に負の値があるか判定する
  # positive.flagがTRUEの時、被説明変数は全て正の数
  positive.flag <- FALSE
  for (i in 1:length(y)){
    if(y[i] > 0) {
      positive.flag <- TRUE
    }else{
      positive.flag <- FALSE
      break
    }
  }
  
  # モデル構築
  
  # 応答変数の性質によるモデル構築
  if(continuity.flag && positive.flag){
    cat("応答変数を連続値で正の値と判断しました\n")
    
    # ガンマ分布
    e.gamma <- try(model.gamma <- glm(y ~ x1, family = Gamma))
    if(class(e.gamma) != "try-error"){
      cat("Gamma model's AIC:")
      print(AIC(model.gamma))
    }
    
    # 正規分布
    e.gaussian <- try(model.gaussian <- glm(y ~ x1, family = gaussian))
    if(class(e.gaussian) != "try-error"){
      cat("Gaussian model's AIC:")
      print(AIC(model.gaussian))
    }
    
  }else if(continuity.flag && !positive.flag){
    cat("応答変数を連続値と判断しました")
    
    # 正規分布
    e.gaussian <- try(model.gaussian <- glm(y ~ x1, family = gaussian))
    if(class(e.gaussian) != "try-error"){
      cat("Gaussian model's AIC:")
      print(AIC(model.gaussian))
    }
    
  }else if(!continuity.flag){
    cat("応答変数を離散値と判断しました")
    
    # ロジスティックモデル
    e.logistic <- try(model.logistic <- glm(y ~ x1, binomial))
    if(class(e.logistic) != "try-error"){
      cat("Logistic model's AIC:")
      print(AIC(model.logistic))
    }
    
    # ポアソン回帰モデル
    e.poisson <- try(model.poisson <- glm(y ~ x1, family = poisson))
    if(class(e.poisson) != "try-error"){
      cat("Poisson regression model's AIC:")
      print(AIC(model.poisson))
    }
  }
  
  # 単回帰
  e.lm <- try(model.lm <- gam(y ~ x1))
  if(class(e.lm) != "try-error"){
    cat("Regression model's AIC:")
    print(AIC(model.lm))
  }
  
  # 平滑化スプライン
  e.gam <- try(model.gam <- gam(y ~ s(x1)))
  if(class(e.gam) != "try-error"){
    cat("Smoothing spline model's AIC:")
    print(AIC(model.gam))
  }
  
  
  # 累乗モデル
  resid <- function(par){
    yhat <- par[1] * x1 ^ par[2]
    sum((y-yhat)^2)	# 残差平方和
  }
  e.power <- try(model.power <- nls(
    y ~ a * x1 ^ b, 
    start = list(a = optim(c(1, 1), resid)$par[1],
                 b = optim(c(1, 1), resid)$par[2])
    )
  )
  if(class(e.power) != "try-error"){
    cat("Power model's AIC:")
    print(AIC(model.power))
  }

  
  # 指数モデル
  resid <- function(par){
    yhat <- par[1] * par[2] ^ x1
    sum((y-yhat)^2)	# 残差平方和
  }
  e.index <- try(model.index <- nls(
    y ~ a * b ^ x1, 
    start = list(a = optim(c(1, 1), resid)$par[1],
                 b = optim(c(1, 1), resid)$par[2])
    )
  )
  if(class(e.index) != "try-error"){
    cat("Exponential model's AIC:")
    print(AIC(model.index))
  }

  # 漸近指数モデル
  e.asymptotic <- try(model.asymptotic <- nls(y ~ SSasymp(x1, Asymp, RO, lrc)))
  if(class(e.asymptotic) != "try-error"){
    cat("Asymptotic exponential model's AIC:")
    print(AIC(model.asymptotic))
  }
  
  # ゴンペルツ成長モデル
  e.gompertz <- try(model.gompertz <- nls(y ~ SSgompertz(x1, Asym, b2, b3)))
  if(class(e.gompertz) != "try-error"){
    cat("Gompertz curve model's AIC:")
    print(AIC(model.gompertz))
  }

  # 遅れS字曲線
  resid <- function(par){
    yhat <- par[1] * (1 - (1 + par[2] * x1) * exp(-par[2] * x1))
    sum((y-yhat)^2)	# 残差平方和
  }
  e.delay <- try(model.delay <- nls(
    y ~ a * (1 - (1 + b * x1) * exp(-b * x1)), 
    start = list(a = optim(c(1, 1), resid)$par[1],
                 b = optim(c(1, 1), resid)$par[2])
    )
  )
  if(class(e.delay) != "try-error"){
    cat("Delay S-shaped curve model's AIC:")
    print(AIC(model.delay))
  }
  
  # 対数近似
  resid <- function(par){
    yhat <- par[1] * log(x1) + par[2]
    sum((y-yhat)^2)	# 残差平方和
  }
  e.logarithm <- try(model.logarithm <- nls(
    y ~ a * log(x1) + b, 
    start = list(a = optim(c(1, 25), resid)$par[1],
                 b = optim(c(1, 25), resid)$par[2])
    )
  )
  if(class(e.logarithm) != "try-error"){
    cat("Logarithmic approximation model's AIC:")
    print(AIC(model.logarithm))
  }

  # warningを戻す
  # options(warn = 1)
  # eroorを戻す
  # options(show.error.messages = TRUE)
}