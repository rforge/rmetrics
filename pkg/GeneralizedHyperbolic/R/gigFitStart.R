### Function to find start values using the Method of Moments
### David Cusack, 4/6/2010

gigFitStart <- function(x, startMethodMoM = "Nelder-Mead", ...) {

  fun1 <- function(expParam) {
    diff1 <- mean(x) - gigMean(param = expParam)
    diff1
  }
  fun2 <- function(expParam) {
    diff2 <- var(x) - gigVar(param = expParam)
    diff2
  }
  fun3 <- function(expParam) {
    diff3 <- skewness(x) - gigSkew(param = expParam)
    diff3
  }
  fun4 <- function(expParam) {
    diff4 <- kurtosis(x) - gigKurt(param = expParam)
    diff4
  }

  MoMOptimFunc <- function(param) {
    expParam <- c(exp(param[1]), exp(param[2]), param[3])
    return((fun1(expParam))^2 + (fun2(expParam))^2 +
      (fun3(expParam))^2 + (fun4(expParam))^2)
  }

  startValuesMoM <- c(0, 0, 1)

  MoMOptim <- optim(startValuesMoM, MoMOptimFunc,
                    method = startMethodMoM, ...)

  paramStartMoM <- c(exp(MoMOptim$par[1]), exp(MoMOptim$par[2]),
                     MoMOptim$par[3])
  paramStartMoM
}
