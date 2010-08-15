### Function to calculate the theoretical mean of a
### generalized hyperbolic distribution given its parameters.
ghypMean <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                     param = c(mu, delta, alpha, beta, lambda)) {

  param <- as.numeric(param)

  if (length(param) == 4)
    param <- c(param, 1)

  ## check parameters
  parResult <- ghypCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  mu <- param[1]
  delta <- param[2]
  alpha <- param[3]
  beta <- param[4]
  lambda <- param[5]

  gamma <- sqrt(alpha^2 - beta^2)

  mu + delta*beta*besselRatio(delta*gamma, lambda, 1)/gamma
} ## End of ghypMean()

### Function to calculate the theoretical variance of a
### generalized hyperbolic distribution given its parameters.
ghypVar <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                    param = c(mu, delta, alpha, beta, lambda)) {

  param <- as.numeric(param)

  if (length(param) == 4)
    param <- c(param, 1)

  ## check parameters
  parResult <- ghypCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  var <- ghypMom(2, param = param, momType = "central")
  return(var)
} ## End of ghypVar()

### Function to calculate the theoretical skewness of a
### generalized hyperbolic distribution given its parameters.
ghypSkew <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                     param = c(mu, delta, alpha, beta, lambda)) {

  param <- as.numeric(param)

  if (length(param) == 4)
    param <- c(param, 1)

  ## check parameters
  parResult <- ghypCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  skew <- ghypMom(3, param = param, momType = "central")/
                 (ghypVar(param = param)^(3/2))
  return(skew)
} ## End of ghypSkew()

### Function to calculate the theoretical kurtosis of a
### generalized hyperbolic distribution given its parameters.
ghypKurt <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                     param = c(mu, delta, alpha, beta, lambda)) {

  param <- as.numeric(param)

  if (length(param) == 4)
    param <- c(param, 1)

  ## check parameters
  parResult <- ghypCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  kurt <- ghypMom(4, param = param, momType = "central")/
                 (ghypVar(param = param)^2) - 3
  return(kurt)
} ## End of ghypKurt()

### Function to calculate the theoretical mode point of a
### generalized hyperbolic distribution given its parameters.
ghypMode <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                     param = c(mu, delta, alpha, beta, lambda)) {

  param <- as.numeric(param)

  if (length(param) == 4)
    param <- c(param, 1)

  ## check parameters
  parResult <- ghypCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  modeFun <- function(x) {
    log(dghyp(x, param = param))
  }

  start <- ghypMean(param = param)
  optResult <- optim(start, modeFun,
                     control = list(fnscale = -1, maxit = 1000),
                     method = "BFGS")

  mode <- ifelse(optResult$convergence == 0, optResult$par, NA)
  mode
} ## End of ghypMode()

