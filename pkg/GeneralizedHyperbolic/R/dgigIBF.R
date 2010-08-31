### Cumulative distribution function of the generalized inverse Gaussian
### Uses incomplete Bessel function of Slevinsky and Safouhi
###
### DJS 9/8/2010
pgigIBF <- function(q, chi = 1, psi = 1, lambda = 1,
                    param = c(chi,psi,lambda), log.p = FALSE,
                    lower.tail = TRUE,
                    ibfTol = .Machine$double.eps^(0.85),
                    nmax = 100, ...) {

  ## check parameters
  parResult <- gigCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)
  if (log.p){
    stop("log.p = TRUE option not yet implemented")
  }

  param <- as.numeric(param)
  chi <- param[1]
  psi <- param[2]
  lambda <- param[3]
  qCalculate <- which((q > 0) & (is.finite(q)))
  prob <- rep(NA, length(q))
  ## probabilities are of upper tail: change later if needs be
  prob[q <= 0] <- 1
  prob[q == Inf] <- 0
  omega <- sqrt(chi * psi)
  KOmega <- besselK(omega, nu = lambda)
  x <- psi*q/2
  y <- chi/(2*q)
  const <- (psi/chi)^(lambda/2)/(2*KOmega)

  for (i in qCalculate){
    prob[i] <- q[i]^lambda*incompleteBesselK(x[i], y[i], -lambda,
                                             tol = ibfTol, nmax = nmax)
  }
  prob[qCalculate] <- const*prob[qCalculate]

  if (lower.tail) prob <- 1 - prob

  return(prob)
} ## End of pgigIBF()

### qgigIBF using pgigIBF based on incomplete Bessel function
### David Scott 09/08/2010
qgigIBF <- function(p, chi = 1, psi = 1, lambda = 1,
                 param = c(chi, psi, lambda),  log.p = FALSE,
                 lower.tail = TRUE, method = c("spline", "integrate"),
                 nInterpol = 501, uniTol = 10^(-7),
                 subdivisions = 100, ibfTol = 10^(-7),
                 nmax = 90, ...){

  ## check parameters
  parResult <- gigCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)
  if (log.p){
    stop("log.p option not yet implemented")
  }

  method <- match.arg(method)
  param <- as.numeric(param)
  chi <- param[1]
  psi <- param[2]
  lambda <- param[3]
  modeDist <- gigMode(param = param)
  pModeDist<- pgigIBF(modeDist, param = param, ibfTol = ibfTol)
  xRange <- gigCalcRange(param = param, tol = 10^(-7))

  quant <- rep(NA, length(p))
  invalid <- which((p < 0) | (p > 1))
  pFinite <- which((p > 0) & (p < 1))


  if (method == "integrate")
  {
    less <- which((p <= pModeDist) & (p > .Machine$double.eps^5))
    quant <- ifelse(p <= .Machine$double.eps^5, 0, quant)
    if (length(less) > 0){
      ## pLow <- min(p[less])
      ## xLow <- modeDist - sqrt(gigVar(param = param))
      ## while (pgigIBF(xLow, param = param, ibfTol = 10^(-5)) >= pLow){
      ##   xLow <- xLow - sqrt(gigVar(param = param))
      ## }
      xLow <- 0
      xRange <- c(xLow, modeDist)
      zeroFn <- function(x, param, p)
      {
        return(pgigIBF(x, param = param, ibfTol = ibfTol) - p)
      }
      for (i in less){
        quant[i] <- uniroot(zeroFn, param = param, p = p[i],
                            interval = xRange, tol = uniTol)$root
      }
    }

    greater <- which ((p > pModeDist) & (p < (1 - .Machine$double.eps^5)))
    p[greater] <- 1 - p[greater]
    quant <- ifelse(p >= (1 - .Machine$double.eps), Inf, quant)
    if (length(greater) > 0){
      pHigh <- min(p[greater])
      xHigh <- modeDist + sqrt(gigVar(param = param))
      while (pgigIBF(xHigh, param = param, lower.tail = FALSE) >= pHigh){
        xHigh <- xHigh + sqrt(gigVar(param = param))
      }
      xRange <- c(modeDist,xHigh)
      zeroFn <- function(x, param, p)
      {
        return(pgigIBF(x, param = param, lower.tail = FALSE,
                     ibfTol = ibfTol) - p)
      }
      for (i in greater){
        quant[i] <- uniroot(zeroFn, param = param, p = p[i],
                            interval = xRange, tol = uniTol)$root
      }
    }
  } else if (method == "spline") {
    inRange <- which((p > pgigIBF(xRange[1], param = param)) &
                     (p < pgigIBF(xRange[2], param = param)))
    small <- which((p <= pgigIBF(xRange[1], param = param)) & (p >= 0))
    large <- which((p >= pgigIBF(xRange[2], param = param)) & (p <= 1))
    extreme <- c(small, large)
    xVals <- seq(xRange[1], xRange[2], length.out = nInterpol)
    yVals <- pgigIBF(xVals, param = param, subdivisions = subdivisions,
                   ibfTol = max(ibfTol, .Machine$double.eps^0.25) )
    splineFit <- splinefun(xVals, yVals)
    zeroFn <- function(x, p){
      return(splineFit(x) - p)
    }

    for (i in inRange){
      quant[i] <- uniroot(zeroFn, p = p[i],
                          interval = xRange, tol = uniTol)$root
    }

    if (length(extreme) > 0){
      quant[extreme] <- qgigIBF(p[extreme], param = param,
                             lower.tail = lower.tail, log.p = log.p,
                             method = "integrate",
                             nInterpol = nInterpol, uniTol = uniTol,
                             subdivisions = subdivisions,
                             ibfTol = ibfTol, ...)
    }
  }
  return(quant)
}
