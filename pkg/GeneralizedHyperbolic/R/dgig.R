### Function to calculate the density of the
### generalized inverse Gaussian distribution
dgig <- function(x, chi = 1, psi = 1, lambda = 1,
                 param = c(chi, psi, lambda), KOmega = NULL) {

  ## check parameters
  parResult <- gigCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)
  
  param <- as.numeric(param)
  chi <- param[1]
  psi <- param[2]
  lambda <- param[3]

  omega <- sqrt(chi * psi)

  if (is.null(KOmega))
    KOmega <- besselK(omega, nu = lambda)

  gigDensity <- ifelse(x > 0, (psi / chi)^(lambda / 2) /
                       (2 * KOmega) * x^(lambda - 1) *
                       exp(-(1/2) * (chi * x^(-1) + psi * x)), 0)
  gigDensity
} ## end of dgig()

### Cumulative distribution function of the generalized inverse Gaussian
### New version intended to give guaranteed accuracy
### Uses exponential approximation in tails
### safeIntegrate() is used over six parts in the middle of the distribution
### Calls gigBreaks to determine the breaks
###
### DJS 25/01/07
pgig <- function(q, chi = 1, psi = 1, lambda = 1,
                 param = c(chi, psi, lambda),
                 small = 10^(-6), tiny = 10^(-10),
                 deriv = 0.3, subdivisions = 100,
                 accuracy = FALSE, ...) {

  ## check parameters
  parResult <- gigCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  param <- as.numeric(param)
  chi <- param[1]
  psi <- param[2]
  lambda <- param[3]

  omega <- sqrt(chi * psi)

  ## Standardise distribution to chi = 1
  q <- q / chi
  psi <- chi * psi
  chi <- 1
  param <- c(chi, psi, lambda)
  KOmega <- besselK(omega, nu = lambda)

  bks <- gigBreaks(param = param, small = small, tiny = tiny, deriv = deriv, ...)
  xTiny <- bks$xTiny
  xSmall <- bks$xSmall
  lowBreak <- bks$lowBreak
  highBreak <- bks$highBreak
  xLarge <- bks$xLarge
  xHuge <- bks$xHuge
  modeDist <- bks$modeDist

  qSort <- sort(q)
  qTiny <- which(qSort < xTiny)
  qSmall <- which(qSort < xSmall)
  qLow <- which(qSort < lowBreak)
  qLessEqMode <- which(qSort <= modeDist)
  qGreatMode <- which(qSort > modeDist)
  qHigh <- which(qSort > highBreak)
  qLarge <- which(qSort > xLarge)
  qHuge <- which(qSort > xHuge)

  ## Break indices into 8 groups: beware of empty groups
  if (length(qLow) > 0) qLessEqMode <- qLessEqMode[qLessEqMode > max(qLow)]
  if (length(qHigh) > 0) qGreatMode <- qGreatMode[qGreatMode < min(qHigh)]
  if (length(qSmall) > 0) qLow <- qLow[qLow > max(qSmall)]
  if (length(qLarge) > 0) qHigh <- qHigh[qHigh < min(qLarge)]
  if (length(qTiny) > 0) qSmall <- qSmall[qSmall > max(qTiny)]
  if (length(qHuge) > 0) qLarge <- qLarge[qLarge < min(qHuge)]
  intFun <- rep(NA, length(q))
  if (length(qTiny) > 0) intFun[qTiny] <- 0
  if (length(qHuge) > 0) intFun[qHuge] <- 1
  intErr <- rep(NA, length(q))
  if (length(qTiny) > 0) intErr[qTiny] <- 0
  if (length(qHuge) > 0) intErr[qHuge] <- tiny


  ## Use safeIntegrate function between xTiny and xHuge in 6 sections
  dgigInt <- function(q) {
    dgig(q, param = param)
  }

  ## Calculate integrals and errors to cut points
  resSmall <- safeIntegrate(dgigInt, xTiny, xSmall, subdivisions, ...)
  resLarge <- safeIntegrate(dgigInt, xLarge, xHuge, subdivisions, ...)
  intSmall <- resSmall$value
  intLarge <- resLarge$value
  errSmall <- resSmall$abs.error
  errLarge <- tiny + resLarge$abs.error
  resLow <- safeIntegrate(dgigInt, xSmall, lowBreak, subdivisions, ...)
  resHigh <- safeIntegrate(dgigInt, highBreak, xLarge, subdivisions, ...)
  intLow <- intSmall + resLow$value
  intHigh <- intLarge + resHigh$value
  errLow <- errSmall + resLow$abs.error
  errHigh <- errLarge + resHigh$abs.error

  for (i in qSmall) {
    intRes <- safeIntegrate(dgigInt, xTiny, qSort[i], subdivisions, ...)
    intFun[i] <- intRes$value
    intErr[i] <- intRes$abs.error
  }

  for (i in qLarge) {
    intRes <- safeIntegrate(dgigInt, qSort[i], xHuge, subdivisions, ...)
    intFun[i] <-  1- intRes$value
    intErr[i] <- intRes$abs.error + tiny
  }

  for (i in qLow) {
    intRes <- safeIntegrate(dgigInt, xSmall, qSort[i], subdivisions, ...)
    intFun[i] <- intRes$value + intSmall
    intErr[i] <- intRes$abs.error + errSmall
  }

  for (i in qHigh) {
    intRes <- safeIntegrate(dgigInt, qSort[i], xLarge, subdivisions, ...)
    intFun[i] <-  1- intRes$value - intLarge
    intErr[i] <- intRes$abs.error + errLarge
  }

  for (i in qLessEqMode) {
    intRes <- safeIntegrate(dgigInt, lowBreak, qSort[i], subdivisions, ...)
    intFun[i] <- intRes$value + intLow
    intErr[i] <- intRes$abs.error + errLow
  }

  for (i in qGreatMode) {
    intRes <- safeIntegrate(dgigInt, qSort[i], highBreak, subdivisions, ...)
    intFun[i] <-  1- intRes$value - intHigh
    intErr[i] <- intRes$abs.error + errLarge
  }

  if (!accuracy) {
    return(intFun[rank(q)])
  } else {
    return(list(value=intFun[rank(q)], error=intErr[rank(q)]))
  }
} ## End of pgig()

### qgig using breaks as for pgig and splines
### David Scott 08/12/06
qgig <- function(p, chi = 1, psi = 1, lambda = 1,
                 param = c(chi, psi, lambda),
                 small = 10^(-6), tiny = 10^(-10),
                 deriv = 0.3, nInterpol = 100,
                 subdivisions = 100, ...) {

  ## check parameters
  parResult <- gigCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  param <- as.numeric(param)
  chi <- param[1]
  psi <- param[2]
  lambda <- param[3]

  if (any(p < 0 | p > 1))
    stop("p must lie between 0 and 1")

  bks <- gigBreaks(param = param, small = small, tiny = tiny, deriv = deriv, ...)
  xTiny <- bks$xTiny
  xSmall <- bks$xSmall
  lowBreak <- bks$lowBreak
  highBreak <- bks$highBreak
  xLarge <- bks$xLarge
  xHuge <- bks$xHuge
  modeDist <- bks$modeDist

  yTiny <- pgig(xTiny, param = param)
  ySmall <- pgig(xSmall, param = param)
  yLowBreak <- pgig(lowBreak, param = param)
  yHighBreak <- pgig(highBreak, param = param)
  yLarge <- pgig(xLarge, param = param)
  yHuge <- pgig(xHuge, param = param)
  yModeDist <- pgig(modeDist, param = param)

  pSort <- sort(p)
  pSmall <- which(pSort < pgig(xSmall, param = param))
  pTiny <- which(pSort < pgig(xTiny, param = param))
  pLarge <- which(pSort > pgig(xLarge, param = param))
  pHuge <- which(pSort > pgig(xHuge, param = param))
  pLow <- which(pSort < pgig(lowBreak, param = param))
  pHigh <- which(pSort > pgig(highBreak, param = param))
  pLessEqMode <- which(pSort <= pgig(modeDist, param = param))
  pGreatMode <- which(pSort > pgig(modeDist, param = param))

  ## Break indices into 8 groups: beware of empty groups
  if (length(pLow) > 0) pLessEqMode <- pLessEqMode[pLessEqMode > max(pLow)]
  if (length(pHigh) > 0) pGreatMode <- pGreatMode[pGreatMode < min(pHigh)]
  if (length(pSmall) > 0) pLow <- pLow[pLow > max(pSmall)]
  if (length(pLarge) > 0) pHigh <- pHigh[pHigh < min(pLarge)]
  if (length(pTiny) > 0) pSmall <- pSmall[pSmall > max(pTiny)]
  if (length(pHuge) > 0) pLarge <- pLarge[pLarge < min(pHuge)]
  qSort <- rep(NA, length(pSort))
  if (length(pTiny) > 0) qSort[pTiny] <- -Inf
  if (length(pHuge) > 0) qSort[pHuge] <- Inf

  if (length(pSmall) > 0) {
    xValues <- seq(xTiny, xSmall, length = nInterpol)
    pgigValues <- pgig(xValues, param = param, small = small, tiny = tiny,
                       deriv = deriv, subdivisions = subdivisions, accuracy = FALSE)
    pgigSpline <- splinefun(xValues, pgigValues)

    for (i in pSmall) {
      zeroFun <- function(x) {
        pgigSpline(x) - pSort[i]
      }

      if (zeroFun(xTiny) >= 0) {
        qSort[i] <- xTiny
      } else {
        if (zeroFun(xSmall) <= 0) {
          qSort[i] <- xSmall
        } else {
          qSort[i] <- uniroot(zeroFun, interval = c(xTiny,xSmall), ...)$root
        }
      }
    }
  }

  if (length(pLow) > 0) {
    xValues <- seq(xSmall, lowBreak, length = nInterpol)
    pgigValues <- pgig(xValues, param = param, small = small, tiny = tiny,
                       deriv = deriv, subdivisions = subdivisions, accuracy = FALSE)
    pgigSpline <- splinefun(xValues, pgigValues)

    for (i in pLow) {
      zeroFun <- function(x) {
        pgigSpline(x) - pSort[i]
      }

      if (zeroFun(xSmall) >= 0) {
        qSort[i] <- xSmall
      } else {
        if (zeroFun(lowBreak) <= 0) {
          qSort[i] <- lowBreak
        } else {
          qSort[i] <- uniroot(zeroFun, interval = c(xSmall,lowBreak), ...)$root
        }
      }
    }
  }

  if (length(pLessEqMode) > 0) {
    xValues <- seq(lowBreak, modeDist, length = nInterpol)
    pgigValues <- pgig(xValues, param = param, small = small, tiny = tiny,
                       deriv = deriv, subdivisions = subdivisions, accuracy = FALSE)
    pgigSpline <- splinefun(xValues, pgigValues)

    for (i in pLessEqMode) {
      zeroFun <- function(x) {
        pgigSpline(x) - pSort[i]
      }

      if (zeroFun(lowBreak) >= 0) {
        qSort[i] <- lowBreak
      } else {
        if (zeroFun(modeDist) <= 0) {
          qSort[i] <- modeDist
        } else {
          qSort[i] <- uniroot(zeroFun, interval = c(lowBreak,modeDist), ...)$root
        }
      }
    }
  }

  if (length(pGreatMode) > 0) {
    xValues <- seq(modeDist, highBreak, length = nInterpol)
    pgigValues <- pgig(xValues, param = param, small = small, tiny = tiny,
                       deriv = deriv, subdivisions = subdivisions, accuracy = FALSE)
    pgigSpline <- splinefun(xValues, pgigValues)

    for (i in pGreatMode) {
      zeroFun <- function(x) {
        pgigSpline(x) - pSort[i]
      }

      if (zeroFun(modeDist) >= 0) {
        qSort[i] <- modeDist
      } else {
        if (zeroFun(highBreak) <= 0) {
          qSort[i] <- highBreak
        } else {
          qSort[i] <- uniroot(zeroFun, interval = c(modeDist,highBreak), ...)$root
        }
      }
    }
  }

  if (length(pHigh) > 0) {
    xValues <- seq(highBreak, xLarge, length = nInterpol)
    pgigValues <- pgig(xValues, param = param, small = small, tiny = tiny,
                       deriv = deriv, subdivisions = subdivisions, accuracy = FALSE)
    pgigSpline <- splinefun(xValues, pgigValues)

    for (i in pHigh) {
      zeroFun <- function(x) {
        pgigSpline(x) - pSort[i]
      }

      if (zeroFun(highBreak) >= 0) {
        qSort[i] <- highBreak
      } else {
        if (zeroFun(xLarge) <= 0) {
          qSort[i] <- xLarge
        } else {
          qSort[i] <- uniroot(zeroFun, interval = c(highBreak,xLarge), ...)$root
        }
      }
    }
  }

  if (length(pLarge) > 0) {
    xValues <- seq(xLarge, xHuge, length = nInterpol)
    pgigValues <- pgig(xValues, param = param, small = small, tiny = tiny,
                       deriv = deriv, subdivisions = subdivisions, accuracy = FALSE)
    pgigSpline <- splinefun(xValues, pgigValues)

    for (i in pLarge) {
      zeroFun <- function(x) {
        pgigSpline(x) - pSort[i]
      }

      if (zeroFun(xLarge) >= 0) {
        qSort[i] <- xLarge
      } else {
        if (zeroFun(xHuge) <= 0) {
          qSort[i] <- xHuge
        } else {
          qSort[i] <- uniroot(zeroFun, interval = c(xLarge,xHuge), ...)$root
        }
      }
    }
  }

  if (length(pHuge) > 0) {
    for (i in pHuge) {
      zeroFun <- function(x) {
        pgig(x, param = param) - pSort[i]
      }

      interval <- c(xHuge,xHuge + (xHuge - xLarge))

      while (zeroFun(interval[1]) * zeroFun(interval[2]) > 0) {
        interval[1] <- interval[1] + (xHuge - xLarge)
      }

      qSort[i] <- uniroot(zeroFun,interval)$root
    }
  }

  return(qSort[rank(p)])
} ## End of qgig()

# Modified version of rgig to generate random observations
# from a generalized inverse Gaussian distribution in the
# special case where lambda = 1.
rgig1 <- function(n, chi = 1, psi = 1, param = c(chi, psi)) {

  if (length(param) == 2)
    param <- c(param, 1)

  ## check parameters
  parResult <- gigCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  chi <- param[1]
  psi <- param[2]
  lambda <- 1

  alpha <- sqrt(psi / chi)
  beta <- sqrt(psi * chi)

  m <- abs(beta) / beta

  g <- function(y) {
    0.5 * beta * y^3 - y^2 * (0.5 * beta * m + lambda + 1) +
      y * (-0.5 * beta) + 0.5 * beta * m
  }

  upper <- m

  while (g(upper) <= 0)
    upper <- 2 * upper

  yM <- uniroot(g, interval = c(0, m))$root

  yP <- uniroot(g, interval = c(m, upper))$root

  a <- (yP - m) * exp(-0.25 * beta * (yP + 1 / yP - m - 1 / m))
  b <- (yM - m) * exp(-0.25 * beta * (yM + 1 / yM - m - 1 / m))
  c <- -0.25 * beta * (m + 1 / m)

  output <- numeric(n)

  for (i in 1:n) {
    needValue <- TRUE

    while (needValue) {
      R1 <- runif(1)
      R2 <- runif(1)
      Y <- m + a * R2 / R1 + b * (1 - R2) / R1
      if (Y > 0) {
        if (-log(R1) >= 0.25 * beta * (Y + 1 / Y) + c) {
          needValue <- FALSE
        }
      }
    }

    output[i] <- Y
  }

  output / alpha
} ## End of rgig1

# Function to generate random observations from a
# generalized inverse Gaussian distribution. The
# algorithm is based on that given by Dagpunar (1989)
rgig <- function(n, chi = 1, psi = 1, lambda = 1,
                 param = c(chi, psi, lambda)) {

  ## check parameters
  parResult <- gigCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  chi <- param[1]
  psi <- param[2]
  lambda <- param[3]

  if (lambda == 1)
    stop(return(rgig1(n, param = c(chi, psi))))

  alpha <- sqrt(psi / chi)
  beta <- sqrt(psi * chi)

  m <- (lambda - 1 + sqrt((lambda - 1)^2 + beta^2)) / beta

  g <- function(y) {
    0.5 * beta * y^3 - y^2 * (0.5 * beta * m + lambda + 1) +
      y * ((lambda - 1) * m - 0.5 * beta) + 0.5 * beta * m
  }

  upper <- m

  while (g(upper) <= 0)
    upper <- 2 * upper

  ## yM <- uniroot(g, interval = c(0, m))$root
  ## Correct problem when psi and chi are very small
  ## Code from Fabian Scheipl
  ## Fabian.Scheipl@stat.uni-muenchen.de
  yM <- uniroot(g, interval = c(0, m),
                tol = min(.Machine$double.eps^0.25,
                          (.Machine$double.eps + g(0) / 10)))$root

  yP <- uniroot(g, interval = c(m, upper))$root

  a <- (yP - m) * (yP / m)^(0.5 * (lambda - 1)) *
         exp(-0.25 * beta * (yP + 1 / yP - m - 1 / m))
  b <- (yM - m) * (yM / m)^(0.5 * (lambda - 1)) *
         exp(-0.25 * beta * (yM + 1 / yM - m - 1 / m))
  c <- -0.25 * beta * (m + 1 / m) + 0.5 * (lambda - 1) * log(m)

  output <- numeric(n)

  for (i in 1:n) {
    needValue <- TRUE

    while (needValue) {
      R1 <- runif(1)
      R2 <- runif(1)
      Y <- m + a * R2 / R1 + b * (1 - R2) / R1
      if (Y > 0) {
        if (-log(R1) >= -0.5 * (lambda - 1) * log(Y) + 0.25 * beta * (Y + 1 / Y) + c) {
          needValue <- FALSE
        }
      }
    }
    output[i] <- Y
  }
  output / alpha
} ## End of rgig()

### Derivative of dgig
ddgig <- function(x, chi = 1, psi = 1, lambda = 1,
                  param = c(chi, psi, lambda), KOmega = NULL, ...) {

  ## check parameters
  parResult <- gigCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  param <- as.numeric(param)
  chi <- param[1]
  psi <- param[2]
  lambda <- param[3]

  omega <- sqrt(chi * psi)

  if (is.null(KOmega))
    KOmega <- besselK(x = omega, nu = lambda)

  ddgig <- ifelse(x > 0,
                  dgig(x, param = param, KOmega) * (chi / x^2 + 2 * (lambda - 1) / x - psi) / 2,
                  0)
  ddgig
} ## End of ddgig()

### Function to set up breaks for pgig and qgig
gigBreaks <- function(chi = 1, psi = 1, lambda = 1,
                      param = c(chi, psi, lambda), small = 10^(-6),
                      tiny = 10^(-10), deriv = 0.3, ...) {

  ## check parameters
  parResult <- gigCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  param <- as.numeric(param)
  chi <- param[1]
  psi <- param[2]
  lambda <- param[3]

  omega <- sqrt(chi * psi)
  ## Find quantiles of standardised gig: adjust later
  psi <- chi * psi
  chi <- 1
  ThetaStand <- c(chi, psi, lambda)
  KOmega <- besselK(x = omega, nu = lambda)
  const <- (psi / chi)^(lambda / 2) / (2 * KOmega)

  xTiny <- 0
  xSmall <- gigCalcRange(param = ThetaStand, tol = small, density = TRUE)[1]
  xLarge <- gigCalcRange(param = ThetaStand, tol = small, density = TRUE)[2]
  xHuge <- gigCalcRange(param = ThetaStand, tol = tiny, density = TRUE)[2]

  modeDist <- gigMode(param = ThetaStand)
  ## Determine break points, based on size of derivative
  xDeriv <- seq(xSmall, modeDist, length.out = 101)
  derivVals <- ddgig(xDeriv, param = ThetaStand, KOmega = KOmega)
  maxDeriv <- max(derivVals)
  minDeriv <- min(derivVals)
  breakSize <- deriv * maxDeriv

  breakFun <- function(x) {
    ddgig(x, param = ThetaStand, KOmega = KOmega) - breakSize
  }

  if ((maxDeriv < breakSize) | (derivVals[1] > breakSize)) {
    lowBreak <- xSmall
  } else {
    whichMaxDeriv <- which.max(derivVals)
    lowBreak <- uniroot(breakFun, c(xSmall, xDeriv[whichMaxDeriv]))$root
  }

  xDeriv <- seq(modeDist, xLarge, length.out = 101)
  derivVals <- -ddgig(xDeriv, param = ThetaStand, KOmega = KOmega)
  maxDeriv <- max(derivVals)
  minDeriv <- min(derivVals)
  breakSize <- deriv * maxDeriv

  breakFun <- function(x) {
    -ddgig(x, param = ThetaStand, KOmega = KOmega) - breakSize
  }

  if ((maxDeriv < breakSize) | (derivVals[101] > breakSize)) {
    highBreak <- xLarge
  } else {
    whichMaxDeriv <- which.max(derivVals)
    highBreak <- uniroot(breakFun, c(xDeriv[whichMaxDeriv], xLarge))$root
  }

  # param[1] is the original value of chi
  breaks <- param[1] * c(xTiny, xSmall, lowBreak, highBreak, xLarge, xHuge, modeDist)
  breaks <- list(xTiny = breaks[1], xSmall = breaks[2],
                 lowBreak = breaks[3], highBreak = breaks[4],
                 xLarge = breaks[5], xHuge = breaks[6],
                 modeDist = breaks[7])
  return(breaks)
} ## End of gigBreaks()
