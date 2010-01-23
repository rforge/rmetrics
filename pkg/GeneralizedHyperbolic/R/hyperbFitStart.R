hyperbFitStart <- function(x, breaks = NULL,
                           startValues = "BN",
                           paramStart = NULL,
                           startMethodSL = "Nelder-Mead",
                           startMethodMoM = "Nelder-Mead", ...) {

  histData <- hist(x, plot = FALSE, right = FALSE)
  
  if (is.null(breaks))
    breaks <- histData$breaks

  midpoints <- histData$mids
  empDens <- ifelse(!is.finite(log(histData$density)), NA, histData$density)
  maxIndex <- order(empDens, na.last = FALSE)[length(empDens)]

  if (length(na.omit(empDens[1:maxIndex])) > 1) {
    leftAsymptote <- lm(log(empDens)[1:maxIndex] ~ midpoints[1:maxIndex])$coef
    rightAsymptote <- c(NA, -10 * leftAsymptote[2]) # arbitrary large value
  }

  if (length(na.omit(empDens[maxIndex:length(empDens)])) > 1) {
    rightAsymptote <- lm(log(empDens)[maxIndex:length(empDens)] ~
                      midpoints[maxIndex:length(empDens)])$coef

    if (length(na.omit(empDens[1:maxIndex])) < 2)
      leftAsymptote <- c(NA, -10 * rightAsymptote[2]) # arbitrary large value
  }

  if ((length(na.omit(empDens[1:maxIndex])) < 2) &
      (length(na.omit(empDens[maxIndex:length(empDens)])) < 2)) {
    if (startValues == "BN" | startValues == "SL")
      stop("not enough breaks to estimate asymptotes to log-density")
  }

  if (startValues == "US") {
    svName <- "User Specified"

    if (is.null(paramStart))
      stop("paramStart must be specified")

    if (!is.null(paramStart)) {
      if (length(paramStart) != 4)
        stop("paramStart must contain 4 values")

      paramStart <- hyperbChangePars(1, 2, paramStart)

      if (paramStart[4] <= 0)
        stop("zeta in paramStart must be greater than zero")

      if (paramStart[2] <= 0)
        stop("delta in paramStart must be greater than zero")
    }
  }

  if (startValues == "FN") {
    svName <- "Fitted Normal"
    nu <- as.numeric(midpoints[maxIndex])
    mu <- mean(x)
    delta <- sd(x)
    hyperbPi <- (nu - mu) / delta
    zeta <- 1 + hyperbPi^2
    paramStart <- c(mu, delta, hyperbPi, zeta)
  }

  if (startValues == "SL") {
    svName <- "Skew Laplace"

    llsklp <- function(param) {
      -sum(log(dskewlap(x, param = param)))
    }

    lSkewAlpha <- log(1 / leftAsymptote[2])
    lSkewBeta <- log(abs(1 / rightAsymptote[2]))
    skewMu <- midpoints[maxIndex]
    paramStart <- c(skewMu, lSkewAlpha, lSkewBeta)
    skewlpOptim <- optim(paramStart, llsklp, NULL,
                         method = startMethodSL, hessian = FALSE, ...)
    phi <- 1 / exp(skewlpOptim$par[2])
    hyperbGamma <- 1 / exp(skewlpOptim$par[3])
    delta <- 0.1 # Take delta to be small
    mu <- skewlpOptim$par[1]
    hyperbPi <- hyperbChangePars(3, 1, c(mu, delta, phi, hyperbGamma))[3]
    zeta <- hyperbChangePars(3, 1, c(mu, delta, phi, hyperbGamma))[4]
    paramStart <- c(mu, delta, hyperbPi, zeta)
  }

  if (startValues == "MoM") {
    svName <- "Method of Moments"
    paramStart <- hyperbFitStartMoM(x, startMethodMoM = startMethodMoM, ...)
    paramStart <- hyperbChangePars(2, 1, paramStart)
  }

  if (!(startValues %in% c("US", "FN", "SL", "MoM")))
    startValues <- "BN"

  if (startValues=="BN") {
    svName <- "Barndorff-Nielsen 1977"
    phi <- leftAsymptote[2]
    hyperbGamma <- -rightAsymptote[2]

    if (!(is.na(leftAsymptote[1]) | is.na(rightAsymptote[1]))) {
      mu <- -(leftAsymptote[1] - rightAsymptote[1]) /
             (leftAsymptote[2] - rightAsymptote[2])
      intersectionValue <- leftAsymptote[1] + mu * leftAsymptote[2]
      logModalDens <- log(max(empDens, na.rm = TRUE))
      zeta <- intersectionValue - logModalDens

      if (zeta <= 0)
        zeta <- 0.1        # This is set arbitrarily
    } else {
      mu <- median(x)
      intersectionValue <- mu
      logModalDens <- log(max(empDens, na.rm = TRUE))
      zeta <- intersectionValue - logModalDens

      if (zeta <= 0)
        zeta <- 0.1        # This is set arbitrarily
    }

    delta <- zeta / sqrt(phi * hyperbGamma)
    hyperbPi <- hyperbChangePars(3, 1, c(mu, delta, phi, hyperbGamma))[3]
    paramStart <- c(mu, delta, hyperbPi, zeta)
  }

  paramStart <- hyperbChangePars(1, 2, paramStart)
  names(paramStart) <- c("mu", "delta", "alpha", "beta")
  list(paramStart = paramStart, breaks = breaks, midpoints = midpoints,
       empDens = empDens, svName = svName)
} ## End of hyperbFitStart()

hyperbFitStartMoM <- function(x, startMethodMoM = "Nelder-Mead", ...) {

  fun1 <- function(param) {
    diff1 <- hyperbMean(param = param) - mean(x)
    diff1
  }

  fun2 <- function(param) {
    diff2 <- hyperbVar(param = param) - var(x)
    diff2
  }

  fun3 <- function(param) {
    diff3 <- hyperbSkew(param = param) - skewness(x)
    diff3
  }

  fun4 <- function(param) {
    diff4 <- hyperbKurt(param = param) - kurtosis(x)
    diff4
  }

  MoMOptimFun <- function(param) {
    (fun1(param))^2 + (fun2(param))^2 +
    (fun3(param))^2 + (fun4(param))^2
  }

  ## Determine starting values for parameters using
  ## Barndorff-Nielsen et al "The Fascination of Sand" in
  ## A Celebration of Statistics pp.78--79
  xi <- sqrt(kurtosis(x) / 3)
  chi <- skewness(x) / 3  # Ensure 0 <= |chi| < xi < 1

  if (xi >= 1)
    xi <- 0.999

  adjust <- 0.001

  if (abs(chi) > xi) {
    if (xi < 0 ) {
      chi <- xi + adjust
    } else {
      chi <- xi - adjust
    }
  }

  hyperbPi <- chi / sqrt(xi^2 - chi^2)
  zeta <- 3 / xi^2 - 1
  rho <- chi / xi
  delta <- (sqrt(1 + zeta) - 1) * sqrt( 1 - rho^2)
  mu <- mean(x) - delta * hyperbPi * RLambda(zeta, lambda = 1)
  startValuesMoM <- c(mu, delta, hyperbPi, zeta)
  startValuesMoM <- hyperbChangePars(1, 2, startValuesMoM, noNames = TRUE)
  ## Get Method of Moments estimates
  MoMOptim <- optim(startValuesMoM, MoMOptimFun, method = startMethodMoM, ...)
  paramStart <- MoMOptim$par
  paramStart
} ## End of hyperbFitStartMoM
