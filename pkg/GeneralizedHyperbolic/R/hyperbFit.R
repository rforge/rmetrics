### Function to fit hyperbolic distribution to data
###
### DJS 11/09/06
hyperbFit <- function(x, freq = NULL, breaks = NULL, paramStart = NULL,
                      startMethod = "Nelder-Mead", startValues = "BN",
                      method = "Nelder-Mead", hessian = FALSE,
                      plots = FALSE, printOut = FALSE,
                      controlBFGS = list(maxit = 200),
                      controlNM = list(maxit = 1000), maxitNLM = 1500, ...) {

  xName <- paste(deparse(substitute(x), 500), collapse = "\n")

  if (!is.null(freq)) {
    if (length(freq) != length(x))
      stop("vectors x and freq are not of the same length")

    x <- rep(x, freq)
  }

  x <- as.numeric(na.omit(x))
  startInfo <- hyperbFitStart(x, breaks = breaks,
                              startValues = startValues,
                              paramStart = paramStart,
                              startMethodSL = startMethod,
                              startMethodMoM = startMethod, ...)
  paramStart <- startInfo$paramStart
  svName <- startInfo$svName
  breaks <- startInfo$breaks
  empDens <- startInfo$empDens
  midpoints <- startInfo$midpoints

  llfunc <- function(param) {
    # This function used to expect (pi, zeta) values.
    # As a result the old code will be executed.
    # This also has a habit of breaking due to incorrect delta
    # values in the new form.

    mu <- param[1]
    delta <- param[2]
    alpha <- param[3]
    beta <- param[4]

    hyperbPi <- beta / sqrt(alpha^2 - beta^2)
    zeta <- delta * sqrt(alpha^2 - beta^2)

    KNu <- besselK(zeta, nu = 1)

    hyperbDens <- (2 * delta * sqrt(1 + hyperbPi^2) * KNu)^(-1) *
                  exp(-zeta * (sqrt(1 + hyperbPi^2) * sqrt(1 + ((x - mu) /
                  delta)^2) - hyperbPi * (x - mu) / delta))
    as.numeric(hyperbDens)
    -sum(log(hyperbDens))
  }

  output <- numeric(7)
  ind <- 1:4

  if (method == "BFGS") {
    opOut <- optim(paramStart, llfunc, NULL, method = "BFGS",
                   control = controlBFGS, ...)
  }

  if (method == "Nelder-Mead") {
    opOut <- optim(paramStart, llfunc, NULL, method = "Nelder-Mead",
                   control = controlNM, ...)
  }

  if (method == "nlm") {
    ind <- c(2, 1, 5, 4)
    opOut <- nlm(llfunc, paramStart, iterlim = maxitNLM, ...)
  }

  param <- as.numeric(opOut[[ind[1]]])[1:4]       # parameter values
  names(param) <- c("mu", "delta", "alpha", "beta")
  maxLik <- -(as.numeric(opOut[[ind[2]]]))        # maximum likelihood
  conv <- as.numeric(opOut[[ind[4]]])             # convergence
  iter <- as.numeric(opOut[[ind[3]]])[1]          # iterations

  # If there is a hessian to be computed, compute it using the likelihood
  # at the estimated parameters, this code was borrowed from the fBasics
  # package, see utils-hessian.R

  if (hessian) {
    n <- length(param)
    lloutput <- llfunc(param)
    eps <- .Machine$double.eps

    # Compute the stepsize:
    h = eps^(1/3) *
        apply(as.data.frame(param), 1, function(z) max(abs(z), 1.0e-2))
    ee = diag(h) # Matrix(diag(h), sparse = TRUE)

    # Compute forward and backward steps:
    gp = vector(mode = "numeric", length = n)
    gm = vector(mode = "numeric", length = n)

    for (i in 1:n)
      gp[i] <- llfunc(param + ee[, i])

    for (i in 1:n)
      gm[i] <- llfunc(param - ee[, i])

    H = h %*% t(h)
    Hm = H
    Hp = H

    # Compute double forward and backward steps:
    for (i in 1:n) {
      for (j in  i:n) {
        Hp[i, j] <- llfunc(param + ee[, i] + ee[, j])
        Hp[j, i] <- Hp[i, j]
        Hm[i, j] <- llfunc(param - ee[, i] - ee[, j])
        Hm[j, i] <- Hm[i, j]
      }
    }

    # Compute the Hessian:
    for (i in 1:n) {
      for (j in  i:n) {
        H[i, j] = ((Hp[i, j] - gp[i] - gp[j] + lloutput + lloutput -
                  gm[i] - gm[j] + Hm[i, j]) / H[i, j]) / 2
        H[j, i] = H[i, j]
      }
    }

    colnames(H) <- names(param)
    rownames(H) <- names(param)

    opOut$hessian <- H
  }

  fitResults <- list(param = param, maxLik = maxLik,
                     hessian = if (hessian) opOut$hessian else NULL,
                     method = method, conv = conv, iter = iter,
                     obs = x, obsName = xName, paramStart = paramStart,
                     svName = svName, startValues = startValues,
                     breaks = breaks, midpoints = midpoints,
                     empDens = empDens)

  class(fitResults) <- "hyperbFit"

  if (printOut)
    print(fitResults, ...)

  if (plots)
    plot.hyperbFit(fitResults, ...)

  fitResults
} ## End of hyperbFit()


### Function to print object of class hyperbFit
### DJS 11/08/06
print.hyperbFit <- function(x,
                            digits = max(3, getOption("digits") - 3), ...) {

  if (!class(x) == "hyperbFit")
    stop("Object must belong to class hyperbFit")

  cat("\nData:     ", x$obsName, "\n")
  cat("Parameter estimates:\n")
  print.default(format(x$param, digits = digits),
                print.gap = 2, quote = FALSE)
  cat("Likelihood:        ", x$maxLik, "\n")
  cat("Method:            ", x$method, "\n")
  cat("Convergence code:  ", x$conv, "\n")
  cat("Iterations:        ", x$iter, "\n")
  invisible(x)
} ## End of print.hyperbFit

### Function to plot results of fitting a hyperbolic distribution
plot.hyperbFit <- function(x, which = 1:4,
                           plotTitles = paste(c("Histogram of ",
                                                "Log-Histogram of ",
                                                "Q-Q Plot of ",
                                                "P-P Plot of "),
                                              x$obsName, sep = ""),
                           ask = prod(par("mfcol")) < length(which) &
                                 dev.interactive(), ...) {

  if (class(x) != "hyperbFit")
    stop("Object must belong to class hyperbFit")

  if (ask) {
    op <- par(ask = TRUE)
    on.exit(par(op))
  }

  par(mar = c(6, 4, 4, 2) + 0.1)
  show <- rep(FALSE, 4)
  show[which] <- TRUE
  param <- x$param
  breaks <- x$breaks
  empDens <- x$empDens
  mipoints <- x$midpoints
  obs <- x$obs
  obsName <- x$obsName

  hypDens <- function(x)
    dhyperb(x, param = param)

  logHypDens <- function(x)
    log(dhyperb(x, param = param))

  ymax <- 1.06 * max(hypDens(seq(min(breaks), max(breaks), 0.1)),
                     empDens, na.rm = TRUE)
  if (show[1]) {
    hist.default(obs, breaks, right = FALSE, freq = FALSE, ylim = c(0, ymax),
                 main = plotTitles[1], ...)
    curve(hypDens, min(breaks) - 1, max(breaks) + 1, add = TRUE, ylab = NULL)
    title(sub = paste("param = (",
          round(param[1], 3), ", ", round(param[2], 3), ", ",
          round(param[3], 3), ", ", round(param[4], 3), ")", sep = ""))
  }

  if (show[2]) {
    logHist(obs, breaks, include.lowest = TRUE, right = FALSE,
            main = plotTitles[2], ...)
    curve(logHypDens, min(breaks) - 1, max(breaks) + 1, add = TRUE,
          ylab = NULL, xlab = NULL)
    title(sub = paste("param = (",
          round(param[1], 3), ", ", round(param[2], 3), ", ",
          round(param[3], 3), ", ", round(param[4], 3), ")", sep = ""))
  }

  if (show[3])
    qqhyperb(obs, param = param, main = plotTitles[3], ...)

  if (show[4])
    pphyperb(obs, param = param, main = plotTitles[4], ...)

  invisible()
}
