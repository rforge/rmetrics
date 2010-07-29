### Function to fit the Generalized Inverse Gaussian distribution to data
### David Cusack, 4/6/2010

gigFit <- function(x, freq = NULL, startValues = c("MoM", "US"),
                   paramStart = NULL,
                   startMethod = c("Nelder-Mead","BFGS"),
                   method = c("Nelder-Mead", "nlm"),
                   plots = FALSE, printOut = FALSE,
                   controlNM = list(maxit = 1000),
                   maxitNLM = 1500, ...) {

  startValues <- match.arg(startValues)
  startMethod <- match.arg(startMethod)
  method <- match.arg(method)
  xName <- paste(deparse(substitute(x), 500), collapse = "\n")

  if (!is.null(freq)) {
    if (length(freq) != length(x))
      stop("vectors x and freq are not of the same length")

    x <- rep(x, freq)
  }
  if (startValues == "US")  {
    if (is.null(paramStart))
      stop("paramStart must be specified")

    if (!is.null(paramStart)) {
      if (length(paramStart) != 3)
        stop("paramStart must contain 3 values")
      if (paramStart[1] < 0)
        stop("chi must be greater than 0")
      if (paramStart[2] < 0)
        stop("psi must be greater than 0")
    }
  }


  if (startValues == "MoM") {
    paramStart <- gigFitStart(x, startMethodMoM = startMethod, ...)
  }

  ## Change paramStart into the log scale
  paramStart <- c(log(paramStart[1]), log(paramStart[2]), paramStart[3])

  ## Creating the Log Likelihood function

  llfunc <- function(param) {
    loggigDens <- param[3]/2*log(exp(param[2])/exp(param[1])) -
      log(2*besselK(sqrt(exp(param[1])*exp(param[2])), nu=param[3])) +
      (param[3] - 1)*log(x) - 1/2*(exp(param[1])*x^-1 + exp(param[2])*x)
    as.numeric(loggigDens)
    return(-sum(loggigDens))
  }

  ind <- 1:2

  if (method == "Nelder-Mead") {
    opOut <- optim(paramStart, llfunc, NULL, method = "Nelder-Mead",
                   control = controlNM, ...)
  }

  if (method == "nlm") {
    ind <- c(2, 1)
    opOut <- nlm(llfunc, paramStart, iterlim = maxitNLM, ...)
  }

  param <- as.numeric(opOut[[ind[1]]])[1:3]
  param <- c(exp(param[1]), exp(param[2]), param[3])
  names(param) <- c("chi", "psi", "lambda")
  maxLik <- -(as.numeric(opOut[[ind[2]]]))


  fitResults <- list(param = param, maxLik = maxLik, method = method,
                     startValues = startValues, paramStart = paramStart,
                     obs = x, obsName = xName)

  class(fitResults) <- c("gigFit", "distFit")

  if (printOut)
    print(fitResults, ...)

  if (plots)
    plot.gigFit(fitResults, ...)
}




### Function to print an object of class gigFit

print.gigFit <- function(x, digits = max(3, getOption("digits") - 3), ...) {

  if (! "gigFit" %in% class(x))
    stop("Object must belong to class gigFit")

  cat("\nData:     ", x$obsName, "\n")
  cat("Parameter estimates:\n")
  print.default(format(x$param, digits = digits),
                print.gap = 2, quote = FALSE)
  cat("Likelihood:        ", x$maxLik, "\n")
  cat("Method:            ", x$method, "\n")
  invisible(x)
}


### Function to plot results of fitting a
### Generalized inverse Gaussian distribution

plot.gigFit <- function(x, which = 1:4,
                        plotTitles = paste(c("Histogram of ",
                        "Log-Histogram of ",
                        "Q-Q Plot of ",
                        "P-P Plot of "),
                        x$obsName, sep = ""),
                        ask = prod(par("mfcol")) < length(which) &
                        dev.interactive(), ...) {

  if (! "gigFit" %in% class(x))
    stop("Object must belong to class gigFit")

  if (ask) {
    op <- par(ask = TRUE)
    on.exit(par(op))
  }

  par(mar = c(6, 4, 4, 2) + 0.1)
  show <- rep(FALSE, 4)
  show[which] <- TRUE
  param <- x$param
  obs <- x$obs

  if (show[1]) {
    hist.default(obs, right = FALSE, freq = FALSE, main = plotTitles[1], ...)
    curve(dgig(x, param = param), add = TRUE, ylab = NULL)
    title(sub = paste("param = (", round(param[1], 3), ", ",
          round(param[2], 3), ", ", round(param[3], 3), ")", sep = ""))
  }

  if (show[2]) {
    logHist(obs, include.lowest = TRUE, right = FALSE,
            main = plotTitles[2], ...)
    curve(log(dgig(x, param = param)), add = TRUE, ylab = NULL, xlab = NULL)
    title(sub = paste("param = (", round(param[1], 3), ", ",
          round(param[2], 3), ", ", round(param[3], 3), ")", sep = ""))
  }

  if (show[3])
    qqgig(obs, param = param, main = plotTitles[3], ...)

  if (show[4])
    ppgig(obs, param = param, main = plotTitles[4], ...)

  invisible()
}

