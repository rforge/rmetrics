### Function to fit the Generalized Inverse Gaussian distribution to data

gigFit <- function(x, startValues = c("GammaIG", "MoM", "US"),
                   paramStart = NULL,
                   method = c("Nelder-Mead", "nlm"),
                   plots = FALSE, printOut = FALSE,
                   controlNM = list(maxit = 1000),
                   maxitNLM = 1500, ...) {

  startValues <- match.arg(startValues)
  method <- match.arg(method)
  xName <- paste(deparse(substitute(x), 500), collapse = "\n")


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
    paramStart <- gigFitStart(x, startMethodMoM = "Nelder-Mead", ...)
  }


  if (startValues == "GammaIG") {
    lg <- try(fitdistr(x, "gamma")$loglik, silent = TRUE)
    lig <- try(fitdistr(1/x, "gamma")$loglik, silent = TRUE)

    c <- class(lg)
    d <- class(lig)

    if (c == "try-error") {
      if (d == "try-error") {
        stop("GammaIG method does not work for this data set")
      }   else g <- fitdistr(1/x, "gamma")$estimate
      paramStart <- c(2*g[1], 0.1, -g[2])
    } else if (d == "try-error") {
      if (c == "try-error") {
        stop("GammaIG method does not work for this data set")
      }   else g <- fitdistr(x, "gamma")$estimate
      paramStart <- c(0.1, 2*g[1], g[2])
    } else if (lg > lig) {
      g <- fitdistr(x, "gamma")$estimate
      paramStart <- c(0.1, 2*g[1], g[2])
    } else if (lg < lig) {
      g <- fitdistr(1/x, "gamma")$estimate
      paramStart <- c(2*g[1], 0.1, -g[2])
    }
  }



  ## Change paramStart into the log scale

  paramStart <- c(log(paramStart[1]), log(paramStart[2]), paramStart[3])

  ## Creating the Log Likelihood function

  llfunc <- function(param) {
    loggigDens <- param[3]/2*log(exp(param[2])/exp(param[1])) -
      log(2*besselK(sqrt(exp(param[1])*exp(param[2])), nu = param[3])) +
        (param[3] - 1)*log(x) - 1/2*(exp(param[1])*x^-1 + exp(param[2])*x)
    as.numeric(loggigDens)
    return(-sum(loggigDens))
  }

  ind <- 1:4

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
  conv <- as.numeric(opOut[[ind[4]]])
  iter <- as.numeric(opOut[[ind[3]]])[1]

  fitResults <- list(param = param,
                     maxLik = maxLik,
                     method = method,
                     conv = conv,
                     iter = iter,
                     obs = x,
                     obsName = xName,
                     paramStart = paramStart,
                     startValues = startValues)
  class(fitResults) <- c("gigFit", "distFit")
  if (printOut)
    print(fitResults, ...)
  if (plots)
    plot.gigFit(fitResults, ...)
  return(fitResults)
}





### Function to print an object of class gigFit
print.gigFit <- function(x, digits = max(3, getOption("digits") - 3), ...)
{

  if (! "gigFit" %in% class(x))
    stop("Object must belong to class gigFit")

  cat("\nData:     ", x$obsName, "\n")
  cat("Parameter estimates:\n")
  print.default(format(x$param, digits = digits),
                print.gap = 2, quote = FALSE)
  cat("Likelihood:        ", x$maxLik, "\n")
  cat("Method:            ", x$method, "\n")
  cat("Convergence code:  ", x$conv, "\n")
  cat("Iterations:        ", x$iter, "\n")
  invisible(x)
}






### Function to plot results of fitting a GIG distribution
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
    hist.default(obs, right = FALSE, freq = FALSE,
                 main = plotTitles[1], ...)
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








### Function to find start values using the Method of Moments
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
        (fun1(expParam))^2 + (fun2(expParam))^2 +
          (fun3(expParam))^2 + (fun4(expParam))^2
    }



    startValuesMoM <- c(0, 0, 1)

    MoMOptim <- optim(startValuesMoM, MoMOptimFunc,
                      method = startMethodMoM, ...)

    paramStartMoM <- c(exp(MoMOptim$par[1]), exp(MoMOptim$par[2]),
                       MoMOptim$par[3])
    if (paramStartMoM[1] < 0.1)
        paramStartMoM <- c(0.1, paramStartMoM[2], paramStartMoM[3])

    if (paramStartMoM[2] < 0.1)
        paramStartMoM <- c(paramStartMoM[1], 0.1, paramStartMoM[3])
    paramStartMoM
}

coef.gigFit <- function(object, ...) {
  object$param
}

vcov.gigFit <- function(object, ...) {
  obs <- object$obs
  param <- object$param
  hessian <- gigHessian(obs, param, hessianMethod= "tsHessian")
  varcov <- solve(hessian)
  varcov
}


gigHessian <- function(x, param, hessianMethod = c("tsHessian", "exact")) {

    if (hessianMethod == "exact") {
        stop("The exact hessian formula is not available yet. Please use method tsHessian instead.")
    }

    if (hessianMethod == "tsHessian") {

        llfuncH <- function(param) {
            loggigDens <- param[3]/2*log(exp(param[2])/exp(param[1])) -
              log(2*besselK(sqrt(exp(param[1])*exp(param[2])),
                            nu = param[3])) + (param[3] - 1)*log(x) -
                              1/2*(exp(param[1])*x^-1 + exp(param[2])*x)

            as.numeric(loggigDens)
            return(sum(loggigDens))
        }
    }

    hessian <- tsHessian(param = param, fun = llfuncH)
    return(hessian)
}



