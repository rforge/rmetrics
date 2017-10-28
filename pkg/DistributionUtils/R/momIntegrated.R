momIntegrated <- function(densFn = "ghyp", param = NULL,
                          order,  about = 0, absolute = FALSE, ...)
{
  if (!is.character(densFn) && !is.function(densFn))
    stop("'densFn' must be supplied as a function or name")

  ## Set default integration limits
  low <- -Inf
  high <- Inf

  if (is.character(densFn)) {
    if (densFn == "ghyp" || densFn == "hyperb" || densFn == "gig" || densFn == "vg") {
      if (!exists(paste0("d", densFn), mode = "function"))
        stop("Relevant package must be loaded for densFn ", densFn)
    }
    if (densFn == "invgamma" || densFn == "inverse gamma") {
      stopifnot(is.numeric( shape <- list(...)$shape ))
      if(shape <= order)
        stop("Order must be less than shape parameter for inverse gamma")
      low <- 0
      dinvgamma <- function(x, shape, rate = 1, scale = 1/rate) {
        ## repl. ifelse(x <= 0, 0, (scale/x)^shape*exp(-scale/x)/(x*gamma(shape)))
        r <- numeric(length(x)) # all 0 --> result
        pos <- x > 0
        x <- x[pos]
        r[pos] <- (scale/x)^shape * exp(-scale/x) / (x*gamma(shape))
        r
      }
      if (!absolute) {
        ddist <- function(x, order, about, ...) (x - about)^order*dinvgamma(x, ...)
      } else {
        ddist <- function(x, order, about, ...) abs(x - about)^order*dinvgamma(x, ...)
      }
    } else { ## "arbitrary" densFn
      dfun <- match.fun(paste("d", densFn, sep = ""))
      if (densFn == "gamma"){
        stopifnot(is.numeric( shape <- list(...)$shape ))
        if(order <= -(shape))
          stop("Order must be greater than shape parameter for gamma")
        low <- 0
      }
      if (!absolute) {
        if (is.null(param)){
          ddist <- function(x, order, about, ...) (x - about)^order*dfun(x, ...)
        } else {
          ddist <- function(x, order, about, param)
            (x - about)^order*dfun(x, param = param)
        }
      } else {
        if (is.null(param)){
          ddist <- function(x, order, about, ...) abs(x - about)^order*dfun(x, ...)
        } else {
          ddist <- function(x, order, about, param)
              abs(x - about)^order*dfun(x, param = param)
        }
      }
    }
  }
  ## Return mom := integrate( * )[["value"]] :
  if (is.null(param)){
    integrate(ddist, low, high,
              order = order, about = about,
              subdivisions = 1000,
              rel.tol = .Machine$double.eps^0.5, ...)[[1]]
  } else {
    integrate(ddist, low, high, param = param,
              order = order, about = about,
              subdivisions = 1000,
              rel.tol = .Machine$double.eps^0.5)[[1]]
  }
}
