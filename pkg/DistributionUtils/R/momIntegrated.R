momIntegrated <- function(densFn = "ghyp",
                          order,  about = 0, absolute = FALSE, ...) {

  if (missing(densFn) | !(is.function(densFn) | is.character(densFn)))
    stop("'densFn' must be supplied as a function or name")

  ## Set default integration limits
  low <- -Inf
  high <- Inf

  if (is.character(densFn)) {

    if (is.null(densFn))
      stop("unsupported distribution")
    if (densFn == "ghyp" | densFn == "hyperb" |
        densFn == "gig" | densFn == "vg")
    {
        if (!exists(paste("d",densFn,sep = ""), mode = "function"))
            stop("Relevant package must be loaded")
    }



    if (densFn == "invgamma" | densFn == "inverse gamma"){
        l <- list(...)
        if(l$shape <= order)
            stop("Order must be less than shape parameter for inverse gamma")
        low <- 0
        dinvgamma <- function(x, shape, rate = 1, scale = 1/rate) {
        dens <- ifelse(x <= 0, 0,
                       (scale / x)^shape * exp(-scale / x) / (x * gamma(shape)))
        return(dens)
      }

      if (!absolute) {
        ddist <- function(x, order, about, ...) {
          (x - about)^order * dinvgamma(x, ...)
        }
      } else {
        ddist <- function(x, order, about, ...) {
          abs(x - about)^order * dinvgamma(x, ...)
        }
      }
    } else {
        dfun <- match.fun(paste("d", densFn, sep = ""))
        if (densFn == "gamma"){
            l <- list(...)
            if(order <= -(l$shape))
                stop("Order must be greater than shape parameter for gamma")
            low <- 0
        }
        if (!absolute) {
            ddist <- function(x, order, about, ...) {
                (x - about)^order * dfun(x, ...)
            }
        } else {
            ddist <- function(x, order, about, ...) {
                abs(x - about)^order * dfun(x, ...)
            }
        }
    }
}
  mom <- integrate(ddist, low, high,
                   order = order, about = about,
                   subdivisions = 1000,
                   rel.tol = .Machine$double.eps^0.5, ...)[[1]]

  ## Return Value:
  return(mom)
}
