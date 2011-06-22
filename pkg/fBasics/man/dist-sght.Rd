\name{sght}

\alias{sght}

\alias{dsght}
\alias{psght}
\alias{qsght}
\alias{rsght}


\title{Standardized generalized hyperbolic Student-t Distribution}


\description{

  Density, distribution function, quantile function
  and random generation for the standardized generalized
  hyperbolic distribution.

}


\usage{
dsght(x, beta = 0.1, delta = 1, mu = 0, nu = 10, log = FALSE)
psght(q, beta = 0.1, delta = 1, mu = 0, nu = 10)
qsght(p, beta = 0.1, delta = 1, mu = 0, nu = 10)
rsght(n, beta = 0.1, delta = 1, mu = 0, nu = 10)
}


\arguments{


  \item{beta, delta, mu}{
    numeric values.
    \code{beta} is the skewness parameter in the range \code{(0, alpha)};
    \code{delta} is the scale parameter, must be zero or positive;
    \code{mu} is the location parameter, by default 0.
    These are the parameters in the first parameterization.
  }
  \item{nu}{
    a numeric value, the number of degrees of freedom.
    Note, \code{alpha} takes the limit of \code{abs(beta)},
    and \code{lambda=-nu/2}.
  }
  \item{x, q}{
    a numeric vector of quantiles.
  }
  \item{p}{
    a numeric vector of probabilities.
  }
  \item{n}{
    number of observations.
  }
  \item{log}{
    a logical, if TRUE, probabilities \code{p} are given as
    \code{log(p)}.
  }
}

\value{

  All values for the \code{*sght} functions are numeric vectors:
  \code{d*} returns the density,
  \code{p*} returns the distribution function,
  \code{q*} returns the quantile function, and
  \code{r*} generates random deviates.

  All values have attributes named \code{"param"} listing
  the values of the distributional parameters.

}



\author{

  Diethelm Wuertz.

}


\examples{
## rsght -
   set.seed(1953)
   r = rsght(5000, beta = 0.1, delta = 1, mu = 0, nu = 10)
   plot(r, type = "l", col = "steelblue",
     main = "gh: zeta=1 rho=0.5 lambda=1")

## dsght -
   # Plot empirical density and compare with true density:
   hist(r, n = 50, probability = TRUE, border = "white", col = "steelblue")
   x = seq(-5, 5, length = 501)
   lines(x, dsght(x, beta = 0.1, delta = 1, mu = 0, nu = 10))

## psght -
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, psght(x, beta = 0.1, delta = 1, mu = 0, nu = 10))

## qsght -
   # Compute Quantiles:
   round(qsght(psght(seq(-5, 5, 1), beta = 0.1, delta = 1, mu = 0, nu =10),
               beta = 0.1, delta = 1, mu = 0, nu = 10), 4)

}


\keyword{distribution}

