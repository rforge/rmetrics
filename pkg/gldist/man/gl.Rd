\name{gldist}

\alias{gldist}
\alias{dgl}
\alias{pgl}
\alias{qgl}
\alias{qdgl}
\alias{rgl}

\title{

  An Asymmetry-Steepness Parameterization of the Generalized
  Lambda Distribution.

}

\description{

  Density, distribution function, quantile function, quantile density
  function and random generation for the generalized Tukey Lambda
  distribution in asymmetry-steepness parametrization. It has location
  equal to the 'median', scale equal to the inter-quartile range and tow
  shape parameters 'chi' and 'xi'.

}

\usage{

dgl(x, med = 0, iqr = 1, chi = 0, xi = 0.6, maxit = 1000L)
pgl(q, med = 0, iqr = 1, chi = 0, xi = 0.6, maxit = 1000L)
qgl(p, med = 0, iqr = 1, chi = 0, xi = 0.6)
qdgl(p, med = 0, iqr = 1, chi = 0, xi = 0.6)
rgl(n, med = 0, iqr = 1, chi = 0, xi = 0.6)

}

\arguments{
  \item{x,q}{
    vector of quantiles.
  }
  \item{p}{
    vector of probabilities.
  }
  \item{n}{
    number of observations. If 'length(n) > 1', the length is taken to
    be the number required.
  }
  \item{med}{
    location parameter (median). If 'length(med) == 4', the vector
    is taken as a description of all four paramters of the distribution.
  }
  \item{iqr}{
    scale parameter (inter-quartile range).
  }
  \item{chi}{
    asymmetry parameter with range -1 < chi < 1.
  }
  \item{xi}{
    steepness parameter with range 0 < xi < 1.
  }
  \item{maxit}{
    maximum number of iteration when calculation inverse of quantile
    function.
  }
}

\value{

  'dgl' gives the density, 'pgl' gives the distribution function, 'qgl'
  gives the quantile function, 'qdgl' gives the quantile density
  function, and 'rgl' generates random deviates.

}

\source{

  Y. Chalabi, D. J. Scott and D. Wuertz, \emph{An Asymmetry-Steepness
  Parameterization of the Generalized Lambda Distribution}. Working
  paper, 2012.

}

\author{

  Yohan Chalabi and Diethelm Wuertz

}

\seealso{

  fitgl, gldist-package

}

\keyword{distribution}
