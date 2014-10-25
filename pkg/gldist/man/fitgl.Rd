\name{fitgl}

\alias{fitgl}

\title{

  Fitting of the Generalized Lambda Distribution.

}

\description{

  Fitting of the Generalized Lambda Distribution with different
  estimators.

}

\usage{

fitgl(x, start, inc = FALSE, na.rm = FALSE, method = c("mle", "hist",
"prob", "quant", "shape"), \dots)

}

\arguments{
  \item{x}{
    A numeric vector of length at least one containing only finite
    values.
  }
  \item{start}{
    A numeric vector (optional), initial values for the parameters to be
  optimized.
  }
  \item{inc}{
    A logical value indicating wether the location (median) and scale
    (inter-quartile) should be included in the optimization or estimated
    by their smaple estimators. FALSE by default.
  }
  \item{method}{
    A character value specifying the method to use to fit the data.
  }
  \item{na.rm}{
    logical.  Should missing values (including 'NaN') be removed?
  }
  \item{\dots}{
    additional arguments to be passed to the optimization function
    'nlminb' or to the objective function. See 'method' argument for the
    availalbe additional arguments of the objective function.
  }
}

\details{

  FIXME

}

\value{

  A list with components.

  \item{call}{Calling function.}
  \item{par}{The best set of parameters found.}
  \item{objective}{The value of \code{objective} corresponding to
    \code{par}.}
  \item{convergence}{An integer code. \code{0} indicates successful
    convergence.}
  \item{message}{A character string giving any additional information returned
    by the optimizer, or \code{NULL}. For details, see \code{nlminb}
    documentation.}
  \item{iterations}{Number of iterations performed.}
  \item{evaluations}{Number of objective function and gradient function
    evaluations.}
}

\references{

  Y. Chalabi, D. J. Scott and D. Wuertz, \emph{An Asymmetry-Steepness
  Parameterization of the Generalized Lambda Distribution}. Working
  paper, 2012.

}

\author{

  Yohan Chalabi and Diethelm Wuertz

}

\seealso{
  gldist, gldist-package
}

\examples{

# Generate deviates
x <- rgl(1000, med = 1, iqr = 2, chi = 0, xi = .5)

# Fit the data set with the different methods
fitgl(x, inc = FALSE, method = "mle")
fitgl(x, inc = TRUE, method = "mle")

fitgl(x, inc = FALSE, method = "hist", breaks = "FD")
fitgl(x, inc = TRUE, method = "hist", breaks = "FD")

fitgl(x, inc = FALSE, method = "prob")
fitgl(x, inc = TRUE, method = "prob")

fitgl(x, inc = FALSE, method = "quant", len = 1000)
fitgl(x, inc = TRUE, method = "quant", len = 1000)

fitgl(x, method = "shape")

}
