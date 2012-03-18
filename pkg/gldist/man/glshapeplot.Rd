\name{glshapeplot}

\alias{glshapeplot}

\title{

  Plot fitted shape parameters of the Generalized Lambda Distribution.

}

\description{

  Plot fitted shape parameters of the Generalized Lambda Distribution.

}

\usage{

glshapeplot(x, method, moments = 1:4, \dots)

}

\arguments{
  \item{x}{
    A numeric vector or a numeric matrix of values. Can be \code{NULL} if one
    wants to only draw the line of moments condition of existence.
  }
  \item{method}{
    A character value specifying which method should be used in
    \code{fitgl}.
  }
  \item{moments}{
    A numeric vector specifying which line of moments conditions should
    be drawn. Can be \code{NULL} if condition of existence of moments
    are not desired.
  }
  \item{\dots}{
    additional arguments passed to \code{fitgl()}.
  }
}

\details{

  FIXME

}

\value{

  A list with the output of \code{fitgl} for each column of \code{x}. Is
  \code{NULL} if no data was fitted, i.e. \code{x = NULL}.

}

\references{

  An Asymmetry-Steepness Parameterization of the Generalized Lambda
  Distribution. Y. Chalabi, D. J. Scott, D. W\"urtz.

}

\author{
  Yohan Chalabi and Diethelm W\"urtz
}

\seealso{
  fitgl, gldist, gldist-package
}

\examples{

# Daily Closing Prices of Major European Stock Indices, 1991-1998
data("EuStockMarkets", package = "datasets")

# use percentile log returns
x <- apply(EuStockMarkets, 2, function(x) diff(log(x)))

# Fitted shape parameters
glshapeplot(x)

# without the line conditions of existence of moments
glshapeplot(x, method = "quant", moments = NULL)

# only the conditions of existence of the first 4 moments
glshapeplot(NULL, moments = 1:4)

}
