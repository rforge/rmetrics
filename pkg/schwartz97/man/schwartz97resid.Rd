\name{resid-futures}
\alias{resid.schwartz2f.fit}
\alias{resid,schwartz2f.fit-method}
\title{Extract Model Residuals}
% <---------------------------------------------------------------------->
\description{

    Function to extract model residuals from
    \code{\link{schwartz2f.fit}}-objects.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{resid}{schwartz2f.fit}(object, data, ttm, type = c("filter", "filter.std", "real"))

}

% <---------------------------------------------------------------------->
\arguments{
  \item{object}{A \code{\link{schwartz2f.fit}}-object returned from \code{\link{schwartz2f.fit}}.}
  \item{data}{A matrix with futures prices.}
  \item{ttm}{A matrix with the corresponding time to maturity (see \bold{Details}).}
  \item{type}{What kind of residuals shall be returned (see \bold{Details}).}
}

% <---------------------------------------------------------------------->
\details{

  The elements of \code{data} and \code{ttm} have the following
  interpretation: \code{data[i,j]} denotes the price of the futures
  contract with time to maturity \code{ttm[i,j]}. \cr

  If \code{type == "filter"}, then the residuals from the measurement
  equation are returned. If \code{type == "filter.std"}, standardized
  residuals from the measurement equation are returned (note that these
  residuals should be standard multivariate normal.
  If \code{type == "real"}, the difference between the observed futures
  prices and the fitted values (see
  \code{\link[=fitted.schwartz2f.fit]{fitted}}) are returned.

}

% <---------------------------------------------------------------------->
\value{

  A matrix containing the residuals.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{fit.schwartz2f}}, \code{\link{schwartz2f.fit}}-class,
  \code{\link[=fitted.schwartz2f.fit]{fitted}}.

}
% <---------------------------------------------------------------------->
% \examples{

% }

% <---------------------------------------------------------------------->
