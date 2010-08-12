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

  If \code{type == "filter"}, then the residuals from the measurement
  equation are returned. If \code{type == "filter.std"}, standardized
  residuals from the measurement equation are returned (note that these
  residuals should be standard multivariate normal).
  If \code{type == "real"}, the difference between the observed futures
  prices and the fitted values (see
  \code{\link[=fitted.schwartz2f.fit]{fitted}}) are returned.

  The model and its parameters are described in the \bold{Details}
  section of the \code{\link[=schwartz2f-class]{schwartz2f}}-class
  documentation and in the package vignette \emph{Technical Document}.

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
\examples{

data(futures)

### Estimate parameters for live.cattle data.
### (little precision required with reltol = 1e-3)
fit.obj <- fit.schwartz2f(futures$live.cattle$price, futures$live.cattle$ttm / 260,
                          deltat = 1 / 260,
                          control = list(maxit = 100, reltol = 1e-3))

### Standardized residuals 
resid.std <- resid(fit.obj, data = futures$live.cattle$price, ttm =
                   futures$live.cattle$ttm / 260, type = "filter.std")
acf(resid.std) # ...are not independent

### Real differences
resid.real <- resid(fit.obj, data = futures$live.cattle$price, ttm =
                    futures$live.cattle$ttm / 260, type = "real")

plot(as.ts(resid.real / futures$live.cattle$price)) # ...are 'relatively' accurate.
}

% <---------------------------------------------------------------------->
