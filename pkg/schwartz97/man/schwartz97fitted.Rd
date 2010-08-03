\name{fitted-futures}
\alias{fitted.schwartz2f.fit}
\alias{fitted,schwartz2f.fit-method}
\title{Extract Model Fitted Values}
% <---------------------------------------------------------------------->
\description{

    Function to extract fitted values from
    \code{\link{schwartz2f.fit}}-objects.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{fitted}{schwartz2f.fit}(object, data, ttm)

}

% <---------------------------------------------------------------------->
\arguments{
  \item{object}{A \code{\link{schwartz2f.fit}}-object returned from \code{\link{schwartz2f.fit}}.}
  \item{data}{A matrix with futures prices.}
  \item{ttm}{A matrix with the corresponding time to maturity (see \bold{Details}).}
}

% <---------------------------------------------------------------------->
\details{

  The elements of \code{data} and \code{ttm} have the following
  interpretation: \code{data[i,j]} denotes the futures price whose time
  to maturity was \code{ttm[i,j]} when it was observed (in units of
  \code{object@deltat}).

}

% <---------------------------------------------------------------------->
\value{

  A matrix containing the fitted values of the same dimension as
  \code{data}.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{fit.schwartz2f}}, \code{\link{schwartz2f.fit}}-class,
  \code{\link[=resid.schwartz2f.fit]{resid}}.

}
% <---------------------------------------------------------------------->
\examples{

data(futures)

### Estimate parameters for lumber data (stop after 100 iterations)
fit.obj <- fit.schwartz2f(futures$lumber$price, futures$lumber$ttm / 260,
                          control = list(maxit = 100))

### Get the fitted values
fitted.futures <- fitted(fit.obj, futures$lumber$price, futures$lumber$ttm / 260)

par(mfrow = c(1, 3))
### Plot futures prices
plot(as.ts(futures$lumber$price), plot.type = "single", ylab = "Futures prices",
     col = gray(seq(0.1, 0.9, length = 4)))
### Plot fitted values
plot(as.ts(fitted.futures), plot.type = "single", ylab = "Fitted values",
     col = gray(seq(0.1, 0.9, length = 4)))
### Plot relative difference
plot(as.ts((fitted.futures - futures$lumber$price) / fitted.futures), plot.type = "single",
     ylab = "Relative difference", col = gray(seq(0.1, 0.9, length = 4)))

}

% <---------------------------------------------------------------------->
