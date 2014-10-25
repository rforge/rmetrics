\name{plot.fit-method}
\alias{plot.schwartz2f.fit}
\alias{plot,schwartz2f.fit,missing-method}
\alias{plot-fit-methods}
% <---------------------------------------------------------------------->
\title{Plot Schwartz two-factor fit-objects}
% <---------------------------------------------------------------------->
\description{

  This function plots the parameter evolution during the fit, the
  filtered state variables (i.e. the spot price and the convenience
  yield), forward curves, or trajectories of the state variables.

}
% <---------------------------------------------------------------------->
\usage{

\S4method{plot}{schwartz2f.fit,missing}(x, type = c("trace.pars", "state", "forward.curve", "sim"),
     data, ttm, \dots)

}
% <---------------------------------------------------------------------->
\arguments{
  \item{x}{A \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}} object.}
  \item{type}{What shall be plotted (see \bold{Details}).}
  \item{data}{A matrix containing futures prices to which parameters were fitted.}
  \item{ttm}{A matrix with the corresponding time to maturity (see \bold{Details}).}
  \item{\dots}{Arguments passed to \code{\link{plot}}.}
}
% <---------------------------------------------------------------------->
\details{

  If \code{type == "trace.pars"}, the parameter evolution of the
  estimation is plotted. The horizontal lines denote the final value.\cr
  If \code{type == "state"}, the filtered state variables are plotted
  and overlaid with the futures prices.\cr
  If \code{type == "forward.curve"}, fitted forward curves are plotted.\cr
  If \code{type == "sim"}, a bunch of simulated trajectories of the
  state variables are plotted. \cr
  
  The elements of \code{data} and \code{ttm} have the following
  interpretation: \code{data[i,j]} denotes the futures price whose time
  to maturity was \code{ttm[i,j]} when it was observed. The time unit
  was defined by the argument \code{deltat} of the function
  \code{\link{fit.schwartz2f}} (stored in \code{x@deltat}).

  
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\seealso{

  \code{\link{fit.schwartz2f}} for parameter estimation,
  \code{\link[=plot.schwartz2f]{plot}}-method for
  \code{\link[=schwartz2f-class]{schwartz2f}}-objects.

  
}
% <---------------------------------------------------------------------->
\examples{

data(futures)

## Estimate parameters for lumber data (stop after 100 iterations)
fit.obj <- fit.schwartz2f(futures$lumber$price, futures$lumber$ttm / 260,
                          deltat = 1 / 260,
                          control = list(maxit = 100))

## Plot parameter evolution
plot(fit.obj, type = "trace.pars")

## Plot the state variables
plot(fit.obj, type = "state", data = futures$lumber$price,
     ttm = futures$lumber$ttm / 260)

## Plot fitted and real forward curves of wheat data since Jan 2010.
lumber.1995 <- lapply(futures$lumber, function(x)x[as.Date(rownames(x)) < "2000-01-01",])
par(mfrow = c(1, 2))
plot(fit.obj, type = "forward.curve", data = lumber.1995$price,
     ttm = lumber.1995$ttm / 260)
futuresplot(lumber.1995)

## Plot trajectories from the state variables
plot(fit.obj, type = "sim")


}
% <---------------------------------------------------------------------->
\keyword{hplot}
\keyword{methods}
