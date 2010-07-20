\name{plot.fit-method}
\alias{plot.schwartz2f.fit}
\alias{plot,schwartz2f.fit,missing-method}
\alias{plot-fit-methods}
% <---------------------------------------------------------------------->
\title{Plot Schwartz two-factor fit-objects}
% <---------------------------------------------------------------------->
\description{

  This function plots trajectories of the Schwartz two-factor model
  including the means and standard deviations.

}
% <---------------------------------------------------------------------->
\usage{

\S4method{plot}{schwartz2f.fit,missing}(x, type = c("trace.pars", "state", "forward.curve", "sim"),
            data, ttm, \dots)

}
% <---------------------------------------------------------------------->
\arguments{
  \item{x}{A \code{\link[=schwartz2f-class]{schwartz2f}} object.}
  \item{type}{Number of trajectories.}
  \item{data}{Time span of the simulation.}
  \item{ttm}{Time steps.}
  \item{\dots}{Time steps.}
}
% <---------------------------------------------------------------------->
\details{

  The red solid lines denote the mean and the red dashed lines denote
  the mean plus one standard deviation for the spot price and the mean
  plus/minus one standard deviation for the convenience yield.
  
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\seealso{

  \code{\link[=plot.schwartz2f]{plot}}-method for
  \code{\link[=schwartz2f-class]{schwartz2f}}-objects.

    \code{\link{fit.schwartz2f}}
  
}
% <---------------------------------------------------------------------->
\examples{

  object <- schwartz2f(s0 = 1, mu = 0.1, sigmaS = 0.2,
                     delta0 = 0, kappa = 2, alpha = 0.05, sigmaE = 0.1,
                     rho = 0.5)

  plot(object, n = 50, time = 2, dt = 1/52)

}
% <---------------------------------------------------------------------->
\keyword{hplot}
\keyword{methods}
