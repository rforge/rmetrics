\name{mean-vcov-methods}
\alias{mean.schwartz2f}
\alias{mean-methods}
\alias{mean,schwartz2f-method}
\alias{vcov.schwartz2f}
\alias{vcov-methods}
\alias{vcov,schwartz2f-method}
% <---------------------------------------------------------------------->
\title{Expected value and variance-covariance}
% <---------------------------------------------------------------------->
\description{

  The function \code{mean} returns the expected value of the spot price
  and the convenience yield for some time in the future. The function
  \code{vcov} returns the covariance matrix of the \emph{log} spot price
  and the convenience yield.

}
% <---------------------------------------------------------------------->
\usage{

\S4method{mean}{schwartz2f}(x, time = 1)

\S4method{vcov}{schwartz2f}(object, time = 1)

}
% <---------------------------------------------------------------------->
\arguments{
  
  \item{x, object}{An object inheriting from class
                   \code{\link[=schwartz2f-class]{schwartz2f}}.}
  \item{time}{The point in time for which the mean or
    covariance matrix is computed. \code{mean} accepts a vector,
    \code{vcov} a scalar only.}
  
}
% <---------------------------------------------------------------------->
\note{
  
  \code{vcov} returns the variance-covariance matrix of [\emph{log} spot
  price, convenience yield](time).
  
}
% <---------------------------------------------------------------------->
\value{
  
  Either the expected value or variance-covariance matrix.
  
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\seealso{

  \code{\link{schwartz2f}} to create Schwartz 2 factor objects,
  \code{\link[=plot.schwartz2f]{plot}} to show trajectories of the state
  variables including means and standard deviations.

}
% <---------------------------------------------------------------------->
\examples{

  mean(schwartz2f(mu = 0.1), time = 1)

  mean(schwartz2f(mu = 0.2), time = 0:3)

  vcov(schwartz2f(), time = 10)

  ### Plot a schwartz2f-object including means and standard deviations
  plot(schwartz2f(sigmaE = 0.1), n = 50, time = 5, dt = 1 / 52)

}
% <---------------------------------------------------------------------->
\keyword{utilities}
\keyword{methods}
