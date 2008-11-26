\name{scale-methods}
\alias{scale.ghyp}
\alias{scale,ghyp-method}
% <---------------------------------------------------------------------->
\title{Scaling and Centering of ghyp Objects}
% <---------------------------------------------------------------------->
\description{

\code{scale} centers and/or scales a generalized hyperbolic distribution
to zero expectation and/or unit variance.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{scale}{ghyp}(x, center = TRUE, scale = TRUE)

}
% <---------------------------------------------------------------------->
\arguments{
  \item{x}{An object inheriting from class \code{\link[=ghyp-class]{ghyp}}.}
  \item{center}{A logical value stating whether the object shall be
    centered to zero expectation.}
  \item{scale}{A logical value stating whether the object shall be
    scaled to unit variance.}
}
% <---------------------------------------------------------------------->
\value{
  An object of class \code{\link[=ghyp-class]{ghyp}}.
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->

\seealso{\code{\link[=transform.ghyp]{transform}}, \code{\link{mean}},
\code{\link{vcov}}.}
% <---------------------------------------------------------------------->
\examples{
  data(indices)

  t.fit <- fit.tmv(indices)
  gauss.fit <- fit.gaussmv(indices)

  ## Compare the fitted Student-t and Gaussian density.
  par(mfrow = c(1, 2))

  ## Once on the real scale...
  plot(t.fit[1], type = "l")
  lines(gauss.fit[1], col = "red")

  ## ...and once scaled to expectation = 0, variance = 1
  plot(scale(t.fit)[1], type = "l")
  lines(scale(gauss.fit)[1], col = "red")
}
% <---------------------------------------------------------------------->
\keyword{utilities}
\keyword{methods}
