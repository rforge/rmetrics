\name{transform-extract-methods}
\alias{transform.ghyp}
\alias{transform,ghyp-method}
\alias{[.ghyp}
\alias{[,ghyp,numeric,missing,missing-method}
% <---------------------------------------------------------------------->
\title{Linear transformation and extraction of generalized hyperbolic
distributions}
% <---------------------------------------------------------------------->
\description{

The \code{transform} function can be used to linearly transform
generalized hyperbolic distribution objects.  The extraction operator
\code{[} extracts some margins of a multivariate generalized hyperbolic
distribution object.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{transform}{ghyp}(`_data`, summand, multiplier)

\method{[}{ghyp}(x, i = c(1, 2))
}
% <---------------------------------------------------------------------->
\arguments{
  \item{_data}{An object inheriting from class \code{\link[=ghyp-class]{ghyp}}.}
  \item{summand}{A \code{vector}.}
  \item{multiplier}{A \code{vector} or a \code{matrix}.}
  \item{x}{A multivariate generalized hyperbolic distribution inheriting from class \code{\link[=ghyp-class]{ghyp}}.}
  \item{i}{Index specifying which dimensions to extract.}
  \item{\dots}{Arguments passed to \code{transform}.}
}
% <---------------------------------------------------------------------->
\value{
  An object of class \code{\link[=ghyp-class]{ghyp}}.
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\seealso{\code{\link{ghyp}}, \code{\link{fit.ghypuv}} and \code{\link{fit.ghypmv}}
         for constructors of \code{\link[=ghyp-class]{ghyp}} objects.}
% <---------------------------------------------------------------------->
\examples{
  ## Mutivariate generalized hyperbolic distribution
  multivariate.ghyp <- ghyp(sigma=var(matrix(rnorm(9),ncol=3)), mu=1:3, gamma=-2:0)

  ## Dimension reduces to 2
  transform(multivariate.ghyp, multiplier=matrix(1:6,nrow=2), summand=10:11)

  ## Dimension reduces to 1
  transform(multivariate.ghyp, multiplier=1:3)

  ## Simple transformation
  transform(multivariate.ghyp, summand=100:102)

  ## Extract some dimension
  multivariate.ghyp[1]
  multivariate.ghyp[c(1, 3)]
}
% <---------------------------------------------------------------------->
\keyword{utilities}
\keyword{methods}
