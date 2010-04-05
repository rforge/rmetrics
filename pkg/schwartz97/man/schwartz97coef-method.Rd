\name{coef-method}
\alias{coef.schwartz2f}
\alias{coef.schwartz2f.fit}
\alias{coef,schwartz2f-method}
\alias{coef,schwartz2f.fit-method}
\alias{coefficients,schwartz2f-method}
\alias{coefficients,schwartz2f.fit-method}
\title{Extract parameters of schwartz2f objects}
\description{

  The function \code{coef} returns parameters of \code{schwartz2f}
  and a \code{schwartz2f.fit} objects as a list. The function
  \code{coefficients} is a synonym for \code{coef}.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{coef}{schwartz2f}(object)
\S4method{coefficients}{schwartz2f}(object)

\S4method{coef}{schwartz2f.fit}(object)
\S4method{coefficients}{schwartz2f.fit}(object)

}
% <---------------------------------------------------------------------->
\arguments{

  \item{object}{An object from class
    \code{\link[=schwartz2f-class]{schwartz2f}} or
    \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}}.}

}
% <---------------------------------------------------------------------->
\value{
  If \code{object} is of class \code{\link[=schwartz2f-class]{schwartz2f}}:
  \tabular{rl}{
  \code{s0} \tab Commodity spot price.\cr
  \code{delta0} \tab Convenience yield. \cr
  \code{mu} \tab Drift parameter of the spot price process. \cr
  \code{sigmaS} \tab Diffusion parameter of the spot price process. \cr
  \code{kappa}\tab Speed of mean-reversion of the convenience yield process.\cr
  \code{alpha}\tab Mean-level of the convenience yield process.\cr
  \code{sigmaE}\tab Diffusion parameter of the convenience yield process.\cr
  \code{rho}\tab Correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.\cr
  }

  If \code{object} is of class \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}}:
  \tabular{rl}{
  \code{s0} \tab Commodity spot price.\cr
  \code{delta0} \tab Convenience yield. \cr
  \code{mu} \tab Drift parameter of the spot price process. \cr
  \code{sigmaS} \tab Diffusion parameter of the spot price process. \cr
  \code{kappa}\tab Speed of mean-reversion of the convenience yield process.\cr
  \code{alpha}\tab Mean-level of the convenience yield process.\cr
  \code{sigmaE}\tab Diffusion parameter of the convenience yield process.\cr
  \code{rho}\tab Correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.\cr
  \code{r}\tab Instantaneous risk-free interest rate.\cr
  \code{lambda}\tab Market price of convenience yield risk (see \bold{Details}).\cr
  \code{alphaT}\tab Mean-level of the convenience yield process with
    respect to the equivalent martingale measure (see \bold{Details}).\cr
  }

}
% <---------------------------------------------------------------------->
\details{

  The market price of convenience yield risk can either be specified
  explicitly by \code{lambda} or implicigtly by \code{alphaT}. The
  relation is \code{alphaT = alpha - lambda / kappa}. See the package
  vignette or
  \code{\link[=schwartz2f-class]{schwartz2f-classes}}.

}
% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}
% <---------------------------------------------------------------------->
\seealso{

  \code{\link[=schwartz2f-class]{schwartz2f}} classes.

}
% <---------------------------------------------------------------------->
\examples{

  coef(schwartz2f())

}
% <---------------------------------------------------------------------->
\keyword{methods}
\keyword{utilities}
