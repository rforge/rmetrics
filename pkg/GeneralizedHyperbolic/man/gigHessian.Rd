\name{gigHessian}
\alias{gigHessian}

\title{
Calculate Two-Sided Hessian for the Generalized Inverse Gaussian Distribution
}
\description{
Calculates the Hessian of a function, either exactly or approximately. Used to
obtaining the information matrix for maximum likelihood estimation.
}
\usage{
gigHessian(x, param, hessianMethod = "tsHessian",
           whichParam = 1)
}
\arguments{
  \item{x}{Data vector.}
  \item{param}{The maximum likelihood estimates parameter vector of the
    generalized inverse Gaussian distribution. There are five different sets of
    parameterazations can be used in this function, the first four sets
    are listed in \code{gigChangePars} and the last set is the log
    scale of the first set of the parameterization, i.e.,
    \code{mu,log(delta),Pi,log(zeta)}.  }
  \item{hessianMethod}{Only the approximate method (\code{"tsHessian"})
    has actually been implemented so far.}
  \item{whichParam}{Numeric. A number between indicating which
    parameterization the argument \code{param} relates to. Only
    parameterization 1 is available so far.}
}
\details{
  The approximate Hessian is obtained via a call to \code{tsHessian}
  from the package \code{DistributionUtils}.  \code{summary.gigFit}
  calls the function \code{gigHessian} to calculate the Hessian matrix
  when the argument \code{hessian = TRUE}.
}
\value{
  \code{gigHessian} gives the approximate Hessian matrix for
  the data vector \code{x} and the estimated parameter vector
  \code{param}.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}, David Cusack
}

\examples{
### Calculate the approximate Hessian using gigHessian:
param <- c(1,1,1)
dataVector <- rgig(500, param = param)
fit <- gigFit(dataVector)
coef <- coef(fit)
gigHessian(x = dataVector, param = coef, hessianMethod = "tsHessian",
              whichParam = 1)

### Or calculate the approximate Hessian using summary.gigFit method:
summary(fit, hessian = TRUE)
}



