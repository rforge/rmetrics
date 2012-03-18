\name{GLDparams}

\alias{CSW2FMKL}
\alias{FMKL2CSW}

\title{

  Parameter conversion for the FMKL and CSW parametrization.

}

\description{

  Parameter conversion for the FMKL and CSW parametrization.

  \itemize{
    \item{CSW2FMKL}{Convert parameters from the CSW to the FMKL
      paramtrization.}
    \item{FMKL2CSW}{Convert parameters from the FMKL to the CSW
      paramtrization.}
  }

}

\usage{

CSW2FMKL(med, iqr, chi, xi)
FMKL2CSW(lambda1, lambda2, lambda3, lambda4)

}

\arguments{
  \item{med, iqr, chi, xi}{
    parameters of the Generalized Lambda distribution in the
    CSW parametrization.
  }
  \item{lambda1, lambda2, lambda3, lambda4}{
    parameters of the Generalized Lambda distribution in the
    FMKL parametrization.
  }
}

\details{

  FIXME

}

\value{

  A numeric vector of length 4.

}

\references{

  An Asymmetry-Steepness Parameterization of the Generalized Lambda
  Distribution. Y. Chalabi, D. J. Scott, D. W\"urtz.

}

\author{
  Yohan Chalabi and Diethelm W\"urtz
}

\seealso{
  gldist, gldist-package
}
