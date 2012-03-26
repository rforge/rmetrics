\name{Parametrization conversion}

\alias{CSW2FKML}
\alias{FKML2CSW}

\title{

  Parameter conversion for the FKML and CSW parameterization.

}

\description{

  Parameter conversion for the FKML and CSW parametrization.

  \itemize{
    \item{CSW2FKML}{Convert parameters from the CSW to the FMKL
      paramtrization.}
    \item{FKML2CSW}{Convert parameters from the FMKL to the CSW
      paramtrization.}
  }

}

\usage{

CSW2FKML(med, iqr, chi, xi)
FKML2CSW(lambda1, lambda2, lambda3, lambda4)

}

\arguments{
  \item{med, iqr, chi, xi}{
    parameters of the Generalized Lambda distribution in the
    CSW parametrization.
  }
  \item{lambda1, lambda2, lambda3, lambda4}{
    parameters of the Generalized Lambda distribution in the
    FKML parametrization.
  }
}

\value{

  A numeric vector of length 4.

}

\references{

  Y. Chalabi, D. J. Scott and D. Wuertz, \emph{An Asymmetry-Steepness
  Parameterization of the Generalized Lambda Distribution}. Working
  paper, 2012.

  M. Freimer, G. Kollia, G. Mudholkar, and C. Lin. \emph{A study of the
  generalized Tukey lambda family}. Communications in Statistics-Theory
  and Methods, 17(10):3547--3567, 1988.

}

\author{

  Yohan Chalabi and Diethelm Wuertz

}

\seealso{

  gldist, gldist-package

}
