\name{gigFitStart}
\alias{gigFitStart}

\title{Find Starting Values for Fitting a
  Generalized Inverse Gaussian Distribution}
\description{
  Finds starting values for input to a maximum likelihood routine for
  fitting generalized inverse Gaussian distribution to data.
}

\usage{
  gigFitStart(x, startValues = "MoM",
                 startMethodMoM = "Nelder-Mead", ...)
}

\arguments{
  \item{x}{Data vector.}
  \item{startValues}{Acronym indicating the method to use for obtaining
  starting values to be used as input to \code{gigFit}. See
  \bold{Details}.}
  \item{startMethodMoM}{Method used by call to \code{\link{optim}} in
    finding method of moments estimates.}
  \item{\dots}{Passes arguments to \code{\link{optim}}.}
}

\details{
  The only value of the argument \code{startValues} so far implemented is:
  \itemize{
    \item{\code{"MoM"}}{Method of moments.}
  }

  When \code{startValues = "MoM"} an initial optimisation is needed to
  find the starting values. This optimisations calls
  \code{\link{optim}}.
}


\value{
  \code{gigFitStart} returns \code{paramStart}, vector with elements
    \code{chi}, \code{psi}, and \code{lambda} giving the starting value
    of \code{param}.
  }


\author{David Scott \email{d.scott@auckland.ac.nz},
  David Cusack}


\seealso{
  \code{\link{dgig}}, \code{\link{gigFit}}.
}

\examples{
param <- c(1, 1, 1)
dataVector <- rgig(500, param = param)
gigFitStart(dataVector)
}

\keyword{distribution}
