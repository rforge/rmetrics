\name{gigFitStart}
\alias{gigFitStart}
\alias{gigFitStartMoM}
\alias{gigFitStartLM}

\title{Find Starting Values for Fitting a
  Generalized Inverse Gaussian Distribution}
\description{
  Finds starting values for input to a maximum likelihood routine for
  fitting the generalized inverse Gaussian distribution to data.
}

\usage{
  gigFitStart(x, startValues = c("LM","GammaIG","MoM","Symb","US"),
              paramStart = NULL,
              startMethodMoM = c("Nelder-Mead","BFGS"), ...)
  gigFitStartMoM(x, paramStart = NULL,
                 startMethodMoM = "Nelder-Mead", ...)
  gigFitStartLM(x, ...)
}

\arguments{
  \item{x}{Data vector.}
  \item{startValues}{Acronym indicating the method to use for obtaining
  starting values to be used as input to \code{gigFit}. See
  \bold{Details}.}
  \item{paramStart}{Starting values for param if \code{startValues =
      "US"}.}
  \item{startMethodMoM}{Method used by call to \code{\link{optim}} in
    finding method of moments estimates.}
  \item{\dots}{Passes arguments to \code{\link{optim}} and calls to
    \code{\link{hist}}.}
}

\details{
  Possible values of the argument \code{startValues} are the following:
  \itemize{
    \item{\code{"LM"}}{Based on fitting linear models to the upper tails
      of the data \code{x} and the inverse of the data \code{1/x}.}
    \item{\code{"GammaIG"}}{Based on fitting gamma and inverse gamma
      distributions.}
    \item{\code{"MoM"}}{Method of moments.}
    \item{\code{"Symb"}}{Not yet implemented.}
    \item{\code{"US"}}{User-supplied.}
  }
  If \code{startValues = "US"} then a value must be supplied for
  \code{paramStart}.

  When \code{startValues = "MoM"} an initial optimisation is needed to
  find the starting values. This optimisations starts from arbitrary
  values, \code{c(1,1,1)} for the parameters
  \eqn{(\chi,\psi,\lambda)}{(chi, psi, lambda)} and calls
  \code{\link{optim}} with the method given by
  \code{startMethodMoM}. Other starting values for the method of moments
  can be used by supplying a value for \code{paramStart}.

  The default method of finding starting values is
  \code{"LM"}. Testing indicates this is quite fast and finds good
  starting values. In addition, it does not require any starting values
  itself.

  \code{gigFitStartMoM} is called by \code{gigFitStart} and implements
  the method of moments approach.

  \code{gigFitStartLM} is called by \code{gigFitStart} and implements
  the linear models approach.
}

\value{
  \code{gigFitStart} returns a list with components:
    \item{paramStart}{A vector with elements \code{chi}, \code{psi},
      and \code{lambda} giving the starting value of \code{param}.}
    \item{breaks}{The cell boundaries found by a call to
      \code{\link{hist}}.}
    \item{midpoints}{The cell midpoints found by a call to
      \code{\link{hist}}.}
    \item{empDens}{The estimated density found by a call to
      \code{\link{hist}}.}

  \code{gigFitStartMoM} and \code{gigFitStartLM} each return
  \code{paramStart}, the starting value of \code{param}, to the calling
  function \code{gigFitStart}
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
