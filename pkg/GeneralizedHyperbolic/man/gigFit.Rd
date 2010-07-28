\name{gigFit}
\alias{gigFit}
\alias{print.gigFit}
\alias{plot.gigFit}


\title{Fit the Generalized Inverse Gausssian Distribution to Data}
\description{
  Fits a generalized inverse Gaussian distribution to data.
  Displays the histogram, log-histogram (both with fitted densities),
  Q-Q plot and P-P plot for the fit which has the maximum likelihood.
}

\usage{
  gigFit(x, freq = NULL, startValues = c("MoM", "US"),
         paramStart = NULL,
         startMethod = c("Nelder-Mead","BFGS"),
         method = c("Nelder-Mead", "nlm"),
         plots = FALSE, printOut = FALSE,
         controlNM = list(maxit = 1000),
         maxitNLM = 1500, ...)


  \method{print}{gigFit}(x,
        digits = max(3, getOption("digits") - 3), ...)

  \method{plot}{gigFit}(x, which = 1:4,
       plotTitles = paste(c("Histogram of ", "Log-Histogram of ",
                          "Q-Q Plot of ", "P-P Plot of "),
                          x$obsName, sep = ""),
       ask = prod(par("mfcol")) < length(which) & dev.interactive(), ...)
 }

\arguments{
  \item{x}{Data vector for \code{gigFit}. Object of class
    \code{"gigFit"} for \code{print.gigFit} and \code{plot.gigFit}.}
  \item{freq}{A vector of weights with length equal to \code{length(x)}.}
  \item{paramStart}{A user specified starting parameter vector param taking
    the form \code{c(chi, psi, lambda)}.}
  \item{startMethod}{Method used by \code{gigFitStart} in calls to
    \code{\link{optim}}.}
  \item{startValues}{Code giving the method of determining starting
    values for finding the maximum likelihood estimate of param}
  \item{method}{Different optimisation methods to consider.
    See \bold{Details}.}
  \item{plots}{Logical. If \code{FALSE} suppresses printing of the
    histogram, log-histogram, Q-Q plot and P-P plot.}
  \item{printOut}{Logical. If \code{FALSE} suppresses printing of
    results of fitting.}
  \item{controlNM}{A list of control parameters for \code{optim}
    when using the \code{"Nelder-Mead"} optimisation.}
  \item{maxitNLM}{A positive integer specifying the maximum number of
    iterations when using the \code{"nlm"} optimisation.}
  \item{digits}{Desired number of digits when the object is printed.}
   \item{which}{If a subset of the plots is required, specify a subset of
    the numbers \code{1:4}.}
  \item{plotTitles}{Titles to appear above the plots.}
  \item{ask}{Logical. If \code{TRUE}, the user is \emph{ask}ed before
    each plot, see \code{\link{par}(ask = .)}.}
  \item{\dots}{Passes arguments to \code{par}, \code{hist},
    \code{logHist}, \code{qqgig} and \code{ppgig}.}
}
\details{
  \code{startMethod} is	\code{"Nelder-Mead"}.

  \code{startValues} can be either:
  \itemize{
    \item{\code{"US"}}{User-supplied.}
    \item{\code{"MoM"}}{Method of moments.}
    }
  For the details concerning the use of \code{paramStart},
  \code{startMethod}, and \code{startValues}, see
  \code{\link{gigFitStart}}.

  The two optimisation method currently available are:
  \itemize{
    \item{\code{"Nelder-Mead"}}{Uses an implementation of the Nelder and
      Mead method as documented in \code{\link{optim}}.}
    \item{\code{"nlm"}}{Uses the \code{\link{nlm}} function in R.}
  }

  For details of how to pass control information for optimisation using
  \code{\link{optim}} and \code{\link{nlm}}, see \code{\link{optim}} and
  \code{\link{nlm}.}

  When \code{method = "nlm"} is used, warnings may be produced. These do
  not appear to be a problem.
}
\value{
  A list with components:
    \item{param}{A vector giving the maximum likelihood estimate of
      param, as \code{c(chi, psi, lambda)}.}
    \item{maxLik}{The value of the maximised log-likelihood.}
    \item{method}{Optimisation method used.}
    \item{x}{The data used to fit the generalized inverse Gaussian
      distribution.}
    \item{xName}{A character string with the actual \code{x} argument
      name.}
    \item{paramStart}{Starting value of param returned by call to
      \code{\link{gigFitStart}}.}
    \item{startValues}{Acronym for the method of finding start values.}
  }

\references{
  \enc{Jörgensen}{Jorgensen}, B. (1982). \emph{Statistical Properties of
   the Generalized Inverse Gaussian Distribution}. Lecture Notes in
   Statistics, Vol. 9, Springer-Verlag, New York.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, David Cusack}


\seealso{
  \code{\link{optim}}, \code{\link{par}},
  \code{\link{hist}}, \code{\link[DistributionUtils]{logHist}},
  \code{\link{qqgig}}, \code{\link{ppgig}}, and \code{\link{gigFitStart}}.

}

\examples{
param <- c(1, 1, 1)
dataVector <- rgig(500, param = param)
## See how well gigFit works
gigFit(dataVector)
gigFit(dataVector, plots = TRUE)

## See how well gigFit works in the limiting cases
## Gamma case
dataVector2 <- rgamma(500, shape = 1, rate = 1)
gigFit(dataVector2, plots = TRUE)

## Inverse gamma
require(actuar)
dataVector3 <- rinvgamma(500, shape = 1, rate = 1)
gigFit(dataVector3, plots = TRUE)

## Use nlm instead of default
gigFit(dataVector, method = "nlm", plots = TRUE)

}

\keyword{distribution}
