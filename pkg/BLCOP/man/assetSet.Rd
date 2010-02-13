\name{Extractors}
\alias{assetSet}
\alias{PMatrix}
\alias{confidences}
\alias{viewMatrix}
\alias{posteriorMeanCov}
\alias{posteriorSimulations}
\alias{numSimulations}
\alias{priorViews}
\title{Extract various fields of view or posterior objects}
\description{
    A collection of functions to extract several fields of BLViews, COPViews, COPPosterior and BLPosterior
    objects.
}
\usage{
assetSet(views)
viewMatrix(views, dropZeroColumns = TRUE)
PMatrix(views)
confidences(views)
posteriorMeanCov(posterior)
posteriorSimulations(posterior)
numSimulations(posterior)
priorViews(posterior)
}

\arguments{
  \item{views}{An object of class BLViews or COPViews}
  \item{posterior}{An object of class BLPosterior (posteriorMeanCov) or COPPosterior (posteriorSimulations, priorViews)
  , as appropriate}
  \item{dropZeroColumns}{Logical flag.  If TRUE, columns of "view matrix" which only have zeros are dropped}
}


\value{
    \item{assetSet}{The names of the assets in the view object's universe}
    \item{confidences}{The set of confidences in each view.}
    \item{PMatrix}{The 'pick' matrix}
    \item{viewMatrix}{The pick matrix augmented with the q vector of the BL model}
    \item{posteriorMeanCov}{The posterior mean and covariance (in a list) of a BLPosterior object}
    \item{posteriorSimulations}{ Matrix of posterior distribution simulations held in a COPPosterior object }
    \item{numSimulations}{ Number of simulations in posterior COP distribution }
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}

\examples{
    pick <- matrix(0, ncol = 4, nrow = 1, dimnames = list(NULL, c("SP", "FTSE", "CAC", "DAX")))
    pick[1,4] <- 1
    vdist <- list(distribution("unif", min = -0.02, max = 0))
    views <- COPViews(pick, vdist, 0.2, c("SP", "FTSE", "CAC", "DAX"))
    assetSet(views)
    confidences(views)
    PMatrix(views)

}

\keyword{utilities}
