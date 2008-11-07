\name{densityPlots}
\alias{densityPlots}
\title{Density plots of prior and posterior distributions}
\description{
    This generic function generates density plots of the marginal posterior and prior distributions of a set of assets in an object of class \code{BLResult} or
    \code{COPResult} for comparative purposes.
}
\usage{
densityPlots(result, assetsSel = NULL, numSimulations = BLCOPOptions("numSimulations"), ...)
}

\arguments{
  \item{result}{Object of class}
  \item{assetsSel}{A numeric vector of assets to plot }
  \item{numSimulations}{ For \code{COPResult} class objects, the number of simulations to use for the market posterior distribution }
  \item{\dots}{Additional arguments passed to \code{plot}}
}
\details{
    For \code{COPResults} objects, density kernel estimates from the samples are used
}
\value{
    None
}

\author{Francisco Gochez, <fgochez@mango-solutions>}

\examples{
    \dontrun{
        dispersion <- c(.376,.253,.360,.333,.360,.600,.397,.396,.578,.775) / 1000
        sigma <- BLCOP:::.symmetricMatrix(dispersion, dim = 4)
        caps <- rep(1/4, 4)
        mu <- 2.5 * sigma %*% caps
        dim(mu) <- NULL
        marketDistribution <- mvdistribution("mt", mean = mu, S = sigma, df = 5 )
        pick <- matrix(0, ncol = 4, nrow = 1, dimnames = list(NULL, c("SP", "FTSE", "CAC", "DAX")))
        pick[1,4] <- 1
        vdist <- list(distribution("unif", min = -0.02, max = 0))
    
        views <- COPViews(pick, vdist, 0.2, c("SP", "FTSE", "CAC", "DAX"))
        posterior <- COPPosterior(marketDistribution, views)
        densityPlots(posterior, 4)
        }
}
\keyword{hplot}

