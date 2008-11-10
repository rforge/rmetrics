\name{COPPosterior}
\alias{COPPosterior}
\title{Calculate the posterior distribution of the market using copula opinion pooling}
\description{
    \code{COPPosteior} uses Attilio Meucci's copula opinion pooling method to incorporate an analyst's subjective
    views with a prior "official" market distribution.  Both the views and the market may have an arbitrary distribution
    as long as it can be sampled in R.  
    Calculations are done with monte-carlo simulation, and the object returned will hold samples drawn from the market
    posterior distribution.
}
\usage{
    COPPosterior(marketDist, views, numSimulations = BLCOPOptions("numSimulations"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{marketDist}{An object of class mvdistribution which describes the prior "official" distribution of the market.}
  \item{views}{An object of class COPViews which describe the subjective views on the market distribution}
  \item{numSimulations}{The number of monte carlo samples to draw during calculations.  Each asset in one's universe will have numSimulations 
  samples from the posterior.}
}

\value{
  An object of class COPResult.
}
\references{Attilio Meucci, "Beyond Black-Litterman:Views on Non-normal Markets".  
    See also Attilio Meucci, "Beyond Black-Litterman in Practice: a Five-Step Recipe to Input Views on non-Normal Markets."}
\author{Francisco Gochez <fgochez@mango-solutions.com>}

\seealso{ \code{\link{BLPosterior}}
    }
\examples{
    
    \dontrun{    
        # An example based on one found in "Beyond Black-Litterman:Views on Non-normal Markets"
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
    }
}
\keyword{math}