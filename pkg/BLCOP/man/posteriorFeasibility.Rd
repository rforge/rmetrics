\name{posteriorFeasibility}
\alias{posteriorFeasibility}
\title{Calculate the "feasibility" of the (Black-Litterman) posterior mean}
\description{
    Attilio Meucci and Gianluca Fusai have suggested using the Mahalanobis distance to assess
    the feasibility of a set of Black-Litterman views.  This function calculates this distance, along
    with a "feasibility" measure based on this distance and the sensitivity of the measure to changes in the "q"
    vector.
}
\usage{
posteriorFeasibility(result)
}

\arguments{
  \item{result}{An object of class BLResult}  
}
\details{
    The feasibility measure proposed by Meucci and Fusai (see the references below) is 1 - F(m), where m is the Mahalanobis distance of
    m from the prior mean calculated respect to the prior distribution.  F is the chi-squared CDF of n-degrees of freedom, where
    n is the number assets in one's universe.  It should be noted that in Meucci and Fusai's paper, a version of Black-Litterman is
    used in which the tau parameter is always set to 1.
}
\value{
  \item{mahalDist }{Mahalonobis distance of posterior mean vector from prior mean}
  \item{mahalDistProb }{1 - F(mahalDist), where F is the CDF of the Chi-squared distribution with n = \#assets degrees of freedom}
  \item{sensitivities}{Derivatives of mahalDistProb with respect to the elements of the "q" vector in the set of views.  Not yet implemented}
}
\section{Warning}{
        It is not clear that the results produced by this routine are entirely sensible, though the calculation
        is very straightforward and seems to match the one discussed in the source paper.  Use with caution.
    }
\references{Fusai, Gianluca and Meucci, Attilio.  "Assessing Views", 2002. http://www.symmys.com/AttilioMeucci/Research/PublFinance/PublFinance.html}
\author{Francisco Gochez <fgochez@mango-solutions.com>}
\examples{
    pickMatrix <- matrix(c(rep(1/2, 2), -1,  rep(0, 3)), nrow = 1, ncol = 6 )
    views <- BLViews(P = pickMatrix, q = 0.08,confidences =  100, assetNames = colnames(monthlyReturns))
    marketPosterior <- BLPosterior(monthlyReturns, views, marketIndex = sp500Returns, 
        riskFree = US13wTB)
    posteriorFeasibility(marketPosterior)
}
\keyword{math}
