\name{BLPosterior}
\alias{BLPosterior}
\alias{posteriorEst}
\title{Compute the posterior distribution of the market according the the Black-Litterman model}
\description{
    BLPosterior is a wrapper function that makes it easy to compute
    the posterior mean and covariance of the returns of an asset set (specified as a matrix ) in one go.  It computes
    the market parameters automatically and then calls \code{posteriorEst}.  \code{posteriorEst} is the "core" function of the model.
    
}
\usage{
BLPosterior(returns, views, tau = 1, marketIndex, riskFree = NULL, kappa = 0, covEstimator = "cov")
posteriorEst(views, mu, tau = 0.5, sigma, kappa = 0)

}

\arguments{
  \item{returns}{A matrix of time series of returns.  The columns should correspond to individual assets.}
  \item{views}{An object of class BLViews}
  \item{tau}{The "tau" parameter in the Black-Litterman model.}
  \item{marketIndex}{A set of returns of a market index.}
  \item{riskFree}{A time series of risk-free rates of return.  Defaults to 0} 
  \item{mu}{A vector of mean equilibrium returns}
  \item{sigma}{The variance-covariance matrix of the returns of the assets}
  \item{kappa}{If greater than 0, the confidences in each view are replaced.  See the details section
      for more information}
  \item{covEstimator}{A string holding the name of the function that should be used to estimate the variance-covariance matrix.
    This function should simply return a matrix.}
}
\details{
    Some authors have suggested that the confidences should not be set by the analyst but rather should just be equal
    to kappa * t(P) \%*\% Sigma \%*\% P.  Setting kappa > 0 will cause this to happen.  In BLPosterior, the equilibrium return
    means are computed with \code{\link{CAPMList}}.
}
\value{
    An object of class BLResult
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}

\examples{ 
## example from Thomas M. Idzorek's paper "A STEP-BY-STEP GUIDE TO THE BLACK-LITTERMAN MODEL"
x <- c(0.001005,0.001328,-0.000579,-0.000675,0.000121,0.000128,-0.000445,-0.000437 ,
     0.001328,0.007277,-0.001307,-0.000610,-0.002237,-0.000989,0.001442,-0.001535 ,
     -0.000579,-0.001307,0.059852,0.027588,0.063497,0.023036,0.032967,0.048039 ,
    -0.000675,-0.000610,0.027588,0.029609,0.026572,0.021465,0.020697,0.029854 ,
     0.000121,-0.002237,0.063497,0.026572,0.102488,0.042744,0.039943,0.065994 ,
     0.000128,-0.000989,0.023036,0.021465,0.042744,0.032056,0.019881,0.032235 ,
    -0.000445,0.001442,0.032967,0.020697,0.039943,0.019881,0.028355,0.035064 ,
    -0.000437,-0.001535,0.048039,0.029854,0.065994,0.032235,0.035064,0.079958 )

    varCov <- matrix(x, ncol = 8, nrow = 8)
    mu <- c(0.08, 0.67,6.41, 4.08, 7.43, 3.70, 4.80, 6.60) / 100
    pick <- matrix(0, ncol = 8, nrow = 3, dimnames = list(NULL, letters[1:8]))
    pick[1,7] <- 1
    pick[2,1] <- -1; pick[2,2] <- 1
    pick[3, 3:6] <- c(0.9, -0.9, .1, -.1)
    confidences <- 1 / c(0.000709, 0.000141, 0.000866)
    myViews <- BLViews(pick, c(0.0525, 0.0025, 0.02), confidences, letters[1:8])
    myPosterior <- posteriorEst(myViews, tau = 0.025, mu, varCov )
    myPosterior
}
\keyword{math}
\keyword{models}
