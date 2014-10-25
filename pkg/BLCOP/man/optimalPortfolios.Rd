\name{optimalPortfolios}
\alias{optimalPortfolios}
\alias{optimalPortfolios.fPort}
\title{Calculates optimal portfolios under prior and posterior distributions}
\description{
  These are wrapper functions that calculate optimal portfolios under the prior and posterior return distributions.  
  \code{optimalPortfolios} works with a user-supplied optimization  function, 
  though simple Markowitz minimum-risk optimization is done with \code{solve.QP} from \code{quadprog} if none is supplied.
  \code{optimalPortfolios.fPort} is a generic utility function which calculates optimal portfolios using routines from
  the \code{fPortfolio} package.
  
}
\usage{
optimalPortfolios(result, optimizer = .optimalWeights.simpleMV, ..., doPlot = TRUE, beside = TRUE)
optimalPortfolios.fPort(result, spec = NULL, constraints = "LongOnly", optimizer = "minriskPortfolio", 
    inputData = NULL, numSimulations = BLCOPOptions("numSimulations")) 
}

\arguments{
  \item{result}{An object of class \code{BLResult}}
  \item{optimizer}{For \code{optimalPortfolios}, An optimization function.  It should take as arguments a vector of means and
      a variance-covariance matrix, and should return a vector of optimal weights.  For \code{optimalPortfolios}, 
      the name of a \code{fPortfolio} function that performs portfolio optimization}
  \item{spec}{Object of class \code{fPORTFOLIOSPEC}.  If NULL, will use a basic mean-variance spec for Black-Litterman
  results, and a basic CVaR spec for COP results}
  \item{inputData}{Time series data (any form that can be coerced into a \code{timeSeries} object)}
  \item{constraints}{String of constraints that may be passed into \code{fPortfolio} optimization routines}
  \item{numSimulations}{For COP results only - the number of posterior simulations to use in the optimization (large
  	numbers here will likely cause the routine to fail)}
  \item{\dots}{ Additional arguments to the optimization function }
  \item{doPlot}{ A logical flag.  Should barplots of the optimal portfolio weights be produced? }
  \item{beside}{A logical flag. If a barplot is generated, should the bars appear side-by side? If \code{FALSE}, 
      differences of weights will be plotted instead.}
}
\details{
    By default, \code{optimizer} is a simple function that performs Markowitz optimization via
    \code{solve.QP}.  In addition to a mean and variance, it takes an optional \code{constraints}
    parameter that if supplied should hold a named list with all of the parameters that \code{solve.QP}
    takes.  
}
\note{ It is expected that \code{optimalPortfolios} will be deprecated in future releases in favour of 
\code{optimalPortfolios.fPort}.
}

\value{
  \code{optimalPortfolios} will return a list with the following items:
  	\item{priorPFolioWeights}{The optimal weights under the prior distribution}
  	\item{postPFolioWeights}{The optimal weights under the posterior distribution}
  \code{optimalPortfolios.fPort} will return a similar list with 2 elements of class \code{fPORTFOLIO}.
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}
\references{Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009); Portfolio Optimization with R/Rmetrics, Rmetrics eBook, Rmetrics Association and Finance Online, Zurich. }
\examples{
	\dontrun{
	    entries <- c(0.001005,0.001328,-0.000579,-0.000675,0.000121,0.000128,-0.000445,-0.000437 ,
	     0.001328,0.007277,-0.001307,-0.000610,-0.002237,-0.000989,0.001442,-0.001535 ,
	     -0.000579,-0.001307,0.059852,0.027588,0.063497,0.023036,0.032967,0.048039 ,
	    -0.000675,-0.000610,0.027588,0.029609,0.026572,0.021465,0.020697,0.029854 ,
	     0.000121,-0.002237,0.063497,0.026572,0.102488,0.042744,0.039943,0.065994 ,
	     0.000128,-0.000989,0.023036,0.021465,0.042744,0.032056,0.019881,0.032235 ,
	    -0.000445,0.001442,0.032967,0.020697,0.039943,0.019881,0.028355,0.035064 ,
	    -0.000437,-0.001535,0.048039,0.029854,0.065994,0.032235,0.035064,0.079958 )
	    
	    varcov <- matrix(entries, ncol = 8, nrow = 8)
	    mu <- c(0.08, 0.67,6.41, 4.08, 7.43, 3.70, 4.80, 6.60) / 100
	    pick <- matrix(0, ncol = 8, nrow = 3, dimnames = list(NULL, letters[1:8]))
	    pick[1,7] <- 1
	    pick[2,1] <- -1; pick[2,2] <- 1
	    pick[3, 3:6] <- c(0.9, -0.9, .1, -.1)
	    confidences <- 1 / c(0.00709, 0.000141, 0.000866)
	    views <- BLViews(pick, c(0.0525, 0.0025, 0.02), confidences, letters[1:8])
	    posterior <- posteriorEst(views, tau = 0.025, mu, varcov )
	    optimalPortfolios(posterior, doPlot = TRUE)
    
    	optimalPortfolios.fPort(posterior, optimizer = "tangencyPortfolio")
    	
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
    
        optimalPortfolios.fPort(myPosterior, spec = NULL, optimizer = "minriskPortfolio", inputData = NULL, 
			numSimulations  = 100	)
	
    }
}
\keyword{utilities}
