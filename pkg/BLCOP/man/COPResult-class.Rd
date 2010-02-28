\name{COPResult-class}
\docType{class}
\alias{COPResult-class}
\alias{densityPlots,COPResult-method}
\alias{show,COPResult-method}
\alias{show.COPResult}
% \alias{probDistance,COPResult-method}
\alias{densityPlots.COPResult}
\alias{optimalPortfolios.fPort.COP}
\alias{optimalPortfolios.fPort,COPResult-method}


\title{Class "COPResult"}
\description{A class that holds the posterior distribution produced with the COP framework}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("COPResult", ...)}. In general however
they are created by the function \code{\link{COPPosterior}}
}
\section{Slots}{
	 \describe{
    \item{\code{views}:}{Object of class \code{"COPViews"}.  These are the views that led to the result }
    \item{\code{marketDist}:}{Object of class \code{"mvdistribution"}.  Prior distribution of the market}
    \item{\code{posteriorSims}:}{Object of class \code{"matrix"}. Matrices holding the simulations of the posteriors with a column for each asset.}
  }
}
\section{Methods}{
  \describe{
    \item{densityPlots}{\code{signature(result = "COPResult")}: Generates density plots of the marginal prior and posterior distributions of each asset.}
    \item{show}{\code{signature(result = "COPResult")}: Displays basic information about the posterior results}
   % \item{probDistance}{\code{signature(result = "COPResult")}: Not yet finished}
%	 }
	\item{optimalPortfolios.fPort}{\code{signature(result = "COPResult")}: Generates optimal prior and posterior portfolios using \code{fPortfolio} package routines }
	}
}
\author{Francisco Gochez <fgochez@mango-solutions.com>}

\seealso{
    \code{\link{COPPosterior}}, \code{\link{BLResult-class}}
}

\keyword{classes}
