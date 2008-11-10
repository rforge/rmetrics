\name{BLResult-class}
\docType{class}
\alias{BLResult-class}
\alias{densityPlots,BLResult-method}
\alias{show,BLResult-method}
\alias{densityPlots.BLResult}

\title{Class "BLResult": posterior of a market distribution in the Black-Litterman sense }
\description{This class holds the posterior market mean and variance-covariance matrix calculated from some prior and set of views.  The original views are also returned.}
\section{Objects from the Class}{
	Objects can be created by calls of the form \code{new("BLResult", ...)}.  However, it is intended that they be created by
	the function posteriorEst(or wrappers to that function).}
\section{Slots}{
	 \describe{
    \item{\code{views}:}{Object of class \code{"BLViews"}.  These are the original views used to calculate this posterior}
    \item{\code{tau}:}{Object of class \code{"numeric"}.  The value of "tau" used}
    \item{\code{priorMean}:}{Object of class \code{"numeric"}: prior vector of market means}
    \item{\code{priorCovar}:}{Object of class \code{"matrix"}: prior of the variance-covariance}
    \item{\code{posteriorMean}:}{Object of class \code{"numeric"}:  posterior mean}
    \item{\code{posteriorCovar}:}{Object of class \code{"matrix"}: posterior variance-covariance}
    \item{\code{confIgnored}:}{Object of class \code{"logical"}: logical flag indicating whether or not confidences-in-views
    were ignored.}
  }
}
\section{Methods}{
  \describe{
    \item{denityPlots}{\code{signature(result = "BLResult")}: Plots the marginal distributions of the asset returns under the prior and posterior distributions}
    \item{show}{\code{signature(object = "BLResult")}: Displays the contents of a result}
	 }
}
\author{Francisco Gochez}

\examples{

}
\keyword{classes}
