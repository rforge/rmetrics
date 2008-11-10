\name{BLViews-class}
\docType{class}
\alias{BLViews-class}
\alias{deleteViews,BLViews-method}
\alias{show,BLViews-method}

\title{Class "BLViews" (Black-Litterman views)}
\description{ An object that holds a set of analyst views, in the Black-Litterman sense, on a set of assets}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BLViews", ...)} or with the \code{BLViews} function.
}
\section{Slots}{
	 \describe{
    \item{\code{P}:}{Object of class \code{"matrix"}. The "pick" matrix}
    \item{\code{qv}:}{Object of class \code{"numeric"}. Means of the views}
    \item{\code{confidences}:}{Object of class \code{"numeric"}.  Holds the confidence in each of the individual views}
    \item{\code{assets}:}{Object of class \code{"character"}: Name of the asset "universe" to which these views apply }
  }
}

\section{Methods}{
  \describe{
    \item{deleteViews}{\code{signature(views = "BLViews", viewsToDel = "numeric")}: Deletes a vector of views from the object, where the vector entries correspond to rows of the pick matrix}
    \item{show}{\code{signature(object = "BLViews")}: Prints views in a user-friendly manner}
  }
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}

\examples{

}
\keyword{classes}