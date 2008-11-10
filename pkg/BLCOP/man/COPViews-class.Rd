\name{COPViews-class}
\docType{class}
\alias{COPViews-class}
\alias{deleteViews,COPViews-method}
\alias{show,COPViews-method}


\title{Class "COPViews" (copula opinion pooling views)}
\description{ An object that holds a set of analyst views, in the copula opinion pooling sense, on a set of assets}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("COPViews", ...) or with the \code{COPViews} function.}. 
}
\section{Slots}{
   \describe{
    \item{\code{pick}:}{Object of class \code{"matrix"}. The pick matrix}
    \item{\code{viewDist}:}{Object of class \code{"list"}. List of probability distributions of the views }
    \item{\code{confidences}:}{Object of class \code{"numeric"}. }
    \item{\code{assets}:}{Object of class \code{"character"}. Name of the asset "universe" to which these views apply.}
  }
}
\section{Methods}{
  \describe{
    \item{deleteViews}{\code{signature(views = "COPViews", viewsToDel = "numeric")}: Deletes a vector of views from the object, where the vector entries correspond to rows of the pick matrix }
    \item{show}{\code{signature(object = "COPViews")}: Prints views in a user-friendly manner }
	 }
}
\author{Francisco Gochez <fgochez@mango-solutions.com>}

\seealso{
	\code{\linkS4class{BLViews}}, \code{\link{COPViews}}, \code{\link{addCOPViews}}, \code{\link{createCOPViews}}
}
\examples{
    showClass("COPViews")
}
\keyword{classes}
