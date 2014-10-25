\name{distribution-class}
\docType{class}
\alias{distribution-class}

\title{Class "distribution"}
\description{A class that describes univariate distributions}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("distribution", ...)}. There is also
a constructor which is also named \code{\link{distribution}}.
}
\section{Slots}{
 \describe{
    \item{\code{RName}:}{Object of class \code{"character"}.  This is the R "suffix" of the distirbution.}
    \item{\code{parameters}:}{Object of class \code{"numeric"}.  A named numeric vector that holds the parameters of the distribution}
  }
}


\author{Francisco Gochez <fgochez@mango-solutions.com>}

\seealso{
	\code{\link{distribution}}, \code{\linkS4class{mvdistribution}}, \code{\link{mvdistribution}}
}
\examples{
    showClass("distribution")
}
\keyword{classes}
