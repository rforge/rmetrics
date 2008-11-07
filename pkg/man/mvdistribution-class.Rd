\name{mvdistribution-class}
\docType{class}
\alias{mvdistribution-class}

\title{Class "mvdistribution"}
\description{A class that describes multivariate distributions}
\section{Objects from the Class}{
   Objects can be created by calls of the form \code{new("distribution", ...)}. There is also 
   a constructor which is also named \code{\link{mvdistribution}}.
    }
\section{Slots}{
	 \describe{
    \item{\code{RName}:}{Object of class \code{"character"}. This is the R "suffix" of the distirbution.}
    \item{\code{parameters}:}{A named list of parameters that characterize the distribution}
  }
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}

\examples{
      showClass("mvdistribution")
}
\seealso{
	\code{\link{distribution}}, \code{\link{mvdistribution}}, \code{\link{distribution-class}}
}
\keyword{classes}
