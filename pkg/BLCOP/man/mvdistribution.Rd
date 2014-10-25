\name{Distribution class constructors}
\alias{mvdistribution}
\alias{distribution}

\title{Constructors for distribution and mvdistribution class objects}
\description{
    These functions create objects of class \code{distribution} and \code{mvdistribution}
}
\usage{
mvdistribution(RName, ...)
distribution(RName, ...)
}

\arguments{
  \item{RName}{A string holding the R suffix corresponding to the distribution, e.g. "pois" for the Poisson distribution}
  \item{\dots}{Additional parameters that parametrize the distribution}
}
\details{
  In general any distribution with a corresponding sampling function can be used.  This function should
  have the name given in \code{RName} but preceded with an "r", e.g. \code{rnorm} for the normal 
  distribution.  When the constructors are called, they check that the given sampling function exists
  and that it takes the arguments that were passed in the \code{...}.
}
\value{
    An object of class \code{distribution} or \code{mvdistribution}.
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}
\seealso{\code{\link{sampleFrom}}}
\examples{
    \dontrun{
	    # create a uniform distribution object and sample from it
	    myUnif <- distribution("unif", min = -0.1, max = 0.1)
	    hist(sampleFrom(myUnif, 1000))
	    
	    mvNormal <- mvdistribution("mnorm", mean = c(1, 5), varcov = diag(c(2, 0.1)))
	    x <- sampleFrom(mvNormal, 1000)
	    plot(x[,1] ~ x[,2])
    }
}
\keyword{distribution}
