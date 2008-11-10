\name{BLCOPOptions}
\alias{BLCOPOptions}
\title{Global package options}
\description{
This function can be used to set or get global options for the BLCOP package.    
}
\usage{
BLCOPOptions(opt, setting)
}

\arguments{
  \item{opt}{A string with the name of an option }
  \item{setting}{The new setting for the option}
}
\details{
    If \code{setting} is omitted, the current setting for \code{opt} is returned.  If both
    arguments are omitted, a list with all of the settings is returned.  The following settings may
    be changed:
    \code{regFunc}:Function used to perform the regression in \code{CAPMalphas}
    \code{numSimulations}:Number of monte-carlo simulations to perform in copula opinion pooling functions
    \code{unitTestPath}: Path where unit tests are located.
}
\value{
    If both arguments omitted, a list.  If setting is omitted, value of an individual setting.

}
\author{Francisco Gochez <fgochez@mango-solutions>}
\examples{
    BLCOPOptions("numSimulations")
}
\keyword{misc}
                                                                      