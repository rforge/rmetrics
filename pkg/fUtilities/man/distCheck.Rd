\name{distCheck}


\alias{distCheck}


\title{Distribution Check}


\description{

    Tests properties of a distribution.
    
}


\usage{
distCheck(fun = "norm", n = 1000, robust = TRUE, subdivisions = 100, \dots)
}


\arguments{

    \item{fun}{
        a character string denoting the name of the distribution.
        }
    \item{n}{
        an integer specifying the number of random variates to be
        generated.
        }
    \item{robust}{
        a logical flag, should robust estimates be used? By
        default \code{TRUE}.
        }
    \item{subdivisions}{
        an integer specifying the numbers of subdivisions
        in integration.
        }
    \item{dots}{
        the distributional parameters.
        }
        
}


\author{

    Diethelm Wuertz for this Rmetrics port.
    
}


\examples{
## distCheck:
   distCheck("norm", mean = 1, sd = 1)
}


\keyword{programming}

