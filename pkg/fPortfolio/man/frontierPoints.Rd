\name{frontierPoints}


\alias{frontierPoints}


\title{Get Frontier Points}


\description{
  
    Extracts the risk and return coordinates of
    the efficient frontier.
    
}
    

\usage{
frontierPoints(object, frontier = c("both", "lower", "upper"), 
    return = "mean", risk = "cov")
}



\arguments{

    \item{object}{
        an object of class \code{fPORTFOLIO}.
        }
    \item{frontier}{
        a character string denoting which part of the
        efficient portfolio should be extractacted.
        }
    \item{return, risk}{
        character strings denoting which return and risk 
        has to be plotted.
        }

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## ---
}


\keyword{models}

