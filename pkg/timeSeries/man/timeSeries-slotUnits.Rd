\name{units}


\alias{getUnits}
\alias{getUnits.default}
\alias{setUnits<-}


\title{Get and Set Unit Names of a 'timeSeries'}


\description{

    Gets and sets the column names of a 'timeSeries' object. The
    column names are also called units or unit names.
    
}


\usage{
getUnits(x)         
setUnits(x) <- value
}


\arguments{

    \item{x}{
        a 'timeSeries' object.
        }
    \item{value}{
        a \code{vector} of unit names.
        }
}


\seealso{timeSeries()}


\examples{
## A Dummy timeSeries Object
   tS <- dummySeries()
   tS

## Get the Units - 
   getUnits(tS)

## Assign New Units to the Series - 
   setUnits(tS) <- c("A", "B")
   head(tS)
}


\keyword{programming}

