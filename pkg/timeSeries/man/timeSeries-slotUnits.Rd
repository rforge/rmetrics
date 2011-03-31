\name{units}


\alias{getUnits}
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
        a \code{timeSeries} object.
        }
    \item{value}{
        a \code{vector}, a \code{data.frame} or a \code{matrix} object of
        numeric data. 
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

