\name{finCenter}


\title{Get and Set Financial Center of a 'timeSeries'}


\alias{getFinCenter}  
\alias{setFinCenter<-}

\alias{finCenter,timeSeries-method}
\alias{finCenter<-,timeSeries-method}


\description{

    Print or assign new financial center to a 'timeSeries' object.
    
}


\usage{
getFinCenter(x)         
setFinCenter(x) <- value

\S4method{finCenter}{timeSeries}(x)
\S4method{finCenter}{timeSeries}(x) <- value
}


\arguments{
  
    \item{x}{
        a 'timeSeries' object.
        }
    \item{value}{
        a character with the the location of the financial center named 
        as "continent/city".
        }
}


\seealso{listFinCenter}


\examples{
## An artificial timeSeries Object - 
   tS <- dummySeries()
   tS

## Print Financial Center -
   finCenter(tS)
   getFinCenter(tS)

## Assign New Financial Center - 
   finCenter(tS) <- "Zurich"
   tS
   setFinCenter(tS) <- "New_York"
   tS
}


\keyword{programming}

