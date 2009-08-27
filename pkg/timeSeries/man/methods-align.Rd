\name{align-methods}

\docType{methods}
\alias{align-methods}
\alias{align,timeSeries-method}


\title{timeSeries Class, Functions and Methods}

\description{
    
    Aligns a 'timeSeries' Object.      
    
}


\usage{
\S4method{align}{timeSeries}(x, by = "1d", offset = "0s",
    method = c("before", "after", "interp", "fillNA"),
    include.weekends = FALSE)

}


\arguments{
  
    \item{x}{
        an object of class \code{timeSeries}.
        }
    \item{by}{
        a character string denoting the period
        }
    \item{offset}{
        a character string denoting the offset
        }
    \item{method}{
        a character string denoting the alignment approach.
        }                
    \item{include.weekends}{
        a logical flag, should weekend be included.
        }
       
}


\value{

    returns an aligned S4 object of class \code{timeSeries}.
        
}


\examples{
## Load Microsoft Dataset - 
   data(MSFT)
   
## Compute Sample Size - 
   dim(MSFT)
   
## Align the Series - 
   MSFT.AL = align(MSFT) 
   
## Show the Size of the Aligned Series - 
   dim(MSFT.AL)
}


\keyword{methods}
\keyword{chron}

