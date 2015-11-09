\name{rank}


\title{Sample Ranks of a Time Series}


\alias{rank,timeSeries-method}


\description{
    
    Returns the sample ranks of the values of a 
    'timeSeries' object.
      
}


\details{

    If all components are different (and no NAs), the ranks are well 
    defined, with values in \code{seq_len(x)}. With some values equal 
    (called ???ties???), the argument ties.method determines the result at 
    the corresponding indices. The \code{"first"} method results in a 
    permutation with increasing values at each index set of ties. The 
    \code{"random"} method puts these in random order whereas the 
    default, \code{"average"}, replaces them by their mean, and 
    \code{"max"} and \code{"min"} replaces them by their maximum 
    and minimum respectively, the latter being the typical sports 
    ranking.

    NA values are never considered to be equal: for \code{na.last = TRUE}
    and \code{na.last = FALSE} they are given distinct ranks in the order 
    in which they occur in \code{x}.
}


\usage{
    \S4method{rank}{timeSeries}(x, na.last = TRUE, ties.method = )
}


\arguments{
        
     \item{x}{
         an univariate object of class \code{timeSeries}.
         }
     \item{na.last}{
         for controlling the treatment of NAs. If TRUE, missing values 
         in the data are put last; if FALSE, they are put first; if NA, 
         they are removed; if "keep" they are kept with rank NA. 
         }
     \item{ties.method}{
         a character string specifying how ties are treated; can be 
         abbreviated.
         }  
       
}


\value{

    returns the ranks of a \code{timeSeries} object.
        
}


\examples{
## Load Microsoft Data -
   X <- 100 * returns(MSFT)

## Compute the Ranks -
   head(rank(X[, "Open"]), 10)
   
## Only Interested in the Vector, then use -
   head(rank(series(X[, "Open"])), 10)
}


\keyword{chron}

