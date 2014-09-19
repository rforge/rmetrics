\name{sort}  


\alias{sort,timeSeries-method}
\alias{sort.timeSeries}                 
                                    
                                    
\title{Sorting a 'timeSeries' by Time Stamps}


\description{

    Sorts a 'timeSeries' object with respect to its time stamps.
    
}


\details{

    Sorts a time series either in increasing or decreasing time 
    stamp order. Internally the function \code{order} from R's
    base packahe is used. \code{order} generates a permutation 
    which rearranges the time stamps in ascending or descending 
    order.
    
    To find out if the series is unsorted, the function
    \code{is.unsorted} from R's base package can be called.

}


\usage{
\S4method{sort}{timeSeries}(x, decreasing = FALSE, \dots)
}


\arguments{

    \item{x}{
        an uni- or multivariate \code{timeSeries} object.
        }
    \item{decreasing}{
        a logical flag. Should we sort in increasing or 
        decreasing order?  By default FALSE.
        }
    \item{\dots}{
        optional arguments passed to other methods.
        }
}


\value{

    Returns a sorted 'timeSeries' object, which can be
    increasing or decreasing in time.
           
}


\examples{   
## Monthly Calendar Series -    
   x <- daily2monthly(LPP2005REC[, 1:2])[3:14, ]
   
## Resample the Series with respect to the time stamps -    
   resampled <- sample(x)
   resampled
   is.unsorted(resampled)
   
## Now sort the serie in decreasing time order - 
   sorted <- sort(resampled, , decreasing = TRUE)
   sorted
   is.unsorted(sorted)
   
## Is the reverted series ordered? - 
   reverted <- rev(sorted)
   reverted
   is.unsorted(reverted)
}


\keyword{chron}

