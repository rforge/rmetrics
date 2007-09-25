\name{TimeSeriesData}

\alias{TimeSeriesData}

\alias{fapply}


\title{timeSeries Class, Functions and Methods}


\description{
    
    A collection and description of functions 
    and methods dealing with regular and irregular 
    'timeSeries' objects. Dates and times are 
    implemented as 'timeDate' objects. Included 
    are functions and methods for the generation 
    and representation of 'timeSeries' objects, 
    and for mathematical operations.          
    
}


\usage{
fapply(x, from, to, FUN, \dots)
}


\arguments{
  
    \item{from, to}{
        starting date and end date, \code{to} must be after \code{from}.
        }
    \item{FUN}{
        the function to be applied.
        }
    \item{x}{
        an object of class \code{timeSeries}.
        } 
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{
    
    \code{end, start}
    \cr
    return a S4 object of class \code{timedate}. These are the 
    start and end dates of a \code{timeSeries} object.
    \cr
    
    \code{as.vector}\cr
    \code{as.matrix}\cr
    \code{as.data.frame}
    \cr
    these are methods which convert a S4 object of class \code{timeSeries}
    either to a vector, a matrix or to a data frame.
    \cr
    
    \code{plot}\cr
    \code{lines}\cr
    \code{points}\cr
    \code{print}
    \cr
    plot and print methods for an object of class \code{timeSeries}.
    Note that the plot function requires the packages \code{its} and
    \code{Hmisc}.
    \cr
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data - 

}


\keyword{chron}

