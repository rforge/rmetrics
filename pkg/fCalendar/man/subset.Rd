\name{subset}


\alias{subset}
\alias{[.timeDate}


\title{Subsetting a 'timeDate' Object}


\description{
    
    Extracts or replaces subsets from 'timeDate' objects.
       
}


\usage{
\method{[}{timeDate}(x, \dots, drop = TRUE)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }
    \item{drop}{
        a logical flag, by default \code{TRUE}.
        }         
}


\value{
   
    returns an object of class \code{timeDate}.

}


\examples{  
## timeCalendar -
   tS = timeCalendar()
 
## [ - 
   # Subsetting Second Quarter:
   tS[4:6] 
   
## [<-
   # Replacing:  
}


\keyword{chron}

