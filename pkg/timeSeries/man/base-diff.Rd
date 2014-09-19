\name{diff}                     
                                    
                                    
\title{diff}


\alias{diff}
\alias{diff,timeSeries-method}


\description{

    Differences a 'timeSeries' 0bject.
    
}


\usage{
diff(x, \dots)
}

% lag=1, diff=1, trim=FALSE, pad=NA,


\arguments{
  
    \item{x}{
      an object of class 'timeSeries'.
      }
   
    \item{\dots}{
      further arguments to be passed. These may include
      }
      
}


\details{

    Arguments to be passed may include:\cr
    
    \code{lag} - an integer indicating which lag to use. 
        By default 1.\cr
    \code{diff} - an integer indicating the order of the difference. 
        By default 1.\cr
    \code{trim} - a logical flag. Should NAs at the beginning of the 
      series be removed? By default FALSE.\cr
    \code{pad} - a umeric value with which NAs should be replaced at 
      the beginning of the series. By default NA.

}

 
\value{

      Returns a differenced S4 'timeSeries' object.

}


\examples{
## Load Microsoft Data Set -
   x <- MSFT[1:12, ]
   x

## Compute Differences -
   diff(x)   
   
## Trimmed Differences -
   diff(x, trim=TRUE) 

## Padded Differences -
   diff(x, trim=FALSE, pad=0) 
}


\keyword{chron}


 
