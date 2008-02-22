\name{TimeDateCoercion}


\title{timeDate Class, Coercion and Transformation}


\alias{TimeDateCoercion}

\alias{as.timeDate}
\alias{as.timeDate.default}
\alias{as.timeDate.POSIXt}
\alias{as.timeDate.Date}


\description{
    
    A collection and description of functions and 
    methods for coercion and transformation of 
    objects of class 'timeDate'.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{as.timeDate.POSIXt} \tab Returns a 'POSIX' object as 'timeDate' object, \cr
    \code{as.timeDate.Date} \tab Returns a 'POSIX' object as 'timeDate' object, \cr
    \code{as.character.timeDate} \tab Returns a 'timeDate' object as character string, \cr
    \code{as.data.frame.timeDate} \tab Returns a 'timeDate' object as data frame, \cr
    \code{as.POSIXct.timeDate} \tab Returns a 'timeDate' object as POSIXct object. } 
       
}


\usage{
\method{as.timeDate}{default}(x, zone = myFinCenter, FinCenter = myFinCenter)
\method{as.timeDate}{POSIXt}(x, zone = myFinCenter, FinCenter = myFinCenter)
\method{as.timeDate}{Date}(x, zone = myFinCenter, FinCenter = myFinCenter) 
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }
    \item{FinCenter}{
        a character with the the location of the  
        financial center named as "continent/city". 
        }
  
}


\value{
  
    \code{as.timeDate.POSIXt} returns an object of class \code{POSIXt}.
    
    \code{as.timeDate.Date} returns an object of class \code{Date}.
    
}


\examples{
## timeDate - 
   tD = timeDate()
   tC = timeCalendar()
   
## 
}


\keyword{chron}

