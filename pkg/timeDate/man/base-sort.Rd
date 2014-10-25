\name{sort}


\title{Sorting 'timeDate' Objects}


\alias{sort}
\alias{sort.timeDate}


\description{
    
    Sorts a 'timeDate' object.
       
}


\usage{
\method{sort}{timeDate}(x, \dots)
}


\arguments{
     
    \item{x}{
        an object of class \code{timeDate}.
        }  
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{
    
    returns an object of class \code{"timeDate"}.

}



\examples{
## c -
   # Create Character Vectors:
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   dts
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
   tms
   
## "+/-" - 
   # Add One Day to a Given timeDate Object:
   GMT = timeDate(dts, zone = "GMT", FinCenter = "GMT")
   GMT
   ZUR = timeDate(dts, zone = "GMT", FinCenter = "Europe/Zurich")
   ZUR
   
## c - 
   # Concatenate and Replicate timeDate Objects:
   c(GMT[1:2], ZUR[1:2])
   c(ZUR[1:2], GMT[1:2])
   
## rep - 
   rep(ZUR[2], times = 3)
   rep(ZUR[2:3], times = 2)  
}


\keyword{chron}

