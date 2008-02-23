\name{c}


\title{Concatenating 'timeDate' Objects}


\alias{c}
\alias{c.timeDate}


\description{
    
    Concatenates 'timeDate' objects.
       
}


\usage{
\method{c}{timeDate}(\dots, recursive = FALSE)
}


\arguments{
      
    \item{recursive}{
        a logical. If recursive  is set to \code{TRUE}, the function 
        recursively descends through lists combining all their elements 
        into a vector. 
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
   
## c - 
   # Concatenate and Replicate timeDate Objects:
   c(GMT[1:2], ZUR[1:2])
   c(ZUR[1:2], GMT[1:2]) 
}


\keyword{chron}

