\name{Sys.timeDate}


\title{System time as 'timeDate' object}


\alias{Sys.timeDate}


\description{

    Returns system time as an object of class 'timeDate'.

}


\usage{
Sys.timeDate(FinCenter = myFinCenter)
}


\arguments{

    \item{FinCenter}{
        a character with the the location of the
        financial center named as "continent/city".
        }

}


\value{

    returns the system time as class \code{"timeDate"} object.

}



\examples{
## Sys.time -

   # direct
   Sys.timeDate()  
   
   # transformed from "POSIX(c)t"         
   timeDate(Sys.time()) 
 
   # Local Time in Zurich
   timeDate(Sys.time(), FinCenter = "Zurich")  
}


\keyword{chron}

