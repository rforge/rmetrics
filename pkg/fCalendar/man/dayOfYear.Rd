\name{dayOfYear}


\title{The Day of the Year}


\alias{dayOfYear}


\description{
    
    returns the day of the year from a 'timeDate' object.
      
}


\usage{
dayOfYear(x)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }  
       
}


\value{


    returns the day count as integer value starting January, 1st. 

}


\seealso{

    \code{\link{dayOfWeek}}
}


\note{

    With version 2.7 the function has been renamed from \code{getdayOfYear}.
    
}



\examples{  
## timeCalendat -  
   tC = timeCalendar()
       
## The days of the Year:
   dayOfYear(tC)
   
## Use Deprecated Function:
   getDayOfYear <- dayOfYear
   getDayOfYear(tC)
}


\keyword{chron}

