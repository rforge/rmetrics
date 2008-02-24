\name{dayOfWeek}


\title{The Day of the Year}


\alias{dayOfWeek}


\description{
    
    returns the day of the year from a 'timeDate' object.
      
}


\usage{
dayOfWeek(x)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }  
       
}


\value{


    returns a three letter character string with the names in English
    of the day of the week,

}


\note{

    With version 2.7 the function has been renamed from \code{getdayOfWeek}.
    
}


\seealso{

    \code{\link{dayOfYear}}
}



\examples{  
## timeCalendat -  
   tC = timeCalendar()
       
## The days of the Year:
   dayOfWeek(tC)
   
## Use Deprecated Function:
   getDayOfWeek <- dayOfWeek
   getDayOfWeek(tC)
}


\keyword{chron}

