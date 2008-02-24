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


\seealso{

    \code{\link{dayOfYear}}
}



\examples{  
## timeCalendat -  
   tC = timeCalendar()
       
## The days of the Year:
   dayOfWeek(tC)
}


\keyword{chron}

