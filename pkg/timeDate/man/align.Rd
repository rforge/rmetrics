\name{align}


\title{Making a 'timeDate' object unique}


\alias{align}
\alias{align,timeDate-method}


\description{
    
    Aligns a 'timeDate' object to regular date/time stamps.
       
}


\usage{
\S4method{align}{timeDate}(x, by = "1d", offset = "0s")
}


\arguments{
      
    \item{x}{
        an object of class \code{timeDate}.
        }  
    \item{by}{
        by a character string formed from an integer length and
        a period identifyer. Valid values are 
        "w", "d", "h", "m", "s", for 
        weeks, days, hours, minutes and seconds. For example
        a bi-weekly period is expressed as \code{"2w"}.
        }
    \item{offset}{
        by a character string formed from an integer length and
        a period identifyer in the same way as  for the argument
        \code{by}.
        }
       
}


\value{
    
    returns an object of class \code{timeDate}.

}


\examples{
## align -

   # Align Bi-Weekly with a 3 Days Offset
   tC = timeCalendar()
   tC
   # align(tC, by = "2w", offset = "3d")  
}


\keyword{chron}

