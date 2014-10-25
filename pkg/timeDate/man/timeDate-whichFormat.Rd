\name{whichFormat}


\title{Format Recognation}


\alias{whichFormat}


\description{
    
    Trys to recognize the date/time format.
       
}


\usage{
whichFormat(charvec, silent = FALSE)  
}


\arguments{
  
    \item{charvec}{
        a character string or vector of dates and times.
        }
    \item{silent}{
        a logical flag. Should a warning be printed if the
        format cannot be recognized?
        }
       
}


\value{
    
    returns a format string.
    
}


\examples{
## midnightStandard -  
   whichFormat("2007-12-31 24:00")
}


\keyword{chron}

