\name{midnightStandard}


\title{Midnight Standard}


\alias{midnightStandard}


\description{
    
    Corrects 'timeDate' objects if they do not fulfill the
    ISO8601 midnigth standard.
       
}


\usage{
midnightStandard(charvec, format)  
}


\arguments{
  
    \item{charvec}{
        a character string or vector of dates and times.
        }
    \item{format}{
        the format specification of the input character vector.
        }
       
}


\value{
    
    returns a \code{timeDate} object.
    
}


\examples{
## midnightStandard -  
   midnightStandard("2007-12-31 24:00")
}


\keyword{chron}

