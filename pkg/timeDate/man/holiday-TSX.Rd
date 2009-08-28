\name{holidayTSX}


\title{TSX Holiday Calendar}


\alias{holidayTSX}


\description{
    
    Returns a holiday calendar for the Toronto Stock Exchange.     
                
}


\usage{
holidayTSX(year = getRmetricsOptions("currentYear"))
}


\arguments{

    \item{year}{
        an integer value or vector of years, formatted as \code{YYYY}.
        }
        
}


\value{

    returns an object of class \code{timeDate}. 

}


\examples{
## holidayTSX -
   holidayTSX()
   holidayTSX(2008:2010)
}

\keyword{chron}