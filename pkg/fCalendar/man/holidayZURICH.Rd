\name{holidayZURICH}


\title{Zurich Holiday Calendar}


\alias{holidayZURICH}


\description{
    
    Returns a holiday calendar for Zurich.     
                
}


\usage{
holidayZURICH(year = currentYear)
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
## holidayZurich -
   holidayZurich()
   holidayZurich(2008-2010)
}

\keyword{chron}

