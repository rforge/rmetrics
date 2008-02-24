\name{holidayNYSE}


\title{Zurich Holiday Calendar}


\alias{holidayNYSE}


\description{
    
    Returns a holiday calendar for the New York Stock Exchange.     
                
}


\usage{
holidayNYSE(year = currentYear)
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
## holidayNYSE -
   holidayNYSE()
   holidayNYSE(2008:2010)
}

\keyword{chron}

