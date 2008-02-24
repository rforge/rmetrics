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


\details{

    The Zurich holiday calendar includes the following holidays:
    NewYearsDay, GoodFriday, EasterMonday, 
    LaborDay, PentecostMonday, ChristmasDay, 
    BoxingDay, CHBerchtoldsDay, CHSechselaeuten, 
    CHAscension, CHConfederationDay, CHKnabenschiessen.

}


\examples{
## holidayZURICH -
   holidayZURICH()
   holidayZURICH(2008:2010)
}


\keyword{chron}

