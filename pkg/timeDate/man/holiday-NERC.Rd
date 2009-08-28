\name{holidayNERC}


\title{NERC Holiday Calendar}


\alias{holidayNERC}


\description{
    
    Returns a holiday calendar for NERC, the 
    North American Reliability Council.     
                
}


\usage{
holidayNERC(year = getRmetricsOptions("currentYear"), FinCenter = "Eastern")
}


\arguments{

    \item{year}{
        an integer value or vector of years, formatted as \code{YYYY}.
        }
    \item{FinCenter}{
        a character value, the name of the financial center to use.
        }
        
}


\value{

    returns an object of class \code{timeDate}. 

}


\author{
    Joe W. Byers
}


\references{
    http://www.nerc.com/~oc/offpeaks.html
}


\examples{
## holidayNERC -
   holidayNERC()
   holidayNERC(2008:2010)
}

\keyword{chron}

