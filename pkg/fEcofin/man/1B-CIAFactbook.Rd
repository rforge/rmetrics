\name{CIAFactbook}

\alias{CIAFactbook}

\alias{ciaCountries}
\alias{ciaIndicators}

\alias{print.ciaCountries}
\alias{print.ciaIndicators}

\alias{ciaByCountry}
\alias{ciaByIndicator}

\alias{ciaByCountry}
\alias{ciaByIndicator}

%DATA

\alias{ciaFactbook}
            

\title{CIA Factbook}


\description{
    
    A collection and description of functions to extract 
    financial and economic market statistics from the
    data available in the CIA World Factbook.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{ciaCountries} \tab Returns a list of CIA country codes, \cr
    \code{ciaIndicators} \tab Returns a list of CIA indicator codes, \cr
    \code{ciaByCountry} \tab Returns all Indicators by country, \cr 
    \code{ciaByIndicator} \tab Returns for all countries indicator ranking. }
 
}


\usage{
ciaCountries()
ciaIndicators()

ciaByCountry(code = "CH", from = FALSE, names = FALSE, details = FALSE)
ciaByIndicator(code = 2001, from = FALSE, details = FALSE)

\method{print}{ciaCountries}(x, \dots)
\method{print}{ciaIndicators}(x, \dots)
}


\arguments{

    \item{code}{
        [ciaByCountry] - \cr
        a character string denoting the country code.
        \cr
        [ciaByIndicator] - \cr
        a character string or integer denoting the indicator code.
        }
    \item{details}{
        a logical flag. Should details be printed? By default 
        \code{FALSE}.
        }
    \item{from}{
        a logical flag. If set to \code{TRUE} an additional column
        will be returned with the information when the data were
        recorded.
        }
    \item{names}{
        a logical flag. If set to \code{TRUE"} then the full names
        of the countries will be returned in an additional column
        }
    \item{x}{
        x an object of class \code{ciaCountries} or 
        \code{ciaIndicators} as returned by the functions 
        \code{ciaCountry} or \code{ciaIndicator}, respectively.
        }
    \item{\dots}{
        arguments to be past to the \code{print} method.
        }
 
}


\value{

    \code{ciaCountries}
    \cr
    returns a data frame with countries and contry codes.
    \cr
    
    \code{ciaIndicators}
    \cr
    returns a data frame with indicator codes.
    \cr

    \code{ciaByCountry}
    \cr
    returns a data frame with indicators by country.
    \cr
    
    \code{ciaByIndicator}
    \cr
    returns a data frame with ranked data for a given indicator.
    
}


\references{

CIA, 2004, 
    \emph{CIA Factbbook 2004},
    http://www.cia.gov/cia/publications/factbook.

}
    
    
\examples{
## SOURCE("fEcofin.1B-CIAFactbook")

## Pie Chart from CIA Oil Production Indicator (Code 2173): 
   # Search for Code:
   ciaIndicators()
   # Create Pie Chart:
   OilProduction = as.integer(as.vector(ciaByIndicator(2173)[2:11, 2]))
   names(OilProduction) = as.vector(ciaByIndicator(2173)[2:11,1])
   print(OilProduction) 
   pie(OilProduction, col = rainbow(10))
   title(main = "Oil Production 2004\n bbl/day")
   mtext("Source: CIA World Factbook", side = 1)
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\keyword{data}

