\name{MarketStatistics}

\alias{MarketStatistics}

\alias{ciaCountries}
\alias{ciaIndicators}

\alias{print.ciaCountries}
\alias{print.ciaIndicators}

\alias{ciaByCountry}
\alias{ciaByIndicator}

\alias{ciaByCountry}
\alias{ciaByIndicator}
            

\title{CIA Factbook}


\description{
    
    A collection and description of functions to extract 
    financial and economic market statistics from the
    data available in the CIA World Factbook and from the
    exchange data collected by the World Federation of 
    Stock Exchanges.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{ciaCountries} \tab Returns a list of CIA country codes, \cr
    \code{ciaIndicators} \tab Returns a list of CIA indicator codes, \cr
    \code{ciaByCountry} \tab Returns all Indicators by country, \cr 
    \code{ciaByIndicator} \tab Returns for all countries indicator ranking. }
    
    To load statistics from the WFE:
    
    \tabular{ll}{
    \code{data(wfe1)} \tab Market capitalization of domestic companies, \cr
    \code{data(wfe2)} \tab Total number of companies with shares listed, \cr
    \code{data(wfe3)} \tab Total value of share trading, \cr 
    \code{data(wfe4)} \tab Market value of bonds listed, \cr
    \code{data(wfe5)} \tab Total value of bond trading, and \cr
    \code{data(wfe6)} \tab Price earning ratio an gross dividend yield. }
    
}


\usage{
ciaCountries()
ciaIndicators()

ciaByCountry(code = "CH", from = FALSE, names = FALSE, details = TRUE)
ciaByIndicator(code = 2001, from = FALSE, details = TRUE)

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
        \code{TRUE}.
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


\details{

    Financial and economic market statistics can be found at several 
    web pages for free. The "OECD Factbook" from the 'Organisation for
    Economic Co-operation and Development', \emph{www.oecd.org},
    "The World Factbook" from the 'Central Intelligence Agency'
    of the US, \emph{www.cia.gov}, and the "Penn World Tables" from
    the 'Center for International Comparisons' at University of
    Pennsylvania, \emph{pwt.econ.upenn.edu}, offer sources of economic,
    environmental and social indicators for the world's core economies.
    
    Statistical data from the exchanges around the world can be obtained
    from the 'World Federation of Stock Exchanges', \emph{www.fibv.com}.
    
    Further sources of statistical data can be found on the web pages of
    the 'Bank for International Settlement', \emph{www.bis.org}, and
    on the web pages of the 'International Monetary Fund', \emph{www.imf.org}.
    
}


\references{

CIA, 2004, 
    \emph{CIA Factbbook 2004},
    http://www.cia.gov/cia/publications/factbook.
    
WFE, 2004,
    \emph{World Federation of Stock Exchanges, Focus 2004},
    http://www.world-exchanges.org.

}
    
    

\examples{
## SOURCE("fBasics.011A-MarketStatistics")

## Pie Chart from CIA Oil Production Indicator (Code 2173): 
   # Search for Code:
   ciaIndicators()
   # Create Pie Chart:
   OilProduction = as.integer(as.vector(ciaByIndicator(2173)[2:11, 2]))
   names(OilProduction) = as.vector(ciaByIndicator(2173)[2:11,1])
   OilProduction 
   pie(OilProduction,col = rainbow(10))
   title(main = "Oil Production 2004\n bbl/day")
   mtext("Source: CIA World Factbook", side = 1)

## Barplot from WFE Capitalization Statistics:
   # Extract Capitalization of/at:
   # NYSE: 7, Tokyo: 37, London: 22, Frankfurt: 15
   # 1991 - 2003 triannual: 3,6,9,12,15
   data(wfe1)
   Table =t(wfe1[c(7,37,22,15),c(3,6,9,12,15)])/1e6
   colnames(Table) = c("NewYork", "Tokyo", "London", "Frankfurt")
   rownames(Table) = as.character(seq(1991, 2003, by = 3))
   Table
   # Create Barplot:
   barplot(Table, beside = TRUE, legend = rownames(Table),
     col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"))
   title(main = "Stock Market Capitalization\n 1991 - 2003")
   mtext("Source: World Federation of Exchanges", side = 4, 
     line = -2, cex = 0.7) 
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\keyword{data}

