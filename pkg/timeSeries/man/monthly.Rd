\name{monthly}


\alias{monthly}

\alias{countMonthlyRecords}
\alias{isMonthly}

\alias{rollMonthlyWindows}
\alias{rollMonthlySeries}


\title{Special Monthly Series}


\description{
    
    Functions and methods dealing with special monthly 
    'timeSeries' objects.      
    \cr  
    
    \tabular{ll}{   
    \code{countMonthlyRecords} \tab Returns a series with monthly counts of records, \cr
    \code{isMonthly} \tab Decides if the series consists of monthly records, \cr
    \code{rollMonthlyWindows} \tab Returns start and end dates for rolling time windows, \cr        
    \code{rollMonthlySeries} \tab Rolls monthly a 'timeSeries' on a given period. }
    
}


\usage{   
countMonthlyRecords(x)
isMonthly(x)

rollMonthlyWindows(x, period = "12m", by = "1m") 
rollMonthlySeries(x, period = "12m", by = "1m", FUN, \dots)
}


\arguments{

    \item{by}{
        a character string specifying the rolling shift composed by the
        length of the shift and its unit, e.g. \code{"3m"} represents 
        quarterly shifts.
        }
    \item{FUN}{ 
        the function to be applied.\cr
        [applySeries] - \cr
        a function to use for aggregation, by default \code{colAvgs}.
        }
    \item{period}{
        [rollMonthlySeries] - \cr
        a character string specifying the rollling period composed by the
        length of the period and its unit, e.g. \code{"12m"} represents 
        one year.
        } 
    \item{x}{
        an object of class \code{timeSeries}.
        }  
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

    NA
        
}


\examples{
## Load Microsoft Daily Data Set:
   data(MSFT) 
   
## Count Monthly Records -
   MSFT.CTS <- countMonthlyRecords(MSFT)
   MSFT.CTS
   
## Show that this is a Monthly Series - 
   isMonthly(MSFT.CTS)
}


\keyword{chron}

