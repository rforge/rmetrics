\name{monthly}


\alias{monthly}

\alias{countMonthlyRecords}
\alias{rollMonthlyWindows}
\alias{rollMonthlySeries}


\title{Special Monthly Series}


\description{
    
    Functions and methods dealing with special monthly 
    'timeSeries' objects.      
}


\details{
  
    The function \code{countMonthlyRecords} computes a 'timeSeries' 
    that holds the number of monthly counts of the records.
    
    The function \code{rollMonthlyWindows} computes start and end 
    dates for rolling time windows.
    
    The function \code{rollMonthlySeries} computes a static over
    rolling periods defined by the function \code{rollMonthlyWindows}.

}


\usage{   
countMonthlyRecords(x)

rollMonthlyWindows(x, period = "12m", by = "1m") 
rollMonthlySeries(x, period = "12m", by = "1m", FUN, \dots)
}


\arguments{

    \item{x}{
        a 'timeSeries' object.
        }  
    \item{period}{
        a character string specifying the rollling period composed by 
        the length of the period and its unit. As examples: \code{"3m"} 
        represents quarterly shifts, and \code{"6m"}, ]code{"12m"}, and
        \code{"24m"} semi-annual, annual and bi-annual shifts. To 
        determine the proper start of the series is in the responsibility
        of the user.
        } 
    \item{by}{
        a character string specifying the rolling shift composed by 
        the length of the shift and its unit. As examples:  \code{"1m"} 
        represents monthly shifts, \code{"3m"} represents quarterly 
        shifts, and \code{"6m"} semi-annual shifts. To determine the
        proper start of the series is in the responsibility of the
        user.
        }
    \item{FUN}{ 
        the function for the statistic to be applied. For example
        in the case of aggregation use\code{colAvgs}.
        }
    \item{\dots}{
        arguments passed to the function \code{FUN}.
        }

}


\value{

    The function \code{countMonthlyRecords} returns a 'timeSeries' 
    object.
    
    The function \code{rollMonthlyWindows} returns a list with two
    named 'tomeDate' entries: \code{$from} and \code{to}. An attribute
    \code{"control"} is added which keeps the \code{start} and
    \code{end} dates of the series. 
    
    The function \code{rollMonthlySeries} computes the statistics
    defined by the function \code{FUN} over a rolling window
    internally computed by the function \code{rollMonthlyWindows}.
    Note, the periods may be overlapping, may be dense, or even
    may have gaps.
        
}


\examples{
## Load Microsoft Daily Data Set:
   x <- MSFT 
   
## Count Monthly Records -
   counts <- countMonthlyRecords(x)
   counts
   
## Quaterly Non-Overlapping Time Periods -
   windows <- rollMonthlyWindows(counts[-1, ], period = "3m", by = "3m") 
   windows
   
## Nicely Reprint Results as a data.frame -
   data.frame(cbind(FROM=format(windows$from), TO=format(windows$to)))
   
## Compute the average number of monthly trading days per quarter -  
   rollMonthlySeries(counts[-1, ], period = "3m", by = "3m", FUN=mean)
}


\keyword{chron}

