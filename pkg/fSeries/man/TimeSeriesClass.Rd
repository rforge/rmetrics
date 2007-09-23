\name{TimeSeriesClass}
\title{timeSeries Class}

\alias{TimeSeriesClass}

\alias{timeSeries}
\alias{timeSeries-class}

\alias{readSeries}

\alias{applySeries}

\alias{orderStatistics}

\alias{seriesData}
\alias{isUnivariate}
\alias{isMultivariate}

\alias{rollMonthlyWindows}
\alias{rollMonthlySeries}

\description{
    
    A collection and description of functions 
    and methods dealing with regular and irregular 
    'timeSeries' objects. Dates and times are 
    implemented as 'timeDate' objects. Included 
    are functions and methods for the generation 
    and representation of 'timeSeries' objects, 
    and for mathematical operations.          
    \cr
    
    Functions to generate and modify 'timeSeries' objects:         

    \tabular{ll}{   
    \code{timeSeries} \tab Creates a 'timeSeries' object from scratch, \cr
    \code{readSeries} \tab Reads a 'timeSeries' from a spreadsheet file, \cr
    \code{applySeries} \tab Applies a function to margins of a 'timeSeries', \cr
    \code{orderStatistics} \tab Computes order statistic of a 'timeSeries'. }
    
    Data Slot and classification of 'timeSeries' objects: 
    
    \tabular{ll}{        
    \code{seriesData} \tab Extracts data slot from a 'timeSeries', \cr
    \code{isUnivariate} \tab Tests if a 'timeSeries' object is univariate, \cr
    \code{isMultivariate} \tab Tests if a 'timeSeries' object is multivariate. }          
   
    Special Functions for Monthly 'timeSeries' objects:  
    
    \tabular{ll}{   
    \code{rollMonthlyWindows} \tab Returns start and end dates for rolling time windows, \cr        
    \code{rollMonthlySeries} \tab Rolls monthly a 'timeSeries' on a given period. }
    
}


\usage{
timeSeries(data, charvec, units = NULL, format = NULL, zone = myFinCenter, 
    FinCenter = myFinCenter, recordIDs = data.frame(), title = NULL, 
    documentation = NULL, ...)   
readSeries(file, header = TRUE, sep = ";", zone = myFinCenter, 
    FinCenter = myFinCenter, title = NULL, documentation = NULL, \dots)
 
applySeries(x, from = NULL, to = NULL, by = c("monthly", "quarterly"), 
    FUN = colAvgs, units = NULL, format = x@format, zone = x@FinCenter, 
    FinCenter = x@FinCenter, recordIDs = data.frame(), title = x@title,
    documentation = x@documentation, \dots)
    
orderStatistics(x)

seriesData(object)
isUnivariate(x)
isMultivariate(x)
    
rollMonthlyWindows(x, period = "12m", by = "1m") 
rollMonthlySeries(x, period = "12m", by = "1m", FUN, \dots)
}


\arguments{

    \item{by}{
        [applySeries] - \cr
        a character either \code{"monthly"} or \code{"quarterly"}. The
        default value is \code{"monthly"}. Only operative when both arguments
        \code{from} and \code{to} have ther default values \code{NULL}.
        In this case the function \code{FUN} will be applied to monthly or
        quarterly periods.
        [rollMonthlySeries] - \cr
        a character string specifying the rolling shift composed by the
        length of the shift and its unit, e.g. \code{"3m"} represents 
        quarterly shifts.
        }
    \item{charvec}{
        a character vector of dates and times.
        }
    \item{data}{
        a \code{data.frame} or a \code{matrix} object of numeric data.
        }
    \item{documentation}{
        optional documentation string, or a vector of character strings.
        }
    \item{file}{
        the filename of a spreadsheet data set from which
        to import the data records.
        }
    \item{FinCenter}{
        a character with the the location of the  financial center named 
        as "continent/city". 
        }
    \item{header}{
        a logical value indicating whether the file contains the
        names of the variables as its first line.  If missing, the
        value is determined from the file format: 'header' is set to
        'TRUE' if and only if the first row contains one fewer field
        than the number of columns.
        }
    \item{format}{
        the format specification of the input character vector,
        \cr
        [as.timeSeries] - \cr
        a character string with the format in POSIX notation to be passed 
        to the time series object.
        }  
    \item{from, to}{
        starting date and end date, \code{to} must be after \code{from}.
        }
    \item{FUN}{ 
        the function to be applied.\cr
        [applySeries] - \cr
        a function to use for aggregation, by default \code{colAvgs}.
        }
    \item{object}{
        [is][seriesData][seriesPositions][show][summary] -    
        an object of class \code{timeSeries}.
        }
    \item{period}{
        [rollMonthlySeries] - \cr
        a character string specifying the rollling period composed by the
        length of the period and its unit, e.g. \code{"12m"} represents 
        one year.
        }
    \item{recordIDs}{
        a data frame which can be used for record identification
        information. \cr
        [print] - \cr
        a logical value. Should the \code{recordIDs} printed together
        with the data matrix and time series positions?
        }
    \item{sep}{
        [readSeries] - \cr
        the field seperator used in the spreadsheet file to separate 
        columns.
        }
    \item{title}{
        an optional title string, if not specified the inputs data 
        name is deparsed.
        }
    \item{units}{
        [applySeries][lag][returnSeries][mergeSeries] - \cr
        an optional character string, which allows to overwrite the 
        current column names of a \code{timeSeries} object. By default 
        \code{NULL} which means that the column names are selected
        automatically.
        \cr
        [durationSeries] - \cr
        a character value or vector which allows to set the 
        units in which the durations are measured. By default durations
        are measured in seconds. 
        } 
    \item{x}{
        [as] - \cr   
        a \code{matrix} type object to be converted.\cr
        [as.vector][as.matrix][as.data.frame] -\cr
        [applySeries] - \cr
        [cut][end][mergeSeries][plot][print][rev][start] - \cr
        an object of class \code{timeSeries}.
        }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }  
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

    \code{timeSeries}\cr
    \code{readSeries}\cr
    \code{returnSeries}\cr
    \code{applySeries}
    \cr
    return a S4 object of class \code{timeSeries}.
    \cr
    
    \code{orderStatistics}
    \cr
    returns ...
    \cr
    
    \code{seriesData}\cr
    \cr
    extracts the \code{@Data} slot from a \code{timeSeries} object. 
    Thus, \code{seriesData} returns an object of class \code{matrix}.
    \cr
    
    \code{isUnivariate}\cr
    \code{isMultivariate}\cr
    \cr
    returns a logical depending if the test is true or not.
    \cr
    
    \code{plot}\cr
    \code{lines}\cr
    \code{points}\cr
    \code{print}
    \cr
    plot and print methods for an object of class \code{timeSeries}.
    \cr
        
}


\details{

    \bold{Generation of Time Series Objects:}
    \cr
    
    We have defined a \code{timeSeries} class which is in many aspects similar
    to the S-Plus class with the same name, but has also some important
    differences. The class has seven Slots, the 'Data' slot which holds 
    the time series data in matrix form, the 'position' slot which holds
    the time/date as a character vector, the 'format' and 'FinCenter'
    slots which are the same as for the 'timeDate' object, the 'units'
    slot which holds the column names of the data matrix, and a 'title'
    and a 'documentation' slot which hold descriptive character strings.
    Date and time is managed in the same way as for \code{timeDate} objects.

}


\note{

    These functions were written for Rmetrics users using R and Rmetrics
    under Microsoft's Windows operating system where timze zones, 
    daylight saving times and holiday calendars are insuffeciently 
    supported. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data -  
   # Microsoft Data: 
   myFinCenter <<- "GMT"
   MSFT = as.timeSeries(data(msft.dat))
   head(MSFT)

## timeSeries -  
   # Create a timeSeries Objec - The Direct Way ...
   Close = MSFT[, 5]
   head(Close)
   # From Scratch ...
   data = as.matrix(MSFT[, 4])
   charvec = rownames(MSFT)
   Close = timeSeries(data, charvec, units = "Close")
   head(Close)
   c(start(Close), end(Close))
    
## window  -    
   # Cut out April Data from 2001:
   tsApril01 = window(Close, "2001-04-01", "2001-04-30") 
   tsApril01

## returnSeries -  
   # Compute Returns:
   args(returnSeries)
   # Continuous Returns:
   returnSeries(tsApril01)
   # Discrete Returns:
   returnSeries(tsApril01, type = "discrete")
   # Don't trim:
   returnSeries(tsApril01, trim = FALSE)
   # Use Percentage Values:
   tsRet = returnSeries(tsApril01, percentage = TRUE, trim = FALSE)
   tsRet
     
## applySeries -   
   # Aggregate weekly:
   GoodFriday(2001)
   to = timeSequence(from = "2001-04-11", length.out = 3, by = "week") 
   from = to - 6*24*3600
   from
   to
   applySeries(tsRet, from, to, FUN = sum)
}


\keyword{chron}

