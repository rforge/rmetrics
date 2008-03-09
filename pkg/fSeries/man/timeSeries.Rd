\name{TimeSeriesClass}


\title{timeSeries Class}

\alias{TimeSeriesClass}

\alias{timeSeries}
\alias{timeSeries-class}

\alias{readSeries}

\alias{orderStatistics}

\alias{seriesData}


\description{
    
    A collection and description of functions 
    and methods dealing with regular and irregular 
    'timeSeries' objects. Dates and times are 
    implemented as 'timeDate' objects. 
    \cr
    
    Functions to generate and modify 'timeSeries' objects:         

    \tabular{ll}{   
    \code{timeSeries} \tab Creates a 'timeSeries' object from scratch, \cr
    \code{readSeries} \tab Reads a 'timeSeries' from a spreadsheet file, \cr
    \code{orderStatistics} \tab Computes order statistic of a 'timeSeries'. }
    
    Data Slot and classification of 'timeSeries' objects: 
    
    \tabular{ll}{        
    \code{seriesData} \tab Extracts data slot from a 'timeSeries'. }          
}


\usage{
timeSeries(data, charvec, units = NULL, format = NULL, zone = myFinCenter, 
    FinCenter = myFinCenter, recordIDs = data.frame(), title = NULL, 
    documentation = NULL, \dots)
       
readSeries(file, header = TRUE, sep = ";", zone = myFinCenter, 
    FinCenter = myFinCenter, title = NULL, documentation = NULL, \dots)
    
orderStatistics(x)

seriesData(object)
}


\arguments{

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
    \item{object}{
        [is][seriesData][seriesPositions][show][summary] -    
        an object of class \code{timeSeries}.
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
        an optional character string, which allows to overwrite the 
        current column names of a \code{timeSeries} object. By default 
        \code{NULL} which means that the column names are selected
        automatically. 
        }     
    \item{x}{
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

