\name{TimeSeriesClass}

\alias{TimeSeriesClass}

\alias{timeSeries}
\alias{timeSeries-class}

\alias{readSeries}
\alias{returnSeries}
\alias{durationSeries}
\alias{midquoteSeries}
\alias{spreadSeries}

\alias{applySeries}

\alias{orderStatistics}

\alias{seriesData}
\alias{isUnivariate}
\alias{isMultivariate}

\alias{show.timeSeries}
\alias{show,timeSeries-method}

\alias{summary.timeSeries}
\alias{plot.timeSeries}
\alias{lines.timeSeries}
\alias{points.timeSeries}

\alias{dummyDailySeries}
\alias{alignDailySeries}
\alias{ohlcDailyPlot}


\title{timeSeries Class}


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
    \code{returnSeries} \tab Computes returns from a 'timeSeries' object, \cr
    \code{durationSeries} \tab Computes durations from a 'timeSeries' object, \cr
    \code{midquoteSeries} \tab Computes mid quotes from a 'timeSeries' object, \cr
    \code{spreadSeries} \tab Computes spreads from a 'timeSeries' object, \cr
    \code{applySeries} \tab Applies a function to margins of a 'timeSeries', \cr
    \code{orderStatistics} \tab Computes order statistic of a 'timeSeries'. }
    
    Data Slot and classification of 'timeSeries' objects: 
    
    \tabular{ll}{        
    \code{seriesData} \tab Extracts data slot from a 'timeSeries', \cr
    \code{isUnivariate} \tab Tests if a 'timeSeries' object is univariate, \cr
    \code{isMultivariate} \tab Tests if a 'timeSeries' object is multivariate. }
    
    Functions to print and plot 'timeSeries' objects:      
    
    \tabular{ll}{   
    \code{print.timeSeries} \tab S3: Print method for a 'timeSeries' object, \cr
    \code{summary.timeSeries} \tab S3: Summary method for a 'timeSeries' object, \cr
    \code{plot.timeSeries} \tab S3: Plot method for a 'timeSeries' object, \cr
    \code{lines.timeSeries} \tab S3: Lines method for a 'timeSeries' object, \cr
    \code{points.timeSeries} \tab S3: Points method for a 'timeSeries' object. }
    
    Special Functions for daily 'timeSeries' objects:  
    
    \tabular{ll}{   
    \code{dummyDailySeries} \tab Creates a dummy daily 'timeSeries' object, \cr        
    \code{alignDailySeries} \tab Aligns a daily 'timeSeries' to new positions, \cr      
    \code{ohlcDailyPlot} \tab Plots open high low close bar chart. }
    
}


\usage{
timeSeries(data, charvec, units = NULL, format = NULL, zone = myFinCenter, 
    FinCenter = myFinCenter, recordIDs = data.frame(), title = NULL, 
    documentation = NULL, ...)   
readSeries(file, header = TRUE, sep = ";", zone = myFinCenter, 
    FinCenter = myFinCenter, title = NULL, documentation = NULL, \dots)
returnSeries(x, type = c("continuous", "discrete"), percentage = FALSE, 
    trim = TRUE, digits = 4, units = NULL)  
durationSeries(x, trim = FALSE, units = c("secs", "mins", "hours"))   
midquoteSeries(x, which = c("Bid", "Ask"))
spreadSeries(x, which = c("Bid", "Ask"), tickSize = NULL)
 
applySeries(x, from = NULL, to = NULL, by = c("monthly", "quarterly"), 
    FUN = colAvgs, units = NULL, format = x@format, zone = x@FinCenter, 
    FinCenter = x@FinCenter, recordIDs = data.frame(), title = x@title,
    documentation = x@documentation, \dots)
    
orderStatistics(x)

seriesData(object)
isUnivariate(x)
isMultivariate(x)

show.timeSeries(object)

\method{summary}{timeSeries}(object, \dots)
\method{plot}{timeSeries}(x, \dots) 
\method{lines}{timeSeries}(x, \dots)
\method{points}{timeSeries}(x, \dots)

dummyDailySeries(x = rnorm(365), units = "X", zone = myFinCenter, 
    FinCenter = myFinCenter)
alignDailySeries(x, method = c("before", "after", "interp", "fillNA"), 
    include.weekends = FALSE, units = NULL, zone = myFinCenter, 
    FinCenter = myFinCenter)
ohlcDailyPlot(x, volume = TRUE, colOrder = c(1:5), units = 1e6, 
    xlab = c("Date", "Date"), ylab = c("Price", "Volume"), 
    main = c("O-H-L-C", "Volume"), grid.nx = 7, grid.lty = "solid", \dots) 
}


\arguments{

    \item{by}{
        [applySeries] - \cr
        a character either \code{"monthly"} or \code{"quarterly"}. The
        default value is \code{"monthly"}. Only operative when both arguments
        \code{from} and \code{to} have ther default values \code{NULL}.
        In this case the function \code{FUN} will be applied to monthly or
        quarterly periods.
        }
    \item{charvec}{
        a character vector of dates and times.
        }
    \item{colOrder}{
        [ohlcDailyPlot] - \cr 
        an integer vector which gives the order of the prices and the
        volume in the input object. By default the following order of
        columns from 1 to 5 is assumed: Open, high, low, close, and volume.
        }
    \item{data}{
        a \code{data.frame} or a \code{matrix} object of numeric data.
        }
    \item{digits}{
        [returnSeries] - \cr
        an integer value. The number of digits to be printed in the 
        output.
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
        [applySeries] - \cr
        a function to use for aggregation, by default \code{colAvgs}.
        }
    \item{grid.lty, grid.nx}{
        [ohlcDailyPlot] - \cr
        The type of grid line and the number of grid lines used in the
        plot.
        }
     \item{include.weekends}{   
        [alignDailySeries] - \cr
        a logical value. Should weekend dates be included or removed
        from the series.
        }
    \item{tickSize}{
        [spreadSeries] - \cr
        the default is NULL to simply compute price changes in original 
        price levels. If ticksize is supplied, the price changes will be 
        divided by the value of \code{inTicksOfSize} to compute
        price changes in ticks.
        }
    \item{main}{
        [ohlcDailyPlot] - \cr
        a character string to title the price and volume plot.
        }
    \item{method}{ 
        [alignDailySeries] - \cr
        the method to be used for the alignment. A character string, one
        of \code{"before"}, use the data from the row whose position is
        just before the unmatched position, or \code{"after"}, use the 
        data from the row whose position is just after the unmatched 
        position, or \code{"linear"}, interpolate linearly between 
        \code{"before"} and \code{"after"}. 
        }
    \item{object}{
        [is][seriesData][seriesPositions][show][summary] -    
        an object of class \code{timeSeries}.
        }
    \item{percentage}{
        [returnSeries] - \cr
        a logical value. By default \code{FALSE}, if \code{TRUE} the 
        series will be expressed in percentage changes.
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
    \item{trim}{
        [diffSeries][returnSeries] - \cr
        a logical value. By default \code{TRUE}, the first missing 
        observation in the return series will be removed. 
        }
    \item{type}{
        [returnSeries] - \cr
        a character string specifying how to compute the returns. Valid 
        choices are: \code{continuous} and \code{discrete}. For the 
        default \code{type="continuous"}, the returns are calculated as 
        the logarithmic differences, otherwise if \code{type="discrete"}, 
        the returns are calculated as percentage changes. 
        }
    \item{units}{
        [applySeries][lag][allignDailySeries][returnSeries][mergeSeries] - \cr
        an optional character string, which allows to overwrite the 
        current column names of a \code{timeSeries} object. By default 
        \code{NULL} which means that the column names are selected
        automatically.
        \cr
        [durationSeries] - \cr
        a character value or vector which allows to set the 
        units in which the durations are measured. By default durations
        are measured in seconds. 
        \cr
        [ohlcDailyPlot] - \cr
        a numeric value, specifying in which multiples the volume should
        be referenced on the plot labels. By default 1e6, i.e. in units
        of 1 Million.
        } 
    \item{volume}{
        [ohlcDailyPlot] - \cr
        a logigical value. Should a volume plot added to the OHLC Plot.
        By default \code{TRUE}.
        }
    \item{which}{
        [midquoteSeries][spreadSeries] - \cr
        a vector with two character strings naming the column names of
        the time series from which to compute the mid quotes and spreads.
        By default these are bid and ask prices with column names
        \code{c("Bid", "Ask")}.
        }
    \item{x}{
        [as] - \cr   
        a \code{matrix} type object to be converted.\cr
        [as.vector][as.matrix][as.data.frame] -\cr
        [applySeries] - \cr
        [cut][end][mergeSeries][plot][print][rev][start] - \cr
        an object of class \code{timeSeries}.
        }
    \item{xlab, ylab}{
        [ohlcDailyPlot] - \cr
        two string vectors to name the x and y axis of the price and
        volume plot.
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


\examples{
## data - Data Frame:
   # Microsoft Data: 
   myFinCenter <<- "GMT"
   MSFT = as.timeSeries(data(msft.dat))
   head(MSFT)

## timeSeries - Conversion:
   # Create a timeSeries Objec - The Direct Way ...
   Close = MSFT[, 5]
   head(Close)
   # From Scratch ...
   data = as.matrix(MSFT[, 4])
   charvec = rownames(MSFT)
   Close = timeSeries(data, charvec, units = "Close")
   head(Close)
   c(start(Close), end(Close))
    
## Cut Series -    
   # Cut out April Data from 2001:
   tsApril01 = cut(Close, "2001-04-01", "2001-04-30") 
   tsApril01

## Return Series -  
   # Compute Returns:
   args(returnSeries)
   # Continuous Returns:
   returnSeries(tsApril01)
   # Discrete Returns:
   returnSeries(tsApril01, type = "discrete")
   # Don't trim:
   returnSeries(tsApril01, trim = FALSE)
   # Use Percentage Values:
   returnSeries(tsApril01, percentage = TRUE, trim = FALSE)
    
## Align Daily Series -     
   # Align with NA:
   args(alignDailySeries)
   tsRet = returnSeries(tsApril01, trim = TRUE)
   GoodFriday(2001)   
   EasterMonday(2001) 
   alignDailySeries(tsRet, method = "fillNA", include.weekends = FALSE)
   alignDailySeries(tsRet, method = "fillNA", include.weekends = TRUE)
    
## alignDailySeries - 
   # Interpolate:
   tsRet
   alignDailySeries(tsRet, method = "interp", include.weekend = FALSE)
   alignDailySeries(tsRet, method = "interp", include.weekend = TRUE)

## applySeries -   
   # Aggregate weekly:
   GoodFriday(2001)
   to = timeSequence(from = "2001-04-11", length.out = 3, by = "week") 
   from = to - 6*24*3600
   from
   to
   applySeries(tsRet, from, to, FUN = sum)
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

