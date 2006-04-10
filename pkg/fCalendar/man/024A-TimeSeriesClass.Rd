\name{TimeSeriesClass}

\alias{TimeSeriesClass}

\alias{timeSeries}
\alias{timeSeries-class}

\alias{read.timeSeries}

\alias{as.timeSeries}
\alias{is.timeSeries}
\alias{print.timeSeries}
\alias{plot.timeSeries}
\alias{lines.timeSeries}
\alias{points.timeSeries}
\alias{Ops.timeSeries}
\alias{[.timeSeries}   
\alias{head.timeSeries}
\alias{tail.timeSeries}
\alias{quantile.timeSeries}

\alias{seriesData}
\alias{seriesPositions}
\alias{isUnivariate}
\alias{isMultivariate}
\alias{start.timeSeries}
\alias{end.timeSeries}
\alias{as.vector.timeSeries}
\alias{as.matrix.timeSeries}
\alias{as.data.frame.timeSeries}
\alias{as.ts.timeSeries}

\alias{applySeries}

\alias{cutSeries}
\alias{cut.timeSeries}
\alias{diffSeries} 
\alias{diff.timeSeries}   
\alias{lagSeries} 
\alias{lag.timeSeries} 
\alias{mergeSeries}
\alias{merge.timeSeries} 
\alias{returnSeries}
\alias{logSeries}
\alias{log.timeSeries}
\alias{absSeries}
\alias{abs.timeSeries}
\alias{revSeries}
\alias{rev.timeSeries}
\alias{outlierSeries}

\alias{alignDailySeries}
\alias{ohlcDailyPlot}


\alias{colnames<-.timeSeries}
\alias{rownames<-.timeSeries}
\alias{dim.timeSeries}
\alias{dimnames.timeSeries}
\alias{is.array.timeSeries}
\alias{scale.timeSeries}
\alias{summary.timeSeries}
\alias{t.timeSeries}
\alias{var.timeSeries}


\title{timeSeries Class and Methods}


\description{
    
    A collection and description of functions 
    and methods dealing with regular and irregular 
    'timeSeries' objects. Dates and times are 
    implemented as 'timeDate' objects. Included 
    are functions and methods for the generation 
    and representation of 'timeSeries' objects, 
    and for mathematical operations.          
    \cr
    
    The functions and methods for the Generation of 'timeSeries' Objects are:         

    \tabular{ll}{   
    \code{timeSeries} \tab Creates a 'timeSeries' object from scratch, \cr
    \code{read.timeSeries} \tab Reads a 'timeSeries' from a spreadsheet file, \cr
    \code{as.timeSeries} \tab S3: Creates 'time Series' from a 'matrix', \cr
    \code{is.timeSeries} \tab S3: Tests if an object is of class a 'timeSeries', \cr
    \code{print.timeSeries} \tab S3: Print method for a 'timeSeries' object, \cr
    \code{plot.timeSeries} \tab S3: Plot method for a 'timeSeries' object, \cr
    \code{lines.timeSeries} \tab S3: Lines method for a 'timeSeries' object, \cr
    \code{points.timeSeries} \tab S3: Points method for a 'timeSeries' object, \cr
    \code{Ops.timeSeries} \tab S3: Arith method for a 'timeSeries' object, \cr
    \code{[.timeSeries} \tab S3: "[" method for a 'timeSeries' object, \cr
    \code{head.timeSeries} \tab S3: returns the head of a 'timeSeries' object, \cr
    \code{tail.timeSeries} \tab S3: returns the tail of a 'timeSeries' object \cr
    \code{quantile.timeSeries} \tab S3: produces quantile of a 'timeSeries' object. }           
            
    The functions and methods for the Representation of 'timeSeries' Objects are:         
            
    \tabular{ll}{        
    \code{seriesData} \tab Extracts data slot from a 'timeSeries', \cr
    \code{seriesPositions} \tab Extracts positions slot from a 'timeSeries', \cr
    \code{isUnivariate} \tab Tests if an object of class 'timeSeries' is univariate, \cr
    \code{start.timeSeries} \tab S3: Extracts start date of a 'timeSeries', \cr
    \code{end.timeSeries} \tab S3: Extracts end date of a 'timeSeries', \cr
    \code{as.vector.timeSeries} \tab S3: Converts a 'timeSeries' to a vector, \cr
    \code{as.matrix.timeSeries} \tab S3: Converts a 'timeSeries' to a matrix, \cr
    \code{as.data.frame.timeSeries} \tab S3: Converts a 'timeSeries' to a data.frame, \cr
    \code{as.ts.timeSeries} \tab S3: Converts a 'timeSeries' to a 'ts' object. }          
           
    The functions and methods for Math Operations of 'timeSeries' Objects are:         
            
    \tabular{ll}{         
    \code{applySeries} \tab Applies a function to margins of a 'timeSeries', \cr
    \code{alignDailySeries} \tab Aligns a daily 'timeSeries' to new positions, \cr        
    \code{cut, cutSeries} \tab Cuts out a piece from a 'timeSeries' object, \cr
    \code{merge, mergeSeries} \tab Merges a 'timeSeries' object with a 'matrix', \cr
    \code{ohlcDailyPlot} \tab Plots open high low close bar chart, \cr
    \code{rev, revSeries} \tab Reverts the order of 'timeSeries' object, \cr
    \code{diff, diffSeries} \tab Takes differences from a 'timeSeries' object, \cr
    \code{lag, lagSeries} \tab Lags a 'timeSeries' object, \cr
    \code{outlierSeries} \tab Removes outliers from a 'timeSeries' object, \cr
    \code{returnSeries} \tab Computes returns from a 'timeSeries' object, \cr
    \code{log, logSeries} \tab Returns logarithms of a 'timeSeries' object, \cr
    \code{abs, absSeries} \tab Returns absolute values of a 'timeSeries' object.}
        
}


\usage{
timeSeries(data, charvec, units = NULL, format = "ISO", zone = "GMT", 
    FinCenter = myFinCenter, recordIDs = data.frame(), title = NULL, 
    documentation = NULL, ...) 
read.timeSeries(file, zone = "GMT", FinCenter = "", title = "", 
    documentation = "", sep = ";")
    
as.timeSeries(x, dimnames = TRUE, format = "")
is.timeSeries(object) 

\method{print}{timeSeries}(x, ...)
\method{plot}{timeSeries}(x, reference.grid = TRUE, lty = 1, ...) 
\method{lines}{timeSeries}(x, ...)
\method{points}{timeSeries}(x, ...)
\method{Ops}{timeSeries}(e1, e2)
\method{[}{timeSeries}(x, i = min(1, nrow(x@Data)):nrow(x@Data), 
    j = min(1, ncol(x@Data)):ncol(x@Data))
\method{head}{timeSeries}(x, \dots)
\method{tail}{timeSeries}(x, \dots)
\method{quantile}{timeSeries}(x, probs = 0.95, column = 1, \dots)

seriesData(object)
seriesPositions(object)
isUnivariate(x)
isMultivariate(x)

\method{start}{timeSeries}(x, \dots)
\method{end}{timeSeries}(x, \dots)
\method{as.vector}{timeSeries}(x, mode = "any")
\method{as.matrix}{timeSeries}(x)
\method{as.data.frame}{timeSeries}(x, row.names = NULL, optional = NULL)
\method{as.ts}{timeSeries}(x, column = 1, \dots)

applySeries(x, from = NULL, to = NULL, by = c("monthly", "quarterly"), 
    FUN = colAvgs, units = NULL, \dots) 
cutSeries(x, from, to, \dots)
diffSeries(x, lag = 1, diff = 1, trim = FALSE, pad = NA, \dots)
lagSeries(x, k = 1, trim = FALSE, units = NULL, \dots)
outlierSeries(x, sd = 10, complement = TRUE)
mergeSeries(x, y, units = NULL, \dots)
returnSeries(x, type = c("continuous", "discrete"), percentage = FALSE, 
    trim = TRUE, digits = 4, units = NULL)
revSeries(x, \dots)
logSeries(x)
absSeries(x)

\method{cut}{timeSeries}(x, from, to, \dots)
\method{diff}{timeSeries}(x, lag = 1, diff = 1, trim = FALSE, pad = NA, \dots)
\method{lag}{timeSeries}(x, k = 1, trim = FALSE, units = NULL, \dots)
\method{merge}{timeSeries}(x, y, units = NULL, \dots)
\method{rev}{timeSeries}(x, \dots)
\method{log}{timeSeries}(x, base = exp(1))
\method{abs}{timeSeries}(x)

alignDailySeries(x, method = c("before", "after", "interp", "fillNA"), 
    include.weekends = FALSE, units = NULL)
ohlcDailyPlot(x, volume = TRUE, colOrder = c(1:5), units = 1e6, 
    xlab = c("Date", "Date"), ylab = c("Price", "Volume"), 
    main = c("O-H-L-C", "Volume"), grid.nx = 7, grid.lty = "solid", \dots) 
  
colnames<-.timeSeries(x) <- value
rownames<-.timeSeries(x) <- value

\method{dim}{timeSeries}(x)
\method{dimnames}{timeSeries}(x)
\method{is.array}{timeSeries}(x)
\method{scale}{timeSeries}(x, center = TRUE, scale = TRUE)
\method{summary}{timeSeries}(object, \dots)
\method{t}{timeSeries}(x)
\method{var}{timeSeries}(x, y = NULL, na.rm = FALSE, use)
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
    \item{base}{
        [log] - \cr
        a positive number. The base with respect to which logarithms
        are computed. Defaults to \code{e=exp(1)}.
        }
    \item{charvec}{
        a character vector of dates and times.
        }
    \item{center, scale}{ 
        [scale] - \cr
        either a logical value or a numeric vector of length equal to
        the number of columns of \code{x}.
        }
    \item{column}{
        specifies a column from a multivariate 'timeSeries' object.
        By default the first column is selected.
        }
    \item{complement}{
        [outlierSeries] - \cr
        a logical flag, should the outler series
        or its complement be returns, by default \code{TRUE}
        which returns the series free of outliers.
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
    \item{diff}{
        [diffSeries] - \cr
        an integer indicating the order of the difference. By default 1.
        }
    \item{digits}{
        [returnSeries] - \cr
        an integer value. The number of digits to be printed in the 
        output.
        }
    \item{dimnames}{
        [as.timeSeries] - \cr
        a logical, if \code{TRUE} the dimension names of the
        matrix are assigned to the time series object.
        }
    \item{documentation}{
        optional documentation string, or a vector of character strings.
        }
    \item{e1, e2}{
        [Ops] - \cr   
        two objects of class \code{timeSeries}.
        }
    \item{file}{
        the filename of a spreadsheet data set from which
        to import the data records.
        }
    \item{FinCenter}{
        a character with the the location of the  financial center named 
        as "continent/city". 
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
    \item{i, j}{
        ["["] - \cr 
        index arguments used for subsettings.
        }        
     \item{include.weekends}{   
        [alignDailySeries] - \cr
        a logical value. Should weekend dates be included or removed
        from the series.
        } 
    \item{k}{   
        [lagSeries] - \cr   
        an integer value. The number of lags (in units of observations).
        By default 1.
        }
    \item{lag}{   
        [diffSeries] - \cr
        an integer indicating which lag to use. By default 1.
        }
    \item{lty}{   
        [plot] - \cr
        an integer value, the typ of line to be drawn. By default 1,
        a solid line.
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
    \item{na.rm}{
        [var] - \cr
        a logical flag. Should missing values be removed?
        By default \code{FALSE}.
        }
    \item{mode}{
        [as.vector] - \cr
        A character string giving an atomic mode, or \code{"any"}.
        }  
    \item{optional, row.names}{
        [as.data.frame] - \cr
        both arguments are currently not used.
        }       
    \item{object}{
        [is][seriesData][seriesPositions][summary] -    
        an object of class \code{timeSeries}.
        }
    \item{pad}{
        [diffSeries] - \cr
        which value should get the padded values? By default \code{NA}.
        Another choice often used would be zero.
        }
    \item{percentage}{
        [returnSeries] - \cr
        a logical value. By default \code{FALSE}, if \code{TRUE} the 
        series will be expressed in percentage changes.
        }
    \item{probs}{
        a numeric value or numeric vector of probabilities with values 
        in \code{[0,1]}.
        }
    \item{recordIDs}{
        a data frame which can be used for record identification.
        }
    \item{reference.grid}{
        [plot] - \cr    
        a logical value. Should a grid be added to the plot? By
        default \code{TRUE}.
        } 
    \item{sd}{
        [outlierSeries] - \cr
        a numeric value of standard deviations, e.g. 10
        means that values larger or smaller tahn ten 
        times the standard deviation will be removed from
        the series.
        }
    \item{sep}{
        [read.timeSeries] - \cr
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
        [ohlcDailyPlot] - \cr
        a numeric value, specifying in which multiples the volume should
        be referenced on the plot labels. By default 1e6, i.e. in units
        of 1 Million.
        }
    \item{use}{
        [var] - \cr
        an optional character string giving a method for computing
        covariances in the presence of missing values. This must 
        be (an abbreviation of) one of the strings \code{"all.obs"},
        \code{"complete.obs"} or \code{"pairwise.complete.obs"}.
        }
    \item{value}{
        [colnames<-][rownames<-] - \cr
        a valid value for column names component of \code{dimnames(x)}. 
        For a \code{"timeSeries"} object this is either \code{NULL} or a 
        character vector of length the column dimension. Not, row names
        cannot be assigne for a \code{"timeSeries"} object, the function
        \code{rownames()} will stop and return an error message.
        }
    \item{volume}{
        [ohlcDailyPlot] - \cr
        a logigical value. Should a volume plot added to the OHLC Plot.
        By default \code{TRUE}.
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
    \item{y}{
        [mergeSeries][var] - \cr   
        a \code{matrix} type object to be merged with an object of
        class \code{timeSeries}. Must have the same number of rows.
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
    \code{read.timeSeries}\cr
    \code{as.timeSeries}
    \cr
    return a S4 object of class \code{timeSeries}.
    \cr
    
    \code{seriesData}\cr
    \code{seriesPositions}
    \cr
    extract the \code{@Data} and \code{@position} slots from a 
    \code{timeSeries} object. Thus, \code{seriesData} returns an 
    object of class \code{matrix}, and \code{seriesPositions}
    returns an object of class \code{timeDate}.
    \cr
    
    \code{is.timeSeries}
    \cr
    returns \code{TRUE} or \code{FALSE} depending on whether its
    argument is of \code{timeSeries} type or not.
    \cr
        
    \code{aggregateSeries}\cr
    \code{applySeries}\cr
    \code{cutSeries}\cr
    \code{mergeSeries}\cr
    \code{returnSeries}\cr
    \code{revSeries}
    \cr
    return a S4 object of class \code{timeSeries}.
    \cr
    
    \code{end, start}
    \cr
    return a S4 object of class \code{timedate}. These are the 
    start and end dates of a \code{timeSeries} object.
    \cr
    
    \code{as.vector}\cr
    \code{as.matrix}\cr
    \code{as.data.frame}
    \cr
    these are methods which convert a S4 object of class \code{timeSeries}
    either to a vector, a matrix or to a data frame.
    \cr
    
    \code{plot}\cr
    \code{lines}\cr
    \code{points}\cr
    \code{print}
    \cr
    plot and print methods for an object of class \code{timeSeries}.
    Note that the plot function requires the packages \code{its} and
    \code{Hmisc}.
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
## SOURCE("fCalendar.024A-TimeSeriesClass")

## data - 
   xmpCalendar("\nStart: Load Microsoft Data > ")
   data(msft.dat)
   myFinCenter = "GMT"
   msft.dat[1:3, ]

## timeSeries -
   xmpCalendar("\nNext: Create a timeSeries Object > ")
   close = msft.dat[, 5]
   ts = timeSeries(close, as.character(msft.dat[, 1]), format = "\%m/\%d/\%Y", 
     units = "MSFT", zone = "GMT", FinCenter = "GMT")
   class(ts)
   ts[1:3,]
   c(start(ts), end(ts))
    
## cutSeries -    
   xmpCalendar("\nNext: Cut out April Data from 2001 > ")
   ts.Apr01 = cutSeries(ts, "2001-04-01", "2001-04-30") 
   ts.Apr01

## returnSeries -  
   xmpCalendar("\nNext: Compute Returns > ")
   # Compute Returns:
   args(returnSeries)
   # Continuous Returns:
   returnSeries(ts.Apr01)
   # Discrete Returns:
   returnSeries(ts.Apr01, type = "discrete")
   # Don't trim:
   returnSeries(ts.Apr01, trim = FALSE)
   # Use Percentage Values:
   returnSeries(ts.Apr01, percentage = TRUE, trim = FALSE)

## mergeSeries -    
   xmpCalendar("\nNext: Merge Series with Returns > ")
   # Include last Day from March:
   ts.APR01 = cutSeries(ts, "2001-03-29", "2001-04-30") 
   ts.merged = mergeSeries(x = ts.APR01, 
     returnSeries(ts.APR01, trim = FALSE)@Data, units = c("MSFT", "Returns"))
   ts.merged
    
## alignDailySeries -     
   xmpCalendar("\nNext: Align with NA > ")
   args(alignDailySeries)
   ts.ret = returnSeries(ts.APR01, trim = TRUE)
   GoodFriday(2001)   
   EasterMonday(2001) 
   alignDailySeries(ts.ret, method = "fillNA", include.weekends = FALSE)
   alignDailySeries(ts.ret, method = "fillNA", include.weekends = TRUE)
    
## alignDailySeries - 
   xmpCalendar("\nNext: Interpolate > ")
   ts.ret
   alignDailySeries(ts.ret, method = "interp", include.weekend = FALSE)
   alignDailySeries(ts.ret, method = "interp", include.weekend = TRUE)

## applySeries -   
   xmpCalendar("\nNext: Aggregate weekly > ")
   GoodFriday(2001)
   to = timeSequence(from = "2001-04-11", length.out = 3, 
     by = "weeks", format = "\%Y-\%m-\%d", FinCenter = "GMT") 
   from = to - 6*24*3600
   data.frame(from, to)
   applySeries(ts.ret, from, to, FUN = sum)
}
   

\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

