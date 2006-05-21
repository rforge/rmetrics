\name{TimeSeriesData}

\alias{TimeSeriesData}

\alias{diff.timeSeries} 
\alias{lag.timeSeries} 
\alias{merge.timeSeries} 
\alias{scale.timeSeries}

\alias{summary.timeSeries}
\alias{var.timeSeries}

\alias{Ops.timeSeries}
\alias{abs.timeSeries}
\alias{sqrt.timeSeries}
\alias{exp.timeSeries}
\alias{log.timeSeries}
\alias{quantile.timeSeries}

\alias{[.timeSeries} 
\alias{cut.timeSeries} 
\alias{head.timeSeries}
\alias{tail.timeSeries}
\alias{outlier.timeSeries}

\alias{dim.timeSeries}
\alias{dimnames.timeSeries}
\alias{colnames<-.timeSeries}
\alias{rownames<-.timeSeries}
\alias{is.array.timeSeries}
  

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
    
    Functions and methods to modify 'timeSeries' objects:         

    \tabular{ll}{  
    \code{diff} \tab Takes differences from a 'timeSeries' object, \cr
    \code{lag} \tab Lags a 'timeSeries' object, \cr
    \code{merge} \tab Merges a 'timeSeries' object with a 'matrix', \cr
    \code{lag} \tab Scales and centers a 'timeSeries' object. }
    
    Functions and methods for classifying 'timeSeries' objects:
    
    \tabular{ll}{
    \code{summary} \tab Summarizes a 'timeSeries' object, \cr
    \code{var} \tab Computes Variance from a 'timeSeries' object.}
    
    Functions and methods for mathematical operations on 'timeSeries' objects:         

    \tabular{ll}{  
    \code{Ops.timeSeries} \tab S3: Arith method for a 'timeSeries' object, \cr
    \code{abs} \tab Returns absolute values of a 'timeSeries' object, \cr
    \code{sqrt} \tab Returns square root of a 'timeSeries' object, \cr
    \code{exp} \tab Returns the exponential values of a 'timeSeries' object, \cr
    \code{log} \tab Returns the logarithm of  a 'timeSeries' object, \cr
    \code{quantile.timeSeries} \tab Computes quantiles of a 'timeSeries' object. } 
    
    Functions and methods for subsetting 'timeSeries' objects: 
    
    \tabular{ll}{
    \code{"["} \tab "[" method for a 'timeSeries' object, \cr
    \code{cut} \tab Cuts a block from a 'timeSeries' object, \cr
    \code{head} \tab Returns the head of a 'timeSeries' object, \cr
    \code{tail} \tab Returns the tail of a 'timeSeries' object, \cr
    \code{outliers} \tab Removes outliers from a 'timeSeries' object. }
   
    Functions and methods for dimensional operations on 'timeSeries' objects: 
    
    \tabular{ll}{
    \code{dim} \tab Returns the dimension of a 'timeSeries' object, \cr
    \code{dimnames} \tab Returns the dimension names of a 'timeSeries' object, \cr
    \code{colnames<-} \tab Assigns column names to a 'timeSeries' object, \cr
    \code{rownames<-} \tab Assigns row names to a 'timeSeries' object, \cr
    \code{is.array} \tab Allows that NCOL and NROW work properly. }
    
}


\usage{
\method{diff}{timeSeries}(x, lag = 1, diff = 1, trim = FALSE, pad = NA, \dots)
\method{lag}{timeSeries}(x, k = 1, trim = FALSE, units = NULL, \dots)
\method{merge}{timeSeries}(x, y, units = NULL, \dots)
\method{scale}{timeSeries}(x, center = TRUE, scale = TRUE)

\method{summary}{timeSeries}(object, \dots)
\method{var}{timeSeries}(x, y = NULL, na.rm = FALSE, use)

\method{Ops}{timeSeries}(e1, e2)
\method{abs}{timeSeries}(x)
\method{sqrt}{timeSeries}(x)
\method{exp}{timeSeries}(x)
\method{log}{timeSeries}(x, base = exp(1))
\method{quantile}{timeSeries}(x, probs = 0.95, \dots)

\method{[}{timeSeries}(x, i = min(1, nrow(x@Data)):nrow(x@Data), 
    j = min(1, ncol(x@Data)):ncol(x@Data))
\method{cut}{timeSeries}(x, from, to, \dots)
\method{head}{timeSeries}(x, n = 6, \dots)
\method{tail}{timeSeries}(x, n = 6, \dots)
\method{outlier}{timeSeries}(x, sd = 10, complement = TRUE, \dots)

\method{dim}{timeSeries}(x)
\method{dimnames}{timeSeries}(x)
colnames<-.timeSeries(x) <- value
rownames<-.timeSeries(x) <- value
\method{is.array}{timeSeries}(x)
}


\arguments{
  
    \item{base}{
        [log] - \cr
        a positive number. The base with respect to which logarithms
        are computed. Defaults to \code{e=exp(1)}.
        }
    \item{center, scale}{ 
        [scale] - \cr
        either a logical value or a numeric vector of length equal to
        the number of columns of \code{x}.
        }
    \item{complement}{
        [outlierSeries] - \cr
        a logical flag, should the outler series
        or its complement be returns, by default \code{TRUE}
        which returns the series free of outliers.
        }
    \item{diff}{
        [diffSeries] - \cr
        an integer indicating the order of the difference. By default 1.
        }
    \item{dimnames}{
        [as.timeSeries] - \cr
        a logical, if \code{TRUE} the dimension names of the
        matrix are assigned to the time series object.
        }
    \item{e1, e2}{
        [Ops] - \cr   
        two objects of class \code{timeSeries}.
        }
    \item{from, to}{
        starting date and end date, \code{to} must be after \code{from}.
        }
    \item{i, j}{
        ["["] - \cr 
        index arguments used for subsettings.
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
    \item{method}{ 
        [alignDailySeries] - \cr
        the method to be used for the alignment. A character string, one
        of \code{"before"}, use the data from the row whose position is
        just before the unmatched position, or \code{"after"}, use the 
        data from the row whose position is just after the unmatched 
        position, or \code{"linear"}, interpolate linearly between 
        \code{"before"} and \code{"after"}. 
        }
    \item{n}{
        [head][tail] - \cr
        an integer specifying the number of lines to be returned. 
        By default \code{n=6}.
        }
    \item{na.rm}{
        [var] - \cr
        a logical flag. Should missing values be removed?
        By default \code{FALSE}.
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
    \item{probs}{
        a numeric value or numeric vector of probabilities with values 
        in \code{[0,1]}.
        }
    \item{sd}{
        [outlierSeries] - \cr
        a numeric value of standard deviations, e.g. 10
        means that values larger or smaller tahn ten 
        times the standard deviation will be removed from
        the series.
        }
    \item{trim}{
        [diffSeries][returnSeries] - \cr
        a logical value. By default \code{TRUE}, the first missing 
        observation in the return series will be removed. 
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
    \item{x}{
        [as] - \cr   
        a \code{matrix} type object to be converted.\cr
        [as.vector][as.matrix][as.data.frame] -\cr
        [applySeries] - \cr
        [cut][end][mergeSeries][plot][print][rev][start] - \cr
        an object of class \code{timeSeries}.
        }
    \item{y}{
        [mergeSeries][var] - \cr   
        a \code{matrix} type object to be merged with an object of
        class \code{timeSeries}. Must have the same number of rows.
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
## SOURCE("fCalendar.024B-TimeSeriesData")

## data - 
   # Load Microsoft Data:
   MSFT = as.timeSeries(data(msft.dat))[1:12, c("Open", "Close")]
   head(MSFT)

## diff | scale - 
   # Difference and scale the timeSeries Object:
   scale(diff(MSFT, lag = 2, trim = TRUE))

## lag -
   # Lag the timeSeries Object:
   lag(MSFT, k = -1:1)
    
## merge -    
   # Merge two timeSeries Object:
   merge(MSFT[, 1], diff(log(MSFT[, 1]), units = c("Price", "Return")))

## summary -  
   # MSFT Summary:
   summary(returnSeries(MSFT))

## var -    
   # Variance of a timeSeries Object:
   var(returnSeries(MSFT, percentage = TRUE))
   
## Ops | +/- * ^ ...
   # Mathematical Operations:
   MSFT^2
   MSFT[, 2] - MSFT[, 1]
   OPEN.RET = returnSeries(MSFT[, 1])
   OPEN.RET > 0
   quantile(OPEN.RET, probs = c(0.9, 0.95))
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

