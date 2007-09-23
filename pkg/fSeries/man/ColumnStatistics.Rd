\name{SimpleStatistics}

\alias{SimpleStatistics}

\alias{fapply}
\alias{aggregate.timeSeries}
\alias{diff.timeSeries} 
\alias{lag.timeSeries} 
\alias{merge.timeSeries} 
\alias{rbind.timeSeries}
\alias{cumsum.timeSeries}
\alias{scale.timeSeries}

\alias{dim.timeSeries}
\alias{dimnames.timeSeries}
\alias{colnames<-.timeSeries}
\alias{rownames<-.timeSeries}
\alias{is.array.timeSeries}
  

\title{Simple timeSeries Statistics}


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
    \code{fapply} \tab Applies a function to 'timeSeries' windows, \cr 
    \code{aggregate} \tab Aggregates a 'timeSeries' object, \cr
    \code{diff} \tab Takes differences from a 'timeSeries' object, \cr
    \code{lag} \tab Lags a 'timeSeries' object, \cr
    \code{merge} \tab Merges two 'timeSeries' objects, \cr
    \code{rbinds} \tab Binds rows of two 'timeSeries' objects, \cr
    \code{cumsum} \tab Returns the cumulated sum of a 'timeSeries' object, \cr
    \code{lag} \tab Scales and centers a 'timeSeries' object. }
    
    Functions and methods for classifying 'timeSeries' objects:
    
    \tabular{ll}{
    \code{mean} \tab Computes column means for a 'timeSeries' object, \cr
    \code{var} \tab Computes Variance from a 'timeSeries' object, \cr
    \code{cov} \tab Computes Covariance from a 'timeSeries' object, \cr
    \code{cor} \tab Computes Correlations from a 'timeSeries' object.}
   
    Functions and methods for dimensional operations on 'timeSeries' objects: 
    
    \tabular{ll}{
    \code{dim} \tab Returns the dimension of a 'timeSeries' object, \cr
    \code{dimnames} \tab Returns the dimension names of a 'timeSeries' object, \cr
    \code{colnames<-} \tab Assigns column names to a 'timeSeries' object, \cr
    \code{rownames<-} \tab Assigns row names to a 'timeSeries' object, \cr
    \code{is.array} \tab Allows that NCOL and NROW work properly. }
    
}


\usage{
fapply(x, from, to, FUN, \dots)

\method{aggregate}{timeSeries}(x, by = c("monthly", "quarterly"), 
    FUN = colMeans, units = NULL, \dots)
\method{diff}{timeSeries}(x, lag = 1, diff = 1, trim = FALSE, pad = NA, \dots)
\method{lag}{timeSeries}(x, k = 1, trim = FALSE, units = NULL, \dots)
\method{merge}{timeSeries}(x, y, units = NULL, \dots)
\method{rbind}{timeSeries}(x, y)
\method{cumsum}{timeSeries}(x)
\method{scale}{timeSeries}(x, center = TRUE, scale = TRUE)

\method{mean}{timeSeries}(x, \dots)
\method{var}{timeSeries}(x, y = NULL, na.rm = FALSE, use)
\method{cov}{timeSeries}(x, y = NULL, use = "all.obs", 
    method = c("pearson", "kendall", "spearman"))
\method{cor}{timeSeries}(x, y = NULL, use = "all.obs", 
    method = c("pearson", "kendall", "spearman"))
\method{quantile}{timeSeries}(x, probs = 0.95, \dots)

\method{dim}{timeSeries}(x)
\method{dimnames}{timeSeries}(x)
colnames<-.timeSeries(x) <- value
rownames<-.timeSeries(x) <- value
\method{is.array}{timeSeries}(x)
}


\arguments{
  
    \item{by}{
        [aggregate] - \cr
        a character string denoting the aggregation period, either 
        \code{"monthly"} or \code{"quarterly"}.
        }
    \item{center, scale}{ 
        [scale] - \cr
        either a logical value or a numeric vector of length equal to
        the number of columns of \code{x}.
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
    \item{from, to}{
        starting date and end date, \code{to} must be after \code{from}.
        }
    \item{FUN}{
        the function to be applied.
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
        \code{"before"} and \code{"after"}. \cr
        [cov][cor] - \cr
        a character string indicating which correlation coefficient (or 
        covariance) is to be computed. One of \code{"pearson"} (default), 
        \code{"kendall"}, or \code{"spearman"}, can be abbreviated.
        }
    \item{na.rm}{
        [var] - \cr
        a logical flag. Should missing values be removed?
        By default \code{FALSE}.
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
    \item{trim}{
        [diffSeries][returnSeries] - \cr
        a logical value. By default \code{TRUE}, the first missing 
        observation in the return series will be removed. 
        }
    \item{units}{
        [applySeries][lag][alignDailySeries][returnSeries][mergeSeries] - \cr
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

    \code{fapply}\cr
    returns an object depending on the output of function \code{FUN}.
    If \code{FUN} returns an S4 object of class \code{timeSeries} then
    also \code{fapply} returns an S4 object of class \code{timeSeries}.
    In all other cases \code{fapply} returns a \code{list}.
    \cr
    
    \code{timeSeries}\cr
    \code{read.timeSeries}\cr
    \code{as.timeSeries}
    \cr
    return an S4 object of class \code{timeSeries}.
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
    \code{window}\cr
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


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data - 
   # Create an artificial timeSeries object:
   myFinCenter <<- "GMT"
   charvec = timeCalendar()
   set.seed(4711)
   data = matrix(exp(cumsum(rnorm(12, sd = 0.1))))
   TS = timeSeries(data, charvec, units = "TS")
   TS
   par(ask = FALSE)
   plot(TS, type = "o", pch = 19)

## diff | scale - 
   # Difference and scale the timeSeries Object:
   scale(diff(TS, lag = 2, trim = TRUE))

## lag -
   # Lag the timeSeries Object:
   lag(TS, k = -1:1)
    
## merge -    
   # Merge two timeSeries Object:
   merge(TS, diff(log(TS), units = c("ts", "diffLog")))

## var -    
   # Variance of a timeSeries Object:
   var(returnSeries(TS, percentage = TRUE))
   
## Ops | +/- * ^ ...
   # Mathematical Operations:
   TS^2
   TS[2:4]
   OR = returnSeries(TS)
   OR
   OR > 0
   quantile(OR, probs = c(0.90, 0.95))
}


\keyword{chron}

