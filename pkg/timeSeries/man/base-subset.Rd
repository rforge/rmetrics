\name{TimeSeriesSubsettings}


\alias{TimeSeriesSubsettings}

\alias{[,timeSeries,ANY,index_timeSeries-method}
\alias{[,timeSeries,character,character-method}
\alias{[,timeSeries,character,index_timeSeries-method}
\alias{[,timeSeries,character,missing-method}
\alias{[,timeSeries,index_timeSeries,character-method}
\alias{[,timeSeries,index_timeSeries,index_timeSeries-method}
\alias{[,timeSeries,index_timeSeries,missing-method}
\alias{[,timeSeries,matrix,index_timeSeries-method}
\alias{[,timeSeries,matrix,missing-method}
\alias{[,timeSeries,missing,character-method}
\alias{[,timeSeries,missing,index_timeSeries-method}
\alias{[,timeSeries,missing,missing-method}
\alias{[,timeSeries,timeDate,character-method}
\alias{[,timeSeries,timeDate,index_timeSeries-method}
\alias{[,timeSeries,timeDate,missing-method}
\alias{[,timeSeries,timeSeries,index_timeSeries-method}
\alias{[,timeSeries,timeSeries,missing-method}

\alias{[,timeSeries,time_timeSeries,ANY-method}
\alias{[,timeSeries,time_timeSeries,character-method}
\alias{[,timeSeries,time_timeSeries,index_timeSeries-method}
\alias{[,timeSeries,time_timeSeries,missing-method}

\alias{$,timeSeries,ANY-method}
\alias{$,timeSeries,character-method}

\alias{[<-,timeSeries,timeDate,index_timeSeries-method}
\alias{[<-,timeSeries,timeDate,missing-method}
\alias{[<-,timeSeries,timeSeries,index_timeSeries-method}
\alias{[<-,timeSeries,timeSeries,missing-method}
\alias{[<-,timeSeries,character,character-method}
\alias{[<-,timeSeries,character,index_timeSeries-method}
\alias{[<-,timeSeries,character,missing-method}
\alias{[<-,timeSeries,index_timeSeries,character-method}
\alias{[<-,timeSeries,matrix,character-method}
\alias{[<-,timeSeries,timeDate,character-method}
\alias{[<-,timeSeries,timeSeries,character-method}

\alias{[<-,timeSeries,character,ANY-method}
\alias{[<-,timeSeries,timeDate,ANY-method}

\alias{$<-,timeSeries,character,ANY-method}
\alias{$<-,timeSeries,character,factor-method}
\alias{$<-,timeSeries,character,numeric-method}

\alias{window,timeSeries-method}
\alias{cut,timeSeries-method}
\alias{head,timeSeries-method}
\alias{tail,timeSeries-method}

\alias{window.timeSeries}
\alias{cut.timeSeries}
\alias{head.timeSeries}
\alias{tail.timeSeries}

\alias{outlier}
\alias{outlier,timeSeries-method}
\alias{outlier,ANY-method}

\title{Subsettig Time Series}


\description{
  
    Subset a 'timeSeries' objects due to different aspects.         
    \cr
  
    \tabular{ll}{
    \code{[} \tab "[" method for a 'timeSeries' object, \cr
    \code{[<-} \tab "[<-" method to assign value for a subset of a 'timeSeries' object, \cr
    \code{window} \tab Windows a piece from a 'timeSeries' object, \cr
    \code{cut} \tab A no longer used synonyme for window, \cr
    \code{head} \tab Returns the head of a 'timeSeries' object, \cr
    \code{tail} \tab Returns the tail of a 'timeSeries' object, \cr
    \code{outliers} \tab Removes outliers from a 'timeSeries' object. }
  
}


\usage{

% \method{[}{timeSeries}(x, i, j, drop)
% \method{[}{timeSeries}(x, i, j) <- value

\S4method{window}{timeSeries}(x, start, end, \dots)

\S4method{head}{timeSeries}(x, n = 6, recordIDs = FALSE, \dots)
\S4method{tail}{timeSeries}(x, n = 6, recordIDs = FALSE, \dots)

\S4method{outlier}{timeSeries}(x, sd = 10, complement = TRUE, \dots)

\S4method{cut}{timeSeries}(x, from, to, \dots)
}


\arguments{
  
    \item{complement}{
        [outlierSeries] - \cr
        a logical flag, should the outler series
        or its complement be returns, by default \code{TRUE}
        which returns the series free of outliers.
        }
     \item{from, to}{
        starting date and end date, \code{to} must be after \code{from}.
        }
     \item{start, end}{
        starting date and end date, \code{end} must be after \code{start}.
        }
%      \item{i, j}{
%         ["["] - \cr 
%         index arguments used for subsettings.
%         }       
     \item{n}{
        [head][tail] - \cr
        an integer specifying the number of lines to be returned. 
        By default \code{n=6}.
        }
     \item{recordIDs}{
        [head][tail] - \cr
        a logical value. Should the \code{recordIDs} returned together
        with the data matrix and time series positions?
        }
     \item{sd}{
        [outlierSeries] - \cr
        a numeric value of standard deviations, e.g. 10
        means that values larger or smaller tahn ten 
        times the standard deviation will be removed from
        the series.
        }
%       \item{value}{
%         a numeric value to use as a replacement. It will be repeated a
%         whole number of times if necessary. 
%      }
     \item{x}{
        an object of class \code{timeSeries}.
        }
     \item{\dots}{
        arguments passed to other methods.
        }
        
}

        
\value{

    all functions return an object of class \code{timeSeries}.
  
}


\examples{
## Create an Artificial timeSeries Object - 
   setRmetricsOptions(myFinCenter = "GMT")
   charvec = timeCalendar()
   set.seed(4711)
   data = matrix(exp(cumsum(rnorm(12, sd = 0.1))))
   tS = timeSeries(data, charvec, units = "tS")
   tS
   
## Subset Series by Counts "[" - 
   tS[1:3, ]
       
## Subset the Head of the Series - 
   head(tS, 6)
}


\keyword{chron}

