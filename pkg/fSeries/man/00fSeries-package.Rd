\name{fSeries-package


\alias{fSeries-package


\alias{fSeries


\docType{package


\title{Utilities and Tools Package


\description{

    Package of Time Series Tools and Utilities.
    
}


\details{

    \tabular{ll{
        Package:    \tab fSeries\cr
        Type:       \tab Package\cr
        Version:    \tab 270.73\cr
        Date:       \tab 2008\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2008 Diethelm Wuertz, Rmetrics Foundation, GPL\cr
        URL:        \tab \url{http://www.rmetrics.org
    




\section{Overview of Topics:}{

    1. S4 timeSeries Class Definition\sr
    2. Creating timeSeries Objects\cr
    3. Printing and Plotting timeSeries Objects\cr
    4. Modifying 'timeSeries' Objects\cr
    
}   
  

\section{1. S4 timeSeries Class}{


    \emph{timeSeries Class Definition}
    
    'timeSeries' ...
    
    \emph{Extracting Information from a timeSeries object:}
    
    isUnivariate\cr
    isMultivariate
    


\section{2. Creating timeSeries Objects}{

    'timeSeries' objects can be created in several ways. One can create 
    them from scratch, or one can read them from a file. If we have a file
    it is assumed that the first column holds a character string with the
    date/time positions, named charvec", and the remaining column(s) the 
    numeric time series records named "data".
    
    \emph{Create a 'timeSeries' object from scratch:}
    
    
    \code{timeSeries}
    
    
    \emph{Read a time Series from a file:}
    
    
    \code{readSeries}
    
    
    \emph{Attach a 'timeSeries' object to the search path:}
    
    
    \tabular{ll}{
    \code{attach} \tab attaches a 'timeSeries' object, \cr
    \code{detach} \tab detaches a 'timeSeries' object [see base package]. } 
    
}


\section{3. Printing and Plotting 'timeSeries' Objects}{

    print
    
    plot\cr
    points\cr
    lines 

}



\section{4. Modifying 'timeSeries' Objects}{

    If we have a price/index series or a return series, it is often
    required taht we have to convert from one to the other representation.
    So Rmetrics makes functions available which compute \code{returns} from 
    price/index series or the \code{cumulated} series from returns. Further
    modifications are concerned with drawdowns, durations, spreads and 
    midquotes.
    
    List of Functions:
    
    \tabular{ll}{  
    \code{returns} \tab Compute returns from prices or indexes, \cr
    \code{cumulated} \tab compute cumulated series from a returns, \cr
    \code{drawdowns} \tab compute series of drawdowns from financial returns,\cr
    \code{durations} \tab compute durations from a financial time series,\cr
    \code{spreads} \tab compute spreads from a price/index stream, \cr
    \code{miquotes} \tab compute mid quotes from a price/index stream.
    }

}


\section{Subsetting timeSeries Objects}{
    
    'timeSeries' objects can subsetted in several ways. The method
    \code{window} (and the deprecated function \code(cut}) extracts 
    the subset of a 'timeSeries' observed between the times \code{from} 
    and  \code{to}. The methods \code{head} and \code{tail} return the
    first or last part of a time series for a specified length.
    The method \code{outlier} can be used to remove huge "outliers"
    which may appear for example through the redefinition of an index
    (it should not be used for outlier detection!).
    
    There are other functions provided which can be considered in broader
    sense as subset functions. For example the function 
    
    \preformatted{
        fapply(x, from, to, FUN, ...) 
    } 
      
    can be used to subset a time series with very complex rules, e.g.
    subset Mondays from a series, subset the last Thursdays in every
    Month, subset from a daily series open (first), high, low, close (last)
    prices to a end-of-month time series, etc.
     
    As another example, we mention the function \code{aggreagte} which 
    can data records subset to monthly or quarterly information with 
    information specified by the function \code{FUN}
    
    \preformatted{
         aggregate(x, by = c("monthly", "quarterly"), FUN = colMeans, 
            units = NULL, ...) 
    } 
    
    List of Functions:
    
    \tabular{ll}{  
    \code{"["} \tab \cr
    \code{"[<-"} \tab \cr
    
    \code{window} \tab \cr 
    \code{head} \tab \cr
    \code{tail} \tab \cr
    
    \code{outlier} \tab \cr
    
    \code{fapply} \tab \cr
    \code{aggreagte \tab
    }
    
    
}


\section{Merging and Binding timeSeries Objects}{

    List of Functions:
    
    merge\cr
    rbind\cr
    lag
    
}


\section{Reordering the columns of timeSeries Objects}{

    List of Functions:
    
    orderColnames\cr
    sortColname\cr
    sampleColnames\cr
    statsColnames\cr
    pcaColnames\cr
    hclustColnames
    
}


\section{Renaming column and rows of timeSeries Objects}{

    List of Functions:
    
    dim\cr
    dim.timeSeries\cr
    dimnames.timeSeries\cr
    colnames<-.timeSeries\cr
    rownames<-.timeSeries\cr
    is.array.timeSeries
    
}



\section{Mathematical Operations}{

    List of Functions:
    
    \tabular{ll}{  
    \code{Ops.timeSeries} \tab S3: Arith method for a 'timeSeries' object, \cr
    \code{abs} \tab Returns absolute values of a 'timeSeries' object, \cr
    \code{sqrt} \tab Returns square root of a 'timeSeries' object, \cr
    \code{exp} \tab Returns the exponential values of a 'timeSeries' object, \cr
    \code{log} \tab Returns the logarithm of  a 'timeSeries' object, \cr
    \code{sign} \tab Returns the signs of a 'timeSeries' object, \cr
    \code{diff} \tab Differences a 'timeSeries' object, \cr 
    \code{scale} \tab Centers and/or scales a 'timeSeries' object, \cr
    \code{quantile} \tab Returns quantiles of an univariate 'timeSeries'. }
    
}


\section{Handling Missing Data}{

    List of Functions:
    \tabular{ll}{  
    \code{na.omit} \tab Handles NAs in a timeSeries object, \cr
    \code{removeNA} \tab removes NAs from a matrix object, \cr
    \code{substituteNA} \tab substitutes NAs by zero, the column mean or median, \cr
    \code{interpNA} \tab interpolates NAs using R's "approx" function. }
    
}


\section{Special Daily and Monthly Operations}{

    Note, a daily time series is not necessarily a series which has only 
    position dates, rather it should mean that we have not more than one 
    record per day. The availability of daily data, e.g. end of day data, 
    is in real time usually at the same local time every working day, but 
    this may be influenced due to holidays or due to the fact that you 
    get the data from a financial center located in another time zone than 
    your own.
    
    If all all these facts can be neglected and only date positions without
    time information is relevant and we speak losely of "daily" data records.
    It is also worth to note, that Rmetrics uses in this case by default 
    the ISO-8601 format "\%Y-\%m-\%d".
    
    \tabular{ll}{   
    \code{dummyDailySeries} \tab Creates a dummy daily 'timeSeries' object, \cr
    \code{alignDailySeries} \tab Aligns a daily 'timeSeries' to new positions,\cr  
    \code{rollDailySeries} \tab Rolls daily a 'timeSeries' on a given period, \cr
    \code{ohlcDailyPlot} \tab Plots open high low close bar chart. }  
    
    
    In the case of monthly data, it is important to note that Rmetrics 
    always expects the full date for the position vector and not only
    the years ans monhts. ...
    
    
    \tabular{ll}{   
    \code{countMonthlyRecords} \tab Returns a series with monthly counts of records, \cr
    \code{isMonthly} \tab Decides if the series consists of monthly records, \cr
    \code{rollMonthlyWindows} \tab Returns start and end dates for rolling time windows, \cr        
    \code{rollMonthlySeries} \tab Rolls monthly a 'timeSeries' on a given period. }
    
}


\section{Column and Row Statistics}{

    List of Functions:
    
    colStats\cr
    rowStats
    
}

\section{Coercion of timeSeries Objects}{

    List of Functions:

    as\cr
    is.timeSeries
    
    as.timeSeries\cr
    as.timeSeries.default\cr
    as.timeSeries.numeric\cr
    as.timeSeries.data.frame\cr 
    as.timeSeries.matrix\cr
    as.timeSeries.ts\cr
    as.timeSeries.character\cr
    as.timeSeries.zoo 
    
    as.vector.timeSeries\cr
    as.matrix.timeSeries\cr
    as.data.frame.timeSeries\cr
    as.ts.timeSeries
    
}    



\keyword{package}

