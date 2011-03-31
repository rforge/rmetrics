\name{timeSeries-package}


\alias{timeSeries-package}


\docType{package}


\title{Utilities and Tools Package}


\description{

    Package of time series tools and utilities.
    
}


\details{

    \tabular{ll}{
        Package:    \tab timeSeries\cr
        Type:       \tab Package\cr
        Version:    \tab see description file\cr
        Date:       \tab 2011\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2011 Rmetrics Association\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }
    
}
  
  

\section{timeSeries - S4 timeSeries Class}{

    
    List of Functions:
    
    \tabular{ll}{
    \code{timeSeries} \tab Creates a 'timeSeries' from scratuch, \cr
    \code{description} \tab ..., \cr
    \code{finCenter} \tab ..., \cr
    \code{getDataPart} \tab ..., \cr
    \code{isReguar} \tab ..., \cr
    \code{isUnivariate} \tab ..., \cr
    \code{series} \tab ... .
    } 
    
}


\section{methods - Time Series Methods}{

    List of Functions:
    
    \tabular{ll}{
    \code{as} \tab ..., \cr
    \code{base} \tab ..., \cr
    \code{comment} \tab ..., \cr
    \code{is} \tab ..., \cr
    \code{mathOps} \tab ..., \cr
    \code{plot} \tab ..., \cr
    \code{show} \tab ..., \cr
    \code{stats} \tab ... .
    }
    
    
    mathOps - List of Functions:
    
    \tabular{ll}{  
    \code{Ops.timeSeries} \tab S3: Arith method for a 'timeSeries' object, \cr
    \code{abs} \tab Returns absolute values of a 'timeSeries' object, \cr
    \code{sqrt} \tab Returns square root of a 'timeSeries' object, \cr
    \code{exp} \tab Returns the exponential values of a 'timeSeries' object, \cr
    \code{log} \tab Returns the logarithm of  a 'timeSeries' object, \cr
    \code{sign} \tab Returns the signs of a 'timeSeries' object, \cr
    \code{diff} \tab Differences a 'timeSeries' object, \cr 
    \code{scale} \tab Centers and/or scales a 'timeSeries' object, \cr
    \code{quantile} \tab Returns quantiles of an univariate 'timeSeries'. 
    }
    
    
    na.omit - List of Functions:
    
    \tabular{ll}{  
    \code{na.omit} \tab Handles NAs in a timeSeries object, \cr
    \code{removeNA} \tab removes NAs from a matrix object, \cr
    \code{substituteNA} \tab substitutes NAs by zero, the column mean or median, \cr
    \code{interpNA} \tab interpolates NAs using R's "approx" function. }
    

}


\section{base - Basic Time Series Functions}{

    List of Functions:
    
    \tabular{ll}{
    \code{apply} \tab ..., \cr
    \code{attach} \tab ..., \cr
    \code{cbind} \tab ..., \cr
    \code{diff} \tab ..., \cr
    \code{dim} \tab ..., \cr
    \code{extract} \tab ..., \cr
    \code{merge} \tab ..., \cr
    \code{rank} \tab ..., \cr
    \code{rev} \tab ..., \cr
    \code{sample} \tab ..., \cr
    \code{scale} \tab ..., \cr
    \code{sort} \tab ..., \cr
    \code{start} \tab ..., \cr
    \code{subset} \tab ..., \cr
    \code{t} \tab ..., \cr
    
    \code{colCumsums} \tab ..., \cr
    \code{colSums} \tab ..., \cr
    \code{rowCumsums} \tab ... .
    }

}


\section{fin - Financial Time Series Functions}{

    
    List of Functions:
    
    \tabular{ll}{ 
    \code{align} ...,\cr 
    \code{cumulated} \tab compute cumulated series from a returns, \cr
    \code{daily} ..., \cr
    \code{drawdowns} \tab compute series of drawdowns from financial returns,\cr
    \code{durations} \tab compute durations from a financial time series,\cr
    \code{miquotes} \tab compute mid quotes from a price/index stream,\cr
    \code{periodicals} ..., \cr
    \code{returns} \tab Compute returns from prices or indexes, \cr
    \code{runlengths} ..., \cr
    \code{smooth} ..., \cr
    \code{splits} ..., \cr
    \code{spreads} \tab compute spreads from a price/index stream, \cr
    \code{turns} ... , \cr
    
    \code{orderColnames} ..., \cr
    \code{orderStatistics} ..., \cr
    \code{rollMean} ..., \cr
    }

}


\section{more ...}{

    
    Daily and Monthly - List of Functions:
    
    \tabular{ll}{   
    \code{dummyDailySeries} \tab Creates a dummy daily 'timeSeries' object, \cr
    \code{alignDailySeries} \tab Aligns a daily 'timeSeries' to new positions,\cr  
    \code{rollDailySeries} \tab Rolls daily a 'timeSeries' on a given period, \cr
    \code{ohlcDailyPlot} \tab Plots open high low close bar chart, \cr
    \code{countMonthlyRecords} \tab Returns a series with monthly counts of records, \cr
    \code{isMonthly} \tab Decides if the series consists of monthly records, \cr
    \code{rollMonthlyWindows} \tab Returns start and end dates for rolling time windows, \cr        
    \code{rollMonthlySeries} \tab Rolls monthly a 'timeSeries' on a given period. 
    }
    

    Column/Row Statistics - List of Functions:
    
    \tabular{ll}{
    \code{colStats} \tab ..., \cr
    \code{rowStats} \tab ... .
    }
    

    Coercion of timeSeries Objects - List of Functions:

    \tabular{ll}{
    \code{as}\cr
    \code{is.timeSeries} \tab ..., \cr  
    \code{as.timeSeries} ..., \cr
    
    \code{as.timeSeries.default} ...,  \cr
    \code{as.timeSeries.numeric} ..., \cr
    \code{as.timeSeries.data.frame} ..., \cr 
    \code{as.timeSeries.matrix} ..., \cr
    \code{as.timeSeries.ts} ..., \cr
    \code{as.timeSeries.character} ..., \cr
    \code{as.timeSeries.zoo} ..., \cr 
    
    \code{as.vector.timeSeries} ..., \cr
    \code{as.matrix.timeSeries} ..., \cr
    \code{as.data.frame.timeSeries} ..., \cr
    \code{as.ts.timeSeries} ... .
    }
    
}    


\keyword{package}

