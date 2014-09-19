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
        Copyright:  \tab (c) 1999-2014 Rmetrics Association\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }
    
}
  
  

\section{timeSeries - S4 timeSeries Class}{
    
    \tabular{ll}{
    \code{timeSeries} \tab Creates a 'timeSeries' from scratch\cr
    \code{getDataPart, series} \tab ... \cr
    \code{getUnits} \tab Extracts the time serie units \cr
    \code{getTime, time} \tab Extracts the positions of timestamps \cr
    \code{use: slot} \tab Extracts the format of the timestamp \cr
    \code{getFinCenter, finCenter} \tab Extracts the financial center \cr
    \code{use: slot} \tab Extracts the record IDs \cr
    \code{getTitle} \tab Extracts the title \cr
    \code{use: slot} \tab Extracts the documentation } 
   
}


\section{Base Time Series Functions}{

    \tabular{ll}{
    \code{apply} \tab Applies a function to blocks of a 'timeSeries' \cr
    \code{attach} \tab Attaches a 'timeSeries' to the search path \cr
    \code{cbind} \tab Combines columns of two 'timeSeries' objects \cr
    \code{  rbind} \tab Combines rows of two 'timeSeries' objects \cr
   %\code{comment} \tab ? ... \cr
    \code{diff} \tab Returns differences of a 'timeSeries' object \cr
    \code{dim} \tab returns dimensions of a 'timeSeries' object \cr
    \code{merge} \tab Merges two 'timeSeries' objects \cr
    \code{rank} \tab Returns sample ranks of a 'timeSeries' object \cr
    \code{rev} \tab Reverts a 'timeSeries' object \cr
    \code{sample} \tab Resamples a 'timeSeries' object \cr
    \code{scale} \tab Scales a 'timeSeries' object \cr
    \code{sort} \tab Sorts a 'timeSeries' object \cr
    \code{start} \tab Returns start date/time of a 'timeSeries' \cr
    \code{  end} \tab Returns end date/time of a 'timeSeries' \cr
    \code{t} \tab Returns the transpose of a 'timeSeries' object }

}


\section{Subsetting 'timeSeries' Objects}{

    \tabular{ll}{
    \code{.subset_} \tab Subsets 'timeSeries' objects \cr
    \code{.findIndex} \tab Index search in a 'timeSeries' object \cr
    \code{[} \tab Subsets a 'timeSeries' object \cr
    \code{[<-]} \tab Assigns values to a subset \cr
    \code{$} \tab Subsets a 'timeSeries' by column names \cr
    \code{$<-} \tab Replaces Subset by column names \cr
    \code{t} \tab Returns the transpose of a 'timeSeries' \cr
    \code{head} \tab Returns the head of a 'timeSeries' \cr
    \code{  tail} \tab Returns the tail of a time Series \cr
    \code{na.omit} \tab Handles NAs in a timeSeries object \cr
    \code{  removeNA} \tab removes NAs from a matrix object \cr
    \code{  substituteNA} \tab substitutes NAs by zero, column mean or median \cr
    \code{  interpNA} \tab interpolates NAs using R's "approx" function }
    
}


\section{Mathematical Operation}{
    
    \tabular{ll}{  
    \code{Ops.timeSeries} \tab S4: Arith method for a 'timeSeries' object \cr
    \code{abs} \tab Returns absolute values of a 'timeSeries' object \cr
    \code{sqrt} \tab Returns square root of a 'timeSeries' object \cr
    \code{exp} \tab Returns the exponential values of a 'timeSeries' object \cr
    \code{log} \tab Returns the logarithm of  a 'timeSeries' object \cr
    \code{sign} \tab Returns the signs of a 'timeSeries' object \cr
    \code{diff} \tab Differences a 'timeSeries' object \cr 
    \code{scale} \tab Centers and/or scales a 'timeSeries' object \cr
    \code{quantile} \tab Returns quantiles of an univariate 'timeSeries'}

}


\section{Methods}{

    \tabular{ll}{
    \code{as.timeSeries} \tab Defines method for a 'timeSeries' \cr
    \code{as.*.default} \tab Returns the input  \cr
    \code{as.*.ts} \tab Transforma a 'ts' object into a 'timeSeries' \cr
    \code{as.*.data.frame} \tab Transforms a 'data.frame' intp a 'timeSeries \cr
    \code{as.*.character} \tab Loads and transforms from a demo file \cr
    \code{as.*.zoo} \tab Transforms a 'zoo' object into a 'timeSeries' \cr 
    \code{as.vector.*} \tab Converts univariate timeSeries to vector \cr
    \code{as.matrix.*} \tab Converts timeSeries to matrix \cr
    \code{as.numeric.*} \tab Converts timeSeries to numeric \cr
    \code{as.data.frame.*} \tab Converts timeSeries to data.frame \cr
    \code{as.ts.*} \tab Converts timeSeries to ts \cr
    \code{as.logical.*} \tab Converts timeSeries to logical \cr
   %\code{comment} \tab ? ... \cr
    \code{is.timeSeries} \tab Tests for a 'timeSeries' object \cr
    \code{plot} \tab Displays a X-Y 'timeSeries' Plot \cr
    \code{lines} \tab Adds connected line segments to a plot \cr
    \code{points} \tab Adds Points to a plot \cr
    \code{show} \tab Prints a 'timeSeries oobject}
    
}


\section{Financial time series functions}{

    \tabular{ll}{ 
    \code{align} \tab Aligns a 'timeSeries' to time stamps \cr 
    \code{cumulated} \tab Computes cumulated series from a returns \cr
    \code{alignDailySeries} \tab Aligns a 'timeSeries' to calendarical dates \cr
    \code{  rollDailySeries} \tab Rolls a 'timeSeries daily\cr
    \code{drawdowns} \tab Computes series of drawdowns from financial returns \cr
    \code{  drawdownsStats} \tab Computes drawdowns statistics \cr
    \code{durations} \tab Computes durations from a financial time series \cr  
    \code{countMonthlyRecords} \tab Counts monthly records in a 'timeSeries' \cr
    \code{  rollMonthlyWindows} \tab Rolls Monthly windows \cr
    \code{  rollMonthlySeries} \tab Rolls a 'timeSeries' monthly \cr   
    \code{endOfPeriodSeries} \tab Returns end of periodical series \cr
    \code{  endOfPeriodStats} \tab Returns end of period statistics \cr
    \code{  endOfPeriodBenchmarks} \tab Returns period benchmarks \cr 
    \code{returns} \tab Computes returns from prices or indexes \cr
    \code{  returns0} \tab  Computes untrimmed returns from prices or indexes \cr   
    \code{runlengths} \tab Computes run lenghts of a 'timeSeries' \cr
    \code{smooth} \tab Smoothes a 'timeSeries' \cr    
    \code{splits} \tab Detects 'timeSeries' splits by outlier detection \cr
    \code{spreads} \tab Computes spreads from a price/index stream \cr
    \code{turns} \tab Computes turning points in a 'timeSeries' object \cr
    \code{  turnsStats} \tab Computes turning points statistics }

}


\section{Statistics Time Series functions}{

    \tabular{ll}{ 
    \code{colCumsums} \tab Computes cumulated column sums of a 'timeSeries' \cr
    \code{  colCummaxs} \tab Computes cumulated maximum of a 'timeSeries' \cr
    \code{  colCummins} \tab Computes cumulated minimum of a 'timeSeries' \cr
    \code{  colCumprods} \tab Computes cumulated pruduct values by column \cr
    \code{  colCumreturns} \tab Computes cumulated returns by column \cr    
    \code{colSums} \tab Computes sums of all values in each column \cr
    \code{  colMeans} \tab Computes means of all values in each column \cr
    \code{  colSds} \tab Computes standard deviations of all values in each column \cr
    \code{  colVars} \tab Computes variances of all values in each column  \cr
    \code{  colSkewness} \tab Computes skewness of all values in each column  \cr
    \code{  colKurtosis} \tab Computes kurtosis of all values in each column  \cr
    \code{  colMaxs} \tab Computes maxima of all values in each column  \cr
    \code{  colMins} \tab Computes minima of all values in each column  \cr
    \code{  colProds} \tab Computes products of all values in each column \cr
    \code{  colStats} \tab Computes statistics of all values in each column  \cr    
    \code{orderColnames} \tab Returns ordered column names of a 'timeSeries' \cr
    \code{  sortColnames} \tab Returns alphabetically sorted column names \cr
    \code{  sampleColnames} \tab Returns sampled column names of a 'timeSeries' \cr
    \code{  pcaColnames} \tab Returns PCA correlation ordered column names \cr
    \code{  hclustColnames} \tab Returns hierarchically clustered columnames \cr
    \code{  statsColnames} \tab Returns statisticall rearrange columnames \cr   
    \code{orderStatistics} \tab Computes order statistics of a 'timeSeries' object \cr   
    \code{rollMean} \tab Computes rolling means of a 'timeSeries' object \cr
    \code{  rollMin} \tab Computes rolling minima of a 'timeSeries' object \cr
    \code{  rollMax} \tab Computes rolling maxima of a 'timeSeries' object \cr
    \code{  rollMedian} \tab Computes rolling medians of a 'timeSeries' object \cr
    \code{  rollStats} \tab Computes rolling statistics of a 'timeSeries' objectcr \cr
    \code{rowCumsums} \tab Computes cumulated column sums of a 'timeSeries' \cr   
    \code{smoothLowess} \tab Smoothes a series with lowess function \cr
    \code{  smoothSupsmu} \tab Smoothes a series with supsmu function \cr
    \code{  smoothSpline} \tab Smoothes a series with smooth.spline function }
}


\section{Misc Functions}{

    \tabular{ll}{  
    
    \code{dummyDailySeries} \tab Creates a dummy daily 'timeSeries' object \cr
    \code{isMonthly} \tab Decides if the series consists of monthly records \cr 
   %\code{Description}  \tab Creates default description string  \cr
    \code{getArgs} \tab Extracts arguments from a S4 method }

} 


\keyword{package}

