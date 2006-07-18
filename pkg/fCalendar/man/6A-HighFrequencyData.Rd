\name{HighFrequencyDataTools}

\alias{HighFrequencyDataTools}

\alias{xjulian}
\alias{xdate}
\alias{xday.of.week}
\alias{xleap.year}

\alias{fxdata.contributors}
\alias{fxdata.parser}
\alias{fxdata.filter}
\alias{fxdata.varmin}

\alias{xts.log}
\alias{xts.diff}
\alias{xts.cut}
\alias{xts.interp}
\alias{xts.map}
\alias{xts.upsilon}
\alias{xts.dvs}
\alias{xts.dwh}


\title{Tools for FX High Frequency Data}


\description{
    
    A collection and description of functions for the 
    management of high frequency financial market time 
    series, especially for FX series collected from a 
    Reuters data feed. The collection includes functions 
    for the management of dates and times formatted in 
    the ISO-8601 string 'CCYYMMDDhhmm', functions for 
    filter and outlier detection of high frequency FX 
    data records as collected from a Reuters data feed, 
    and functions which can be used to calculate log-prices, 
    log-returns, to extract subsamples, to interpolate 
    in time, to build business time scales, and to 
    de-seasonalize and de-volatilize high frequency 
    financial market data.
    \cr
    
    'CCYYMMDDhhmm' Dates and Times functions are:
    
    \tabular{ll}{   
    \code{xjulian} \tab Julian minute counts for 'CCYYMMDDhhmm' formats, \cr
    \code{xdate} \tab 'CCYYMMDDhhmm' from Julian minute counts, \cr
    \code{xday.of.week} \tab day of week from 'CCYYMMDDhhmm' dates/times, \cr
    \code{xleap.year} \tab Decides if 'CCYYMMDDhhmm' is a leap year or not. }            
            
    Filter and outlier detection functions are: 
    
    \tabular{ll}{       
    \code{fxdata.contributors} \tab Creates a table with contributor names, \cr  
    \code{fxdata.parser} \tab Parses FX contributors and delay times, \cr
    \code{fxdata.filter} \tab Filters price and spread values from FX records, \cr
    \code{fxdata.varmin} \tab Aggregates records to variable minutes format. }
            
    Functions for De-Seasonalization and De-Volatilization:
    
    \tabular{ll}{
    \code{xts.log} \tab Calculates logarithms for xts time series values, \cr
    \code{xts.diff} \tab Differentiates xts time series values with lag=1, \cr
    \code{xts.cut} \tab Cuts a piece out of a xts time series, \cr
    \code{xts.interp} \tab Interpolates for equidistant time steps, \cr
    \code{xts.map} \tab Creates a volatility adjusted time-mapping, \cr 
    \code{xts.upsilon} \tab Interpolates a time series in upsilon time, \cr
    \code{xts.dvs} \tab Creates a de-volatilizised time series, \cr
    \code{xts.dwh} \tab Plots intra-daily/weekly histograms. }  
                
}


\usage{
xjulian(xdates, origin = 19600101)
xdate(xjulians, origin = 19600101)
xday.of.week(xdates)
xleap.year(xdates)

fxdata.contributors(x, include = 10)
fxdata.parser(x, parser.table)
fxdata.filter(x, parameter = "strong", doprint = TRUE)
fxdata.varmin(x, digits = 4)

xts.log(xts)
xts.diff(xts)
xts.cut(xts, from.date, to.date)
xts.interp(xts, deltat = 1, method = "constant")
xts.map(xts, mean.deltat, alpha) 
xts.upsilon(xts, weekly.map = seq(from = 59, by =60, length = 168), 
    method = "constant", doplot = TRUE, \dots)
xts.dvs(xts, k, volatility, doplot = TRUE, \dots) 
xts.dwh(xts, deltat = 60, period = "weekly", dolog = TRUE, 
    dodiff = TRUE, doplot = TRUE) 
}


\arguments{

    \item{alpha}{
        the scaling exponent, a numeric value. For a random
        walk this will be 2.
        }
    \item{deltat}{
        the time in minutes between interpolated data points, 
        by default 1 minute.
        }
    \item{digits}{ 
        an integer value, the number of digits for the
        \code{BID} and \code{ASK} price. By default five.
        }
    \item{dolog, dodiff}{
        two logicals. Should the logarithm of the input data be taken?
        Should the difference of the input data be taken?
        Note, if both \code{dolog} and \code{dodiff} are set to true, 
        the input data are expected to be price values.
        }   
    \item{doplot}{
        a logical. Should a plot be displayed?
        }
    \item{doprint}{
        a logical, should the filter parameters be printed?
        }
    \item{from.date, to.date}{
        ISO-8601 start and end dates, [CCYYMMDD].
        }
    \item{include}{
        the contributors are sorted by frequency, the \code{include}
        market makers are slected, an integer value. By default 10.)
        }
    \item{k}{
        sampling frequency, an integer value. Takes values
        on a sequence of the order of 10 data points.
        }
    \item{mean.deltat}{
        the average size of the time intervals in minutes, an
        integer value.
        }
    \item{method}{
        a character string naming the interpolation method, either
        "linear" or "constant".
        }
    \item{origin}{
        the origin date of the counter, in ISO-8601 date format,
        [CCYYMMDDhhmm]. By default January 1st, 1960.
        }
    \item{parameter}{
        a character string, either \code{strong} or \code{weak}
        denoting the filter parameter settings.
        }
    \item{parser.table}{
        the table of contributors produced by \code{fxdata.contributors},
        a data.frame. In this table market leaders are marked.
        }
    \item{period}{
        a string, either "weekly", "daily" or "both" selecting the 
        type of the histogram. By default "weekly".
        }
    \item{volatility}{
        average volatility, a numeric value. Takes values
        of the order of the variance of the time series data.
        }
    \item{weekly.map}{
        an integer vector of time intervals, by default 168
        hourly intervals, spanning one week. Volatility
        based maps can be created by the function \code{xts.map}.
        }
    \item{x}{
        a 6 column standardized FX data frame with XDATE, DELAY, 
        CONTRIBUTOR, BID, ASKL and FLAG fields.
        }
    \item{xdates}{
        a numeric vector of ISO-8601 formatted Gregorian dates/times, \cr
        [CCYYMMDDhhmm].
        }
    \item{xjulians}{
        a numeric vector of Julian Minute Counts.
        }
    \item{xts}{
        a list with date/time \code{t} in ISO-8601
        format, [CCYYMMDDhhmm], and data values \code{x}.
        }
    \item{\dots}{
        arguments to be passed.
        }   
                
}


\value{
    
    \bold{Date and Time Functions:}
    \cr
    
    \code{xjulian}
    \cr
    returns a numeric vector of Julian minute counts.
    \cr
    
    \code{xdates}
    \cr
    returns a numeric vector of ISO-8601 formatted dates/times, i.e.
    [CCYYMMDDhhmm].
    \cr
    
    \code{xday.of.week}
    \cr
    returns a numeric vector with entries  between \code{0} (Sunday) and 
    \code{6} (Saturday).
    \cr
    
    \code{xleap.year}
    \cr
    returns a logical vector with entries TRUE or FALSE, weather the date 
    falls in a leap year or not.
    \cr
    
    
    \bold{Filter and Outlier Detection:}
    \cr
    
    \code{fxdata.contributors}
    \cr
    returns a dataframe with the following columns: \code{CONTRIBUTOR}, 
    the code naming the contributor, a character string; \code{COUNTS}, 
    the counts, how often the contributor appeared in the file, an integer;
    \code{PERCENT}, the same in percent, a numeric value; \code{SELECT}, 
    a logical. If TRUE the contributor belongs to the group of the 
    \code{n} market makers, otherwise not.
    \cr
    
    \code{fxdata.parser}, \code{fxdata.filter}
    \cr
    return a data frame with the same structure as \code{x}, i.e. a 
    standardized FX high frequencey data file structure.
    \cr
    
    \code{fxdata.varmin}
    \cr
    return a data frame with the same structure as \code{x}, i.e. a 
    standardized FX high frequencey data file structure. The second column 
    named \code{DELAY} is not used and set to zero for each data record. The 
    third column \code{CONTRIBUTOR} is set to "MEAN", the method how the 
    variable minute record was evaluated. The last column \code{FLAG}
    count the number of values from which the variable minute data 
    record was evaluated.
    \cr
    
    
    \bold{De-seasonalization and de-volatilization:}
    \cr
    
    All functions beside \code{xts.map} and \code{xts.dwh} return a
    list with the following two components:
    \code{t}, the date/time in ISO8601 format, [CCYYMMDDhhmm], the same
    as the input data \code{xts$t},
    \code{x}, the logarithmic values of the input data records \code{xts$x}, 
    a numeric vector.
    \cr

    \code{xts.map}
    \cr
    returns list with the following two components:
    \code{xmap}, a numeric vector with the time intervals,
    \code{ymap}, a numeric vector, the values to be mapped.
    \cr

    \code{xts.dws}
    \cr 
    If \code{daily} was selected, a list with the following two 
    components is returned: 
    \code{td}, the daily histogram breaks,
    \code{xd}, the daily histogram freqencies.\cr
    If \code{weekly} was selected, a list with the following two 
    components is returned: 
    \code{tw}, the weekly histogram breaks,
    \code{xw}, the weekly histogram freqencies,\cr
    If \code{both} was selected, a list with all four components 
    is returned.
    
    
}


\details{

    \bold{Date and Time Functions:}
    \cr\cr
    Note, that the \code{x*} indicates "extended" Date format including 
    Time management functionality, whereas in \code{sjulian}, \code{sdate}, 
    etc. the \code{s*} indicates "standard" or "simple" Date format, 
    handling days, months years and centuries.
    \cr\cr
    
    \bold{The Data Preprocessing Process:}
    \cr\cr
    \code{fxdata.contributors} creates a contributor list from a 
    FX high frequency from a Reuters data feed and marks the market 
    leaders. \code{fxdata.parser} selects with the information from
    the contributors list data records from market leaders. As input
    serves a standardized high frequency data file. Then the function
    \code{fxdata.filter} filters the FX data records and finally
    the function \code{fxdata.varmin} creates a "variable 
    minutes" formatted data file, i.e. all data records within the same 
    minute are averaged. The preprocessed data are the starting point
    for further investiuagtions.
    \cr
    
    \bold{The Standardized FX high frequency data file structure:}
    \cr\cr
    \code{x} is a standardized data frame with 6 columns. The first 
    column gives the dates/time \code{XDATE} in ISO-8601 format 
    [CCYYMMDDhhmm], the second column is a measure for the feed 
    \code{DELAY}, the third column denotes the \code{CONTRIBUTOR} 
    code, the fifth and six columns are the \code{BID} and \code{ASK} 
    price, and the last column is an information \code{FLAG}, to 
    add additional information. 
    \cr

    \bold{The Contributor List:}
    \cr\cr
    The output of the \code{fxdata.contributors} function is used 
    as input for the function 
    \code{fxdata.parser}, which allows to extract the contributors 
    marked as market makers in the output table.
    \cr
    
    \bold{The Parser:}
    \cr\cr
    The functions \code{fxdata.parser} parses the data.
    The parser table, \code{parser.table}, is a data frame with 4 
    columns: \code{CONTRIBUTOR} denotes a code naming the contributor, 
    \code{COUNTS} gives the number of counts, how often the contributor 
    appeared in the file, \code{PERCENT} the same as a percent value,
    \code{SELECT} denotes a logical valie, if TRUE the contributor 
    belongs to the group of the market makers, otherwise not.
    \cr
    
    \bold{Variable Minutes Formatted Files:}
    \cr\cr
    The function \code{fxdata.varmin}
    creates data records within a variable minutes format.
    \cr
    
    \bold{Log Prices and Log Returns:}
    \cr\cr
    The function \code{xts.log} is mainly used to create log-prices from 
    high frequency price records and the function \code{xts.diff}
    is used to create log-returns. 
    \cr
  
    \bold{Subsamples:}
    \cr\cr
    Th function \code{xts.cut} is mainly used to create a subsample from
    data records. If the start and/or end date are out of the time range 
    the time series is simply forward/backward extrapolated with the first 
    and/or last value.
    \cr
  
    \bold{Interpolation:}
    \cr\cr
    The function \code{xts.interp} is used to interpolate data records. 
    The method allows for two different kinds of interpolations, either 
    \code{"linear"} for a linear interpolation or \code{"constant"} for 
    a constant interpolation keeping the previous value in time (left 
    value) within the interpolation region.
    \cr
  
    \bold{Business Time Maps:}
    \cr\cr
    The function \code{xts.map} is mainly used to create the time map 
    required by the 
    function \code{xts.upsilon}. Important: The argument \code{xts} must 
    start on a Monday and end on a Sunday. Use \code{xts.cut} to guarantue 
    this.
    \cr
  
    \bold{De-Seasonalization:}
    \cr\cr
    The function \code{xts.upsilon} is  used to create data records with 
    volatility 
    adjusted time steps obtained from the "upsilon time" approach. These 
    time steps can be taken from the time map crreated by the function 
    \code{xts.map}. The data records are interpolated according to this 
    time schedule.
    \cr
  
    \bold{De-Volatilization:}
    \cr\cr
    The de-volatilization algorithm is based on Zhou's approach. The
    algorithm used by the function \code{xts.dvs} reduces the sample
    frequency by keeping the variance of 
    the price changes constant, therefore the name "de-volatilization". 
    The procedure removes volatility by sampling data at different dates 
    for different times. When the market is highly volatile more data are
    sampled. Equivalently, the time is stretched. When the market is
    less volatile, less data are sampled. Equivalently, the time is
    compressed. Although the resulting subsequence has unequally space
    calendar date/time intervals, it produces an almost equally volatile
    time series. This time series is called a de-volatilized time
    series, or "dv-Series".
    \cr
  
    \bold{Daily/Weekly Historgram Plots:}
    \cr\cr
    Financial market data exhibit seasonal structures over the day or 
    week. This can be made explicit by daily or weekly histogram plots 
    of the data using the function \code{xts.dwh}.
    
}


\note{

    These functions were written originally for R Version 1.5. Only
    minor changes were made to make these functions available for
    Version 1.9. Date and time classes are outdated, but the functions
    are still working.
    
    The file \code{fdax97m.csv} is too large and therefore not part of 
    the \code{fBasics} distribution. Please contact \emph{inf@rmetrics.org}.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\references{

ISO-8601 (1988); 
    \emph{Data Elements and Interchange Formats -
    Information Interchange, Representation of Dates and Time},
    International Organization for Standardization,
    Reference Number ISO 8601, 14 pages.

Zhou B. (1995);
    \emph{Forecasting Foreign Exchange Rates Subject to De-volatilization}, 
    in: Freedman R.S., Klein A.R., Lederman J. eds., Artificial Intelligence 
    in the Capital Markets, Irwin Publishing, Chicago, p. 137--156.

Guillaume D.M., Dacorogna M.M., Dave R.R., Mueller U.A., Olsen R.B., 
    Pictet O.V. (1997); 
    \emph{From the bird's eye to the microscope: 
    a survey of new stylized facts of the intra-daily foreign 
    exchange markets}, 
    Finance and Stochastics 1, 95--129.
        
}


\examples{
## SOURCE("fCalendar.6A-HighFrequencyData")
\dontrun{
## xjulian - 
   # Julian Counts:
   # Return the number of minute counts for the last day in every 
   # month for year 2000 beginning January 1st, 2001 at 16:00:
   xjulian(c(
     20000131, 20000229, 20000331, 20000430, 20000531, 20000630,
     20000731, 20000831, 20000930, 20001031, 20001130, 20001231)*10000 + 
     1600, origin = 20000101)
   # This doesn't work in S-Plus, you get a sequence of NA's,
   # use instead:
   xjulian(c(
     200001311600, 200002291600, 200003311600, 200004301600, 200005311600, 
     200006301600, 200007311600, 200008311600, 200009301600, 200010311600, 
     200011301600, 200012311600), origin = 20000101)
     
## xdate - 
   # Convert Julian Counts to Dates:
   # Return the number of minute counts for th
   # Manage Date/Time in Extended Date/Time Format, ISO-8601
   # Date: 1973-01-01 15:30
   xjulian(197301011530)
   print(xdate(xjulian(197301011530)), digits = 9)
  
## xday.of.week -
   # Calculate the day of week for 1973-01-01 16:15
   # Compute Day of Week:
   xday.of.week(197301011615)
        
## xleap.year -
   # Check for Leap Years:
   # Falls Februar 1st, 2000 16:15 in a leap year?
   xleap.year(200002011615)    
   
## fxdata.contributors - 
   # Filter Contributors:
   # Print contributor list:
   data(usdthb)
   usdthb[1:25, ]
   # Create contributor list:
   fxdata.contributors(usdthb, include = 5)
   
## fxdata.parser - 
   # Parse Data:
   # Create a contributor list and mark the first 5 market makers:
   parser.table = fxdata.contributors(usdthb, include = 5)
   # Parse the market makers and print the first 25 entries:
   fxdata.parser(usdthb, parser.table)[1:25, ]
   
## fxdata.filter - 
   # Filter Records:
   # Filter data and plot unfiltered data:
   par(mfrow = c(2, 1))
   NumberOfRecords = length(usdthb[,1])
   NumberOfRecords
   plot(usdthb[,4], type = "l", 
        xlab = "Tick Number from Reuters THB=", 
        ylab = "100*log(Bid[n]/Bid[1])      Bid",
        ylim = c(-20,30), main="USDTHB June 1997 unfiltered")
   lines(x = c(1, NumberOfRecords), y = rep(usdthb[1, 4], 2), col = "steelblue")
   lines(-100*log(usdthb[1, 4]/usdthb[, 4]))
   lines(x = c(1, NumberOfRecords), y = c(0, 0), col = "steelblue")
   # Filter the data:
   usdthb = fxdata.filter(usdthb, parameter = "strong")
   # Quick And Dirty Time Scaling
   Records = length(usdthb$accepted[, 4])
   scale = NumberOfRecords/Records
   # Plot filtered data:
   plot(x=(1:Records)*scale, y = usdthb$accepted[, 4], type = "l", 
        xlab = "Tick Number from Reuters THB=", 
        ylab = "100*log(Bid[n]/Bid[1])      Bid", 
        ylim = c(-20, 30), main = "USDTHB June 1997 filtered")
   y = rep(usdthb$accepted[1, 4], 2)
   lines(x = c(1, NumberOfRecords), y = y, col = "steelblue")
   y = -100*log(usdthb$accepted[1, 4]/usdthb$accepted[, 4])
   lines(x = (1:Records)*scale, y = y)
   lines(x = c(1, NumberOfRecords), y = c(0, 0), col = "steelblue")
   
## fxdata.varmin - 
   # Variable Minute Records:
   # Variable Minute Records from filter accepted Data,
   # create a varmin file and print the first 25 entries:
   fxdata.varmin(usdthb$accepted, digits = 5)[1:25, ]  
   
## xts.log - 
   # Log Prices of FX Data:
   # Calculate log-prices from AUDUSD bid prices
   options(digits = 10)
   data(audusd)
   prices = list(t = audusd[,"XDATE"], x = audusd[,"BID"])
   # Print the first 25 entries:
   log.prices = xts.log(prices)
   data.frame(log.prices)[1:25, ]
   
## xts.diff - 
   # Returns of FX Data:
   # Calculate one hourly AUDUSD log-returns
   prices = list(t = audusd[,"XDATE"], x = audusd[,"BID"])
   # Calculate the returns and print the first 25 entries:
   data.frame(xts.diff(xts.log(prices)))[1:25, ]
   
## xts.cut - 
   # Cut out a Piece From a FX File:
   # Retrieve the AUDUSD bid quotes for October 21, 1997, 16:00 
   prices = list(t = audusd[,"XDATE"], x = audusd[,"BID"])
   # Retrieve prices and print the first 25 entries:
   data.frame(xts.cut(prices, from.date = 19971021, 
     to.date = 19971021))[1:25, ]

## xts.interp - 
   # Interpolate of FX Data:
   # Interpolate AUDUSD bid prices 
   # on a 15 minutes  time scale for October 21, 1997:
   prices = list(t = audusd[,"XDATE"], x = audusd[,"BID"])
   # Interpolate the prices and print the first 25 entries:
   data.frame(xts.interp(prices, deltat = 15))[1:25, ]
   
## xts.map - 
   # Create Business Time Map:
   options(object.size = 5e8)
   par(mfrow = c(2, 1))
   # Load and plot prices:
   data(fdax9710)
   index = list(t = fdax9710[,"XDATE"], x = fdax9710[,"FDAX"])  
   # Start on Monday - end on Sunday, 3 weeks:
   index = xts.cut(index, from.date=19971006, to.date=19971026)
   plot(index$x, type = "l", xlab = "Prices", main = "Prices in event time")   
   # Create hourly upsilon time map - start on Monday - end on Sunday:
   tmap = xts.map(index, mean.deltat = 60, alpha = 1.05)
   plot(x = tmap$xmap, y = tmap$ymap, ylim = c(0, max(tmap$x)), type="l", 
     main = "Time Mapping")   
   tmap 
   
## xts.upsilon -  
   # De-seasonalize in Upsilon Time:
   index = list(t = fdax9710[,"XDATE"], x = fdax9710[,"FDAX"])  
   # Start on Monday - end on Sunday, 3 weeks:
   index = xts.cut(index, from.date = 19971006, to.date = 19971026)
   plot(index$x, type = "l", xlab = "Prices", main = "Prices in event time")   
   # Create hourly upsilon time map - start on Monday - end on Sunday:
   tmap = xts.map(index, mean.deltat = 60, alpha = 1.05)
   # Extract data records according to time map:
   index.ups = xts.upsilon(index, weekly.map = tmap$ymap, 
     main="Prices in Upsilon time")
    
## xts.dvs - 
   # De-volatilize Time Series:
   index = list(t=fdax9710[,"XDATE"], x=fdax9710[,"FDAX"])  
   # Start on Monday - end on Sunday, 3 weeks:
   index = xts.cut(index, from.date=19971006, to.date=19971026)
   plot(index$x, type = "l", ylab = "Prices", main = "Prices in event time")    
   # Devolatilize Time Series With dv-Series Algorithm:
   index.dvs = xts.dvs(index, k = 8, 
     volatility = 13.15*var(diff(log(index$x))), main = "Prices from dv-series") 

## xts.dws -
   # Plot daily/weekly Charts:
   # NOTE:
   # The file \code{fdax97m.csv} is too large and therefore not part 
   # of  this distribution. Please contact \emph{inf@rmetrics.org}.
   data(fdax97m)
   xts = list(t = fdax97m[,"XDATE"], x = fdax97m[,"FDAX"])
   # Start on Monday - end on Sunday, 3 weeks:
   xts = xts.cut(index, from.date = 19970106, to.date = 19971228)
   # Create Daily and Weekly Histograms:
   result = xts.dwh (xts, period = "both", dolog = TRUE, 
     dodiff = TRUE, deltat = 30, doplot = TRUE)     
}
}


\keyword{data}

