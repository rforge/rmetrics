\name{BenchmarkAnalysis}

\alias{BenchmarkAnalysis}

\alias{ohlcPlot}

\alias{sharpeRatio}
\alias{sterlingRatio}
\alias{maxDrawDown}

\alias{getReturns}


\title{Utilities and Benchmark Analysis}


\description{

    A collection and description of utility 
    and benchmark functions for the analysis 
    of financial markets. The collection 
    provides a set of functions for the 
    computation of returns, for the display 
    of price charts, and for benchmark measurements.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{getReturns} \tab Computes returns given a price series, \cr
    \code{ohlcPlot} \tab Plots open--high--low--close bar charts, \cr
    \code{sharpeRatio} \tab Computes Sharpe Ratio, \cr
    \code{sterlingRatio} \tab Computes Sterling Ratio, \cr
    \code{maxDrawDown} \tab Computes maximum drawdown.}
    
}


\usage{
getReturns(x, type = c("continuous", "discrete"), percentage = FALSE, 
    trim = TRUE)
    
ohlcPlot(x, xlim = NULL, ylim = NULL, xlab = "Time", ylab, col = par("col"),
    bg = par("bg"), axes = TRUE, frame.plot = axes, ann = par("ann"),
    main = NULL, date = c("calendar", "julian"), format = "\%Y-\%m-\%d",
    origin = "1899-12-30", \dots)
    
sharpeRatio(x, r = 0, scale = sqrt(250))
sterlingRatio(x)

maxDrawDown(x)
}


\arguments{
    
    \item{date, format, origin}{
        [ohlcPlot] - \cr
        date elements,\cr
        \code{date}, a string indicating the type of x axis annotation. 
        Default is calendar dates. \cr
        \code{format}, a string indicating the format of the x axis 
        annotation if \code{date == "calendar"}. For details see
        \code{\link{format.POSIXct}}. \cr
        \code{origin} an R object specifying the origin of the Julian 
        dates if \code{date == "calendar"}. Defaults to 1899-12-30 
        (Popular spreadsheet programs internally also use Julian dates 
        with this origin).
        }
    \item{percentage}{
        [getReturns] - \cr
        a logical flag. If set to \code{TRUE}, the return series will 
        be expressed in percentage points otherwise not. The default 
        is \code{FALSE}. 
        }
    \item{r}{
        [sharpeRatio] - \cr
        the risk free rate. Default corresponds to using portfolio
        returns not in excess of the riskless return.
        }
    \item{scale}{
        [sharpeRatio] - \cr
        a scale factor. Default corresponds to an annualization
        when working with daily financial time series data.
        } 
    \item{trim}{
        [getReturns] - \cr
        a logical flag. If set to \code{TRUE}, the first missing 
        observation in the return series will be removed. The 
        default is \code{TRUE}.
        }
    \item{type}{
        [getReturns] - \cr
        a character string specifying the type of returns to be 
        computed. If \code{type="continuous"}, the returns are calculated 
        as the logarithm differences; if \code{type="discrete"}, the 
        returns are calculated as percentage changes.
        } 
    \item{x}{
        a numeric vector of prices.
        For \code{ohlcPlot} a multivariate time series object of 
        class \code{mts} is required.
        }
    \item{xlim, ylim, xlab, ylab, col, bg, axes, frame.plot, ann, main}{
        [ohlcPlot] - \cr
        graphical arguments, see \code{\link{plot}},
        \code{\link{plot.default}} and \code{\link{par}}.
        }       
    \item{\dots}{
        [ohlcPlot] - \cr
        further graphical arguments passed to \code{\link{plot.window}}, 
        \code{\link{title}}, \code{\link{axis}}, and \code{\link{box}}.
        }       

}

\details{

    \bold{Open--High--Low--Close Chart:}
    \cr\cr
    Within an open--high--low--close bar chart, each bar represents
    price information for the time interval between the open and the close
    price. The left tick for each bar indicates the open price for the
    time interval. The right tick indicates the closing price for the time
    interval. The vertical length of the bar represents the price range
    for the time interval.
    The time scale of \code{x} must be in Julian dates (days since the
    \code{origin}).
    \cr
    \code{[tseries:plotOHLC]}
    \cr
    
    \bold{Sharpe and Sterling Ratios:}
    \cr\cr  
    The Sharpe ratio is defined as a portfolio's mean return in excess of
    the riskless return divided by the portfolio's standard deviation. In
    finance the Sharpe Ratio represents a measure of the portfolio's
    risk-adjusted (excess) return. 
    The Sterling ratio is defined as a portfolio's overall return divided
    by the portfolio's maximum drawdown statistic. In finance the
    Sterling Ratio represents a measure of the portfolio's risk-adjusted
    return.
    \cr
    \code{[tseries:sharpe]}
    \cr  
    
    \bold{Maximum Drawdown:}
    \cr\cr
    The maximum drawdown or maximum loss statistic is defined as the 
    maximum value drop after one of the peaks of \code{x}. For financial
    instruments the maximum drawdown represents the worst investment 
    loss for a buy--and--hold strategy invested in \code{x}.
    \cr
    \code{[tseries:maxdrawdown]}
    \cr
    
    \bold{Get Returns:}
    \cr\cr
    The function computes the return series given a financial security 
    price series. The price series may be an object of class \code{numeric}
    or a time series object. This includes objects of classes \code{"ts"},
    \code{"its"} and/or \code{"timeSeries"}.
    
}


\value{

    \code{ohlcPlot} 
    \cr
    creates an Open--High--Low--Close chart.
    
    \code{sharpeRatio}\cr
    \code{sterlingRatio} 
    \cr
    return the Sharpe or Sterling ratio, a numeric value.
    
    \code{maxDrawDown}
    \cr
    returns a list containing the following three components: 
    \code{maxDrawDown}, double representing the max drawdown or max loss 
    statistic; \code{from}, the index (or vector of indices) where the 
    maximum drawdown period starts; \code{to}, the index (or vector of 
    indices) where the max drawdown period ends.
    
    \code{getReturns}
    \cr
    returns an object of the same class as the input object \code{x}.

}


\author{

    Adrian Trapletti for the ohlcPlot,*Ratio and maxDrawDown functions, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fMultivar.1B-BenchmarkAnalysis")

\dontrun{
## ohlcPlot -
   # Plot OHLC for SP500
   # ohlcPlot(x, ylab = "price", main = instrument)
   
## sharpeRatio -
   xmpMultivar("\nStart: Sharpe Ratio > ")
   # Sharpe Ratio for DAX and FTSE:
   data(EuStockMarkets)
   dax = log(EuStockMarkets[, "DAX"])
   ftse = log(EuStockMarkets[, "FTSE"])
   # Ratios:
   sharpeRatio(dax)
   sharpeRatio(ftse)
   
## maxDrawDown -
   xmpMultivar("\nNext: Max-Draw-Downs for the DAX Index >")
   par(mfrow = c(1, 1), cex = 0.7)
   data(EuStockMarkets)
   dax = log(EuStockMarkets[, "DAX"])
   mdd = maxDrawDown(dax)
   mdd
   # Plot DAX:
   plot(dax)
   segments(time(dax)[mdd$from], dax[mdd$from],
     time(dax)[mdd$to], dax[mdd$from])
   segments(time(dax)[mdd$from], dax[mdd$to],
     time(dax)[mdd$to], dax[mdd$to])
   mid = time(dax)[(mdd$from + mdd$to)/2]
   arrows(mid, dax[mdd$from], mid, dax[mdd$to], col = 2)
   title(main = "DAX: Max Drawdown")
   
## getReturns -
   xmpMultivar("\nNext: Returns for the DAX Index >")
   par(mfrow = c(1, 1), cex = 0.7)
   data(EuStockMarkets)
   dax = log(EuStockMarkets[, "DAX"])
   plot(getReturns(dax))
   title(main = "DAX: Returns")
}
}
  

\keyword{math}

