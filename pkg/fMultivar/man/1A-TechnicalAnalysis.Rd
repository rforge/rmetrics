\name{TechnicalAnalysis}

\alias{TechnicalAnalysis}

\alias{emaTA}
\alias{biasTA}
\alias{medpriceTA}
\alias{typicalpriceTA}
\alias{wcloseTA}

\alias{rocTA}
\alias{oscTA}
\alias{momTA}
\alias{macdTA}
\alias{cdsTA}

\alias{cdoTA}
\alias{vohlTA}
\alias{vorTA}

\alias{stochasticTA}
\alias{fpkTA}
\alias{fpdTA}
\alias{spdTA}
\alias{apdTA}

\alias{wprTA}
\alias{rsiTA}

\alias{accelTA}
\alias{adiTA}
\alias{adoscillatorTA}
\alias{bollingerTA}
\alias{chaikinoTA}
\alias{chaikinvTA}
\alias{garmanklassTA}
\alias{nviTA}
\alias{obvTA}
\alias{pviTA}
\alias{pvtrendTA}
\alias{williamsadTA}
\alias{williamsrTA}

\alias{SMA}
\alias{EWMA}


\title{Tools for the Technical Analysis}


\description{

    A collection and description of functions 
    for the technical analysis of stock markets. 
    The collection provides a set of the most 
    common technical indicators.
    \cr
    
    Utility Functions:
    
    \tabular{ll}{
    \code{emaTA} \tab Exponential Moving Average, \cr
    \code{biasTA} \tab Bias Indicator, \cr
    \code{medpriceTA} \tab Medium Price Indicator, \cr
    \code{typicalpriceTA} \tab Typical Price Indicator, \cr
    \code{wcloseTA} \tab Weighted Close Indicator, \cr
    \code{rocTA} \tab Rate of Change, \cr
    \code{oscTA} \tab Oscillator Indicator. }
    
    Oscillator Indicators:
    
    \tabular{ll}{
    \code{momTA} \tab Momentum Indicator, \cr
    \code{macdTA} \tab MACD Indicator, \cr
    \code{cdsTA} \tab MACD Signal Line, \cr
    \code{cdoTA} \tab MACD Oscillator, \cr
    \code{vohlTA} \tab High/Low Volatility, \cr
    \code{vorTA} \tab Volatility Ratio. }
    
    
    \tabular{ll}{
    \code{stochasticTA} \tab Stochastics Oscillator, \cr
    \code{fpkTA} \tab Fast Percent K, \cr
    \code{fpdTA} \tab Fast Percent D, \cr
    \code{spdTA} \tab Slow Percent D, \cr
    \code{apdTA} \tab Averaged Percent D, \cr
    \code{wprTA} \tab William's Percent R, \cr
    \code{rsiTA} \tab Relative Strength Index. }
    
    S-Plus Like Moving Averages:
    \tabular{ll}{
    \code{SMA} \tab Simple Moving Average, \cr       
    \code{EWMA} \tab Exponentially Weighted  Moving Average.}
    
    
}


\usage{
emaTA(x, lambda, startup = 0)
biasTA(x, lag)
medpriceTA(high, low)
typicalpriceTA(high, low, close)
wcloseTA(high, low, close)
rocTA(x, lag)
oscTA(x, lag1 = 25, lag2 = 65)

momTA(x, lag)
macdTA(x, lag1, lag2)
cdsTA(x, lag1 = 12, lag2 = 26, lag3 = 9)
cdoTA(x, lag1 = 12, lag2 = 26, lag3 = 9)
vohlTA(high, low)
vorTA(high, low)

stochasticTA(close, high, low, lag1, lag2, type = c("fast", "slow")) 
fpkTA(close, high, low, lag)
fpdTA(close, high, low, lag1, lag2)
spdTA(close, high, low, lag1, lag2, lag3)
apdTA(close, high, low, lag1, lag2, lag3, lag4)
wprTA(close, high, low, lag)
rsiTA(close, lag)

SMA(x, n = 5)
EWMA(x, lambda, startup = 0)
}


\arguments{

    \item{lag, lag1, lag2, lag3, lag4}{
        integer values, time lags.
        }
    \item{n}{
        [SMA] - \cr
        an integer value, time lag.
        }
    \item{lambda}{
        [emaTA][EWMA] - \cr
        a numeric value between zero and one giving the decay length 
        of the exponential moving average. If an integer value greater 
        than one is given, lambda is used as a lag of "n" periods to 
        calculate the decay parameter. 
        }
    \item{startup}{
        [emaTA][EWMA] - \cr
        an integer value, the startup position of the exponential
        moving average, by default 0.
        }
    \item{type}{
        [stochasticTA] - \cr
        a character string, either \code{"fast"} or "\code{"slow"}
        characterizing the type of the percent K and percent D
        indicator. By default \code{type="fast"}
        }
    \item{x, high, low, close}{
        a numeric vector of prices, either opening, closing, or
        high and low values.
        For \code{ohlcPlot} a multivariate time series object of 
        class \code{mts}.
        }

}


\value{

    \code{*TA}
    \cr 
    The technical Indicators return the following numeric vectors (or matrix):

    \code{emaTA} returns the Exponential Moving Average, EMA \cr
    \code{biasTA} returns the EMA-Bias, \cr
    \code{medpriceTA} returns the Medium Price, \cr
    \code{typicalpriceTA} returns the Typical Price, \cr
    \code{wcloseTA} returns the Weighted Closing Price, \cr
    \code{rocTA} returns the Rate of Change Indicator, \cr
    \code{oscTA} returns the EMA Oscillator Indicator, \cr
    \code{momTA} returns the Momentum Oscillator, \cr
    
    \code{macdTA} returns the MACD Oscillator, \cr
    \code{cdsTA} returns the MACD Signal Line, \cr
    \code{cdo} returns the MACD Oscillator, \cr
    \code{vohlTA} returns the High/Low Volatility Oscillator, \cr
    \code{vorTA} returns Volatility Ratio Oscillator, \cr
    
    \code{stochasticTA} returns a 2-column matrix with percent K and D Indicator, \cr
    \code{fpkTA} returns the Fast Percent-K Stochastics Indicator, \cr
    \code{fpdTA} returns the Fast Percent-D Stochastics Indicator, \cr
    \code{spdTA} returns the Slow Percent-D Stochastics Indicator, \cr
    \code{apdTA} returns the Averaged Percent-D Stochastics Indicator, \cr
    \code{wprTA} returns the Williams Percent-R Stochastics Indicator, \cr
    \code{rsiTA} returns the Relative Strength Index Stochastics Indicator. \cr

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\examples{
## SOURCE("fMultivar.1A-TechnicalAnalysis")

## -
}
   

\keyword{math}

