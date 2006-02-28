\name{StylizedFacts}

\alias{StylizedFacts}

\alias{acfPlot}
\alias{pacfPlot}
\alias{ccfPlot}
\alias{teffectPlot}
\alias{lmacfPlot}
\alias{lacfPlot}

\alias{logpdfPlot}
\alias{qqgaussPlot}
\alias{scalinglawPlot}


\title{Stylized Facts}


\description{

    A collection and description of functions to 
    investigate and to plot several stylized facts 
    of economic and financial time series. This 
    includes fat tails, autocorrelations, 
    crosscorrelations, long memory behavior, and the
    Taylor effect.
    \cr

    The functions to display stylized facts are:
    
    \tabular{ll}{
    \code{logpdfPlot} \tab logarithmic density plots, \cr
    \code{qqgaussPlot} \tab Gaussian quantile quantile plot, \cr
    \code{scalinglawPlot} \tab scaling behavior plot, \cr
    \code{acfPlot} \tab autocorrelation function plot, \cr
    \code{pacfPlot} \tab partial autocorrelation function plot, \cr
    \code{ccfPlot} \tab cross correlation function plot, \cr
    \code{lmacfPlot} \tab long memory autocorrelation function plot, \cr
    \code{lacfPlot} \tab lagged autocorrelation function plot, \cr
    \code{teffectPlot} \tab Taylor effect plot.}
    
}


\usage{
logpdfPlot(x, n = 50, doplot = TRUE, type = c("lin-log", "log-log"), \dots)
qqgaussPlot(x, span = 5, col = "steelblue4", main = "Normal Q-Q Plot", \dots)
scalinglawPlot(x, span = ceiling(log(length(x)/252)/log(2)), doplot = TRUE, \dots)
acfPlot(x, \dots)
pacfPlot(x, \dots)
ccfPlot(x, y, lag.max = max(2, floor(10*log10(length(x)))), 
    type = c("correlation", "covariance", "partial"), \dots)    
lacfPlot(x, n = 12, lag.max)
lmacfPlot(x, lag.max = max(2, floor(10*log10(length(x)))), ci = 0.95, 
    main = "ACF", doprint = TRUE)
teffectPlot(x, deltas = seq(from = 0.2, to = 3, by = 0.2), lag.max = 10, 
    ymax = NA, standardize = TRUE)
}


\arguments{

    \item{ci}{
        [lmacf] - \cr
        the confidence interval, by default 95 percent, i.e. 0.95.
        }
    \item{col}{
        a character string denoting the plot color, by default
        \code{"steelblue"}.
        }
    \item{deltas}{
        [teffectPlot] - \cr
        the exponents, a numeric vector, by default ranging 
        from 0.2 to 3.0 in steps of 0.2.
        }
    \item{doplot}{
        a logical. Should a plot be displayed?
        }
    \item{doprint}{
        a logical, should the results be printed?
        }
    \item{lag.max}{
        maximum lag for which the autocorrelation should be 
        calculated, an integer.
        }
    \item{main}{
        a character string, the title of the plot.
        }
    \item{n}{
        [lacfPlot] - \cr
        an integer, the number of lags.
        \cr
        [logpdfPlot] - \cr
        an integer, the number of break and count points.
        }
    \item{span}{
        [scalinglawPlot] - \cr
        an integer value, determines for the \code{qqgaussPlot} the 
        plot range, by default 5, and for the \code{scalingPlot} a
        reasonable number of of points for the scaling range, by
        default daily data with 252 business days per year are
        assumed.
        }
    \item{standardize}{
        a logical. Should the vector \code{x} be standardized?
        }
    \item{type}{
        a character, either \code{e} for "exceedences", \code{d} 
        for "distances", or by default \code{b} for "both", 
        selecting which plot should be displayed.
        }
    \item{x, y}{
        numeric vectors, 
        \cr
        [acfPlot][pacfPlot][ccfPlot] - \cr
        a numeric vector or matrix or a univariate or 
        multivariate (not \code{ccf}) time series object.
        }
    \item{ymax}{
        [teffectPlot] - \cr
        maximum y-axis value on plot, is.na(ymax) TRUE the 
        value is selected automatically.
        }
    \item{\dots}{
        for \code{tsPlot} one or more univariate or multivariate time 
        series, else other arguments to be passed.
        }

}


\value{
    
    \code{logpdfPlot}
    \cr
    returns a list with the following components: \code{breaks}, 
    histogram mid-point breaks; \code{counts}, histogram counts;
    \code{fbreaks}, fitted Gaussian breaks; \code{fcounts}, fitted 
    Gaussian counts.
    \cr
    
    \code{qqgaussPlot}
    \cr
    returns a Gaussian Quantile-Quantile Plot.
    \cr
      
    \code{scalingPlot}
    \cr
    returns a list with the following components: \code{exponent},
    the scaling exponent, a numeric value; \code{fit}, a list with 
    the coefficients returned by \code{lsfit}, i.e. \code{intercept} 
    and \code{X}.
    \cr
    
    \code{acfPlot}, \code{pacfplot}, \code{ccfPlot}
    \cr
    return an object of class \code{"acf"}, see \code{\link{acf}}.
    \cr

    \code{lmacfPlot}
    \cr
    returns a  list with the following elements: \code{fit}, a list 
    by itself with elements \code{Intercept} and slope \code{X},
    \code{hurst}, the Hurst exponent, both are numeric values.
    \cr
    
    \code{lacfPlot}
    returns a list with the following two elements: \code{Rho}, the
    autocorrelation function, \code{lagged}, the lagged correlations.
    \cr
    
    \code{teffectPlot}
    \cr
    returns a numeric matrix of order \code{deltas} by \code{max.lag} 
    with the values of the autocorrelations.
    
}

\details{

    
    \bold{Tail Behavior:}
    \cr\cr
    \code{logpdfPlot} and \code{qqgaussPlot} are two simple functions
    which allow a quick view on the tails of a distribution.
    The first creates a logarithmic or 
    double-logarithmic density plot and returns breaks and counts.
    For the double logarithmic plot, the negative side of the distribution 
    is reflected onto the positive axis.\cr
    The second creates a Gaussian Quantile-Quantile plot.
    \cr
    
    \bold{Scaling Behavior:}
    \cr\cr
    The function \code{scalingPlot} plots the scaling law of financial 
    time series under aggregation and returns an estimate for the scaling 
    exponent. The scaling behavior is a very striking effect of the 
    foreign exchange market and also other markets expressing a regular
    structure for the volatility. Considering the average absolute
    return over individual data periods one finds a scaling power law
    which relates the mean volatility over given time intervals
    to the size of these intervals. The power law is in many cases 
    valid over several orders of magnitude in time. Its exponent  
    usually deviates significantly from a Gaussian random walk model 
    which implies 1/2. 
    \cr
    
    \bold{Autocorrelation Functions:}
    \cr\cr
    The functions \code{acfPlot}, \code{pacfPlot}, and \code{ccfPlot}
    plots and estimate autocorrelation, ACF, partial autocorrelation, 
    PACF, and cross-covariance and cross-correlation functions, CCF. 
    The functions allow to get a first view on correlations in and 
    between time series. The functions are synonyme function 
    calls for R's \code{acf}, \code{pacf}, and \code{ccf} from the 
    the \code{ts} package.
    \cr
    
    \bold{Long Memory Autocorrelation Function:}
    \cr\cr
    The function \code{lmacfPlot} plots and estimates the 
    long memory autocorrelation function and computes from the 
    plot the  Hurst exponent of a time series. The volatility of  
    financial time series exhibits (in contrast to
    the logarithmic returns) in almost every financial market a slow
    ecaying autocorrelation function, ACF. We talk of a long memory 
    if the decay in the ACF is slower than exponential, i.e. the 
    correlation function decreases algebraically with increasing 
    (integer) lag. 
    Thus it makes sense to investigate the decay on a double-logarithmic 
    scale and to estimate the decay exponent. The function
    \code{lmacf} calculates and plots the autocorrelation function of 
    the vector \code{x}. If the time series exhibits long memory 
    behaviour, it can easily be seen as a stright line in the plot. 
    This double-logarithmic plot is displayed and a linear regression 
    fit is done from which the intercept and slope ar calculated. 
    From the slope the Hurst exponent is derived.
    \cr
        
    \bold{Taylor Effect:}
    \cr\cr
    The "Taylor Effect" describes the fact that absolute returns of
    speculative assets have significant serial correlation over long
    lags. Even more, autocorrelations of absolute returns are
    typically greater than those of squared returns. From these 
    observations the Taylor effect states, that that the autocorrelations
    of absolute returns to the the power of \code{delta}, 
    \code{abs(x-mean(x))^delta} reach their maximum at \code{delta=1}.
    The function \code{teffect} explores this behaviour. A plot is
    created which shows for each lag (from 1 to \code{max.lag}) the 
    autocorrelations as a function of the exponent \code{delta}. 
    In the case that the above formulated hypothesis is supported,
    all the curves should peak at the same value around \code{delta=1}.
    
}


\references{

Taylor S.J. (1986); 
    \emph{Modeling Financial Time Series},
    John Wiley and Sons, Chichester.

Ding Z., Granger C.W.J., Engle R.F. (1993); 
    \emph{A long memory proerty of stock market returns and a new model},
    Journal of Empirical Finance 1, 83.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.14A-StylizedFacts")

## logpdfPlot -
   xmpBasics("\nStart: log PDF Plot > ")
   # Plot the log-returns of the NYSE Composite Index
   # and compare with the Gaussian Distribution:  
   par(mfrow = c(2, 2))
   data(nyseres)
   # Extract from data.frame:
   x = nyseres[, 1]
   logpdfPlot(x, main = "log PDF Plot")
   # loglogpdfPlot -
   # Plot the log-returns of the NYSE Composite Index
   # and compare with the Gaussian Distribution:
   logpdfPlot(x, type = "log-log", main = "log-log PDF Plot")
   
## qqgaussPlot -
   xmpBasics("\nNext: QQ Normal Plot > ")
   # Create a Gaussian Quantile-Quantile plot
   # for the NYSE Composite Index log-returns:
   qqgaussPlot(x)
   
## scalinglawPlot -
   xmpBasics("\nNext: Scaling Law  Plot > ")
   # Investigate and Plot the Scaling Law
   # for the NYSE Composite Index log-returns:
   scalinglawPlot(x)
    
## acfPlot -
   xmpBasics("\nNext: Auto-Correlation Function Plot > ")
   data(EuStockMarkets)
   par(mfrow = c(2, 1))
   returns.ftse = diff(log(EuStockMarkets[,"FTSE"]))
   returns.dax = diff(log(EuStockMarkets[,"DAX"]))
   acfPlot(x = returns.ftse, main = "FTSE Autocorrelation")
   
## ccfPlot -
   xmpBasics("\nNext: Cross-Correlation Function Plot > ")
   ccfPlot(x = returns.ftse, y = returns.dax,
     main="FTSE - DAX Crosscorrelation")

## lmacfPlot -
   xmpBasics("\nNext: Long-Memory ACF Plot > ")
   # Estimate and plot the Long Memory ACF of the DAX volatilities
   # and evaluate the Hurst exponent of a time series:
   par(mfrow = c(2, 1))
   lmacfPlot(abs(returns.dax), main = "DAX")
   
## teffectPlot -
   xmpBasics("\nNext: Taylor Effect Plot > ")
   # Estimate and plot the Taylor Effect for the
   # log returns of the NYSE Compositie Index.
   teffectPlot(returns.dax)
   teffectPlot(returns.ftse)
}


\keyword{hplot}

