\name{acfPlot}


\alias{acfPlot}
\alias{pacfPlot}
\alias{teffectPlot}

\alias{lacfPlot}


\title{Autocorrelation Function Plots}


\description{

    Returns plots of autocorrelations including
    the autocorrelation function ACF, the partial
    ACF, the lagged ACF, and the Taylor effect plot.
    \cr

    The functions to display stylized facts are:
    
    \tabular{ll}{
    \code{acfPlot} \tab autocorrelation function plot, \cr
    \code{pacfPlot} \tab partial autocorrelation function plot, \cr
    \code{lacfPlot} \tab lagged autocorrelation function plot, \cr
    \code{teffectPlot} \tab Taylor effect plot.}
     
}


\usage{
acfPlot(x, labels = TRUE, \dots)
pacfPlot(x, labels = TRUE, \dots) 

lacfPlot(x, n = 12, lag.max = 20, type = c("returns", "values"),
    labels = TRUE, \dots)

teffectPlot(x, deltas = seq(from = 0.2, to = 3, by = 0.2), lag.max = 10, 
    ymax = NA, standardize = TRUE, labels = TRUE, \dots)
}


\arguments{

    \item{deltas}{
        the exponents, a numeric vector, by default ranging 
        from 0.2 to 3.0 in steps of 0.2.
        }
    \item{labels}{
        a logical value. Whether or not x- and y-axes should be automatically 
        labeled and a default main title should be added to the plot.
        By default \code{TRUE}.
        }
    \item{lag.max}{
        maximum lag for which the autocorrelation should be 
        calculated, an integer.
        }
    \item{n}{
        an integer value, the number of lags.
        }
    \item{standardize}{
        a logical value. Should the vector \code{x} be standardized?
        }
    \item{type}{
        [lacf] - \cr
        a character string which specifies the type of the input
        series, either "returns" or series "values". In the case of 
        a return series as input, the required value series is 
        computed by cumulating the financial returns: 
        \code{exp(colCumsums(x))}
        }
    \item{x}{
        an uni- or multivariate return series of class \code{timeSeries} 
        or any other object which can be transformed by the function
        \code{as.timeSeries()} into an object of class \code{timeSeries}.
        }
    \item{ymax}{
        maximum y-axis value on plot, \code{is.na(ymax)} TRUE, then
        the value is selected automatically.
        }
    \item{\dots}{
        arguments to be passed.
        }

}


\value{
    
    
    \code{acfPlot}, \code{pacfplot},
    \cr
    return an object of class \code{"acf"}, see \code{\link{acf}}.
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
    
    \bold{Autocorrelation Functions:}
    \cr\cr
    The functions \code{acfPlot} and \code{pacfPlot}, plot and estimate 
    autocorrelation and partial autocorrelation function. The functions 
    allow to get a first view on correlations within the time series. 
    The functions are synonyme function calls for R's \code{acf} and 
    \code{pacf} from the the \code{ts} package.
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
    \emph{A long memory property of stock market returns and a new model},
    Journal of Empirical Finance 1, 83.
    
}


\examples{   
## data - 
   # require(MASS)
   plot(SP500, type = "l", col = "steelblue", main = "SP500")
   abline(h = 0, col = "grey")

## teffectPlot -
   # Taylor Effect:
   teffectPlot(SP500)
}


\keyword{hplot}

