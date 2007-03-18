\name{LongRangeDependence}

\alias{LongRangeDependence}

\alias{fgnTrueacf}
\alias{fgnTruefft}
\alias{fgnStatsSlider}

\alias{farimaTrueacf}
\alias{farimaTruefft}
\alias{farimaStatsSlider}


\title{Statistics of the True LRD Process}


\description{

    A collection and description of functions 
    to investigate the true statistics of the
    long range dependence or long memory behavior 
    of an univariate time series process. 
    \cr
    
    The Functions are:
    
    Functions to model the true autocorrelation function and spectrum:
    
    \tabular{ll}{ 
    \code{fgnTrueacf} \tab Returns true FGN covariances, \cr
    \code{fgnTruefft} \tab returns true FGN fast Fourier transform, \cr  
    \code{fgnStatsSlider} \tab returns a plot of true FGN Statistics, \cr  
    \code{farimaTrueacf} \tab returns true FARIMA covariances, \cr
    \code{farimaTruefft} \tab returns true FARIMA fast Fourier transform, \cr
    \code{farimaStatsSlider} \tab returns a plot of true FARIMA Statistics. }
    
}


\usage{   
fgnTrueacf(n = 100, H = 0.7) 
fgnTruefft(n = 100, H = 0.7) 
fgnStatsSlider()

farimaTrueacf(n = 100, H = 0.7) 
farimaTruefft(n = 100, H = 0.7)
farimaStatsSlider()
}


\arguments{
    
    \item{H}{
        the Hurst exponent, a numeric value between 0.5 and 1,
        by default 0.7.
        }
    \item{n}{
        number of data points to be simulated, a numeric value, 
        by default 100.
        }

}


\value{   

    \code{fgnTrueacf}\cr
    \code{farimaTrueacf}\cr
    return the true covariance of an FGN or FARIMA time series process.
    \cr
    
    
    \code{fgnTruefft}\cr
    \code{farimaTruefft}\cr
    return the true spectrum of an FGN or FARIMA time series process.
    \cr
    
    fgnStatsSlider()\cr
    farimStatsSlider()\cr
    interactively display the true covariance and the true spectrum
    of an FGN or FARIMA time series process.
    
   
    
}


\details{
    
    \bold{Functions to Model True Correlations and Spectrum:}
    \cr
    
    The functions \code{fgnTrueacf} and \code{farimaTrueacf} return the
    true covariances of an FGN and Gaussian FARIMA(0,d,0) time 
    series process.
    
    The functions \code{fgnTruefft} and \code{farimaTruefft} return the
    true fast Fourier transform of an FGN and Gaussian FARIMA(0,d,0) 
    time series process.
    
    The R functions are implemented from SPlus code written by
    J. Beran [1994].
   
}


\author{

    J. Beran, ported by Maechler, code as listed in the Appendix of his Book, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\references{

Beran J. (1992);
    \emph{Statistics for Long-Memory Processes},
    Chapman and Hall, New York, 1994.
    
}


\examples{
## fgnTrueacf -
   fgnTrueacf(n = 20, H = 0.8)
   
## fgnTruefft -
   fgnTruefft(n = 20, H = 0.8)
   
## farimaTrueacf -
   farimaTrueacf(n = 20, H = 0.8)
   
## farimaTruefft -
   farimaTruefft(n = 20, H = 0.8)
}


\keyword{models}

