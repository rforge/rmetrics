\name{smooth}                     
                                    
                                    
\title{Smoothes Time Series Objects}


\alias{smoothLowess}
\alias{smoothSpline}
\alias{smoothSupsmu}


\description{
    
    Smoothes a 'timeSeries' object.      
    
}
    
    
\usage{
smoothLowess(x, f = 0.5, \dots)
smoothSpline(x, spar = NULL, \dots)
smoothSupsmu(x, bass = 5, \dots)
}


\arguments{

    \item{x}{
        an univariate 'timeSeries' object.
        }
    \item{f}{
        the lowess smoother span. This gives the proportion of 
        points in the plot which influence the smooth at each 
        value. Larger values give more smoothness.
        }
    \item{spar}{
        smoothing parameter, typically (but not necessarily) in 
        (0,1]. By default \code{NULL}, i.e. the value will be
        automatically selected.
        }
    \item{bass}{
        controls the smoothness of the fitted curve. Values of 
        up to 10 indicate increasing smoothness.
        }
    \item{\dots}{
        optional arguments to be passed to the underlying smoothers.
        }
        
}


\details{

    The functions \code{smoothLowess}, \code{smoothSpline}, \code{smoothSupsmu}
    allow to smooth \code{timeSerie} object. The are interfaces to the
    function \code{lowess}, \code{supmsu}. and \code{smooth.spline} in R's
    \code{stats} package. 
   
    The \code{\dots} arguments allow to pass optional arguments to the 
    underlying \code{stats} functions and tailor the smoothing process. 
    We refer to the manual pages of these functions for a proper setting
    of these options.
   
}   


\value{

    returns a bivariate 'timeSeries' object, the first column holds the original
    time series data, the second the smoothed series.
        
}


   
\author{

    The R core team for the underlying smoother functions.
     
}


\examples{
## Use Close from MSFT's Price Series  - 
   head(MSFT)
   MSFT.CLOSE <- MSFT[, "Close"]
   head(MSFT.CLOSE)
   
## Plot Original and Smoothed Series by Lowess - 
   MSFT.LOWESS <- smoothLowess(MSFT.CLOSE, f = 0.1)
   head(MSFT.LOWESS)
   plot(MSFT.LOWESS)
   title(main = "Close - Lowess Smoothed")
   
## Plot Original and Smoothed Series by Splines - 
   MSFT.SPLINE <- smoothSpline(MSFT.CLOSE, spar = 0.4)
   head(MSFT.SPLINE)
   plot(MSFT.SPLINE)
   title(main = "Close - Spline Smoothed")
   
## Plot Original and Smoothed Series by Supsmu - 
   MSFT.SUPSMU <- smoothSupsmu(MSFT.CLOSE)
   head(MSFT.SUPSMU)
   plot(MSFT.SUPSMU)
   title(main = "Close - Spline Smoothed")
}


\keyword{chron}

