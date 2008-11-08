\name{seriesPlot}


\alias{seriesPlot}


\alias{assetsReturnPlot}
\alias{assetsCumulatedPlot}
\alias{assetsSeriesPlot}
\alias{assetsHistPlot}
\alias{assetsLogDensityPlot}
\alias{assetsQQNormPlot}

\alias{assetsRiskReturnPlot}
\alias{assetsNIGShapeTrianglePlot}

\alias{assetsHistPairsPlot} 



\title{Assets Series Plots}


\description{
  
    A collection and description of functions which 
    display several different kind of views on
    multivariate data sets of assets.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    
    % TIME SERIES ASSETS PLOTS:
    \code{assetsReturnPlot} \tab Displays return series of individual assets, \cr
    \code{assetsCumulatedPlot} \tab Displays cumulated returns of individual assets, \cr
    \code{assetsSeriesPlot} \tab Displays time series of individual assets, \cr
    \code{assetsHistPlot} \tab Displays histograms of individual assets, \cr
    \code{assetsLogDensityPlot} \tab Displays a pdf plot on logarithmic scale, \cr
    \code{assetsQQNormPlot} \tab Displays normal qq-plots of individual assets, \cr
    
    % MULTIVARIATE RISK PLOTS:
    \code{assetsRiskReturnPlot} \tab Displays risk-return giagram of assets, \cr
    \code{assetsNIGShapeTrianglePlot} \tab Displays NIG Shape Triangle, \cr
    
    % BIVARIATE HISTOGRAM PLOTS::
    \code{assetsHistPairsPlot} \tab Displays bivariate Histogram Plot. }
}


\usage{
assetsReturnPlot(x, col = "steelblue", \dots)
assetsCumulatedPlot(x, col = "steelblue", \dots)
assetsSeriesPlot(x, col = "steelblue", ...)
assetsHistPlot(x, col = "steelblue", skipZeros = FALSE, \dots)
assetsLogDensityPlot(x, estimator = c("hubers", "sample", "both"), 
    labels = TRUE, \dots)
assetsQQNormPlot(x, col = "steelblue", skipZeros = FALSE, \dots)

assetsRiskReturnPlot(x, col = "steelblue", percentage = FALSE, scale = 252, 
    labels = TRUE, add = TRUE, \dots)
assetsNIGShapeTrianglePlot(x, labels, col = "steelblue", \dots)

assetsHistPairsPlot(x, bins = 30, method = c("square", "hex"), ...) 
}


\arguments{
  
    \item{add}{
        a logical flag, defining the color to fill the boxes.
        }
    \item{bins}{
        an integer value, the number of bins used for the biariate 
        histogram.
        }
    \item{col}{
        a character string, defining the color to fill the boxes.
        }
    \item{estimator}{
        a character string naming the type of estimator to fit the mean 
        and variance of the normal density. This may be either \code{"huber"}, 
        \code{sample"}, or \code{"both"}.
        }    
    \item{labels}{
        a logical flag, if \code{TRUE} then default labels will be used,
        otherwise the plots will be displayed without labels and the user
        can add his own labels.
        }
    \item{method}{
        a character string denoting whic h type of binning should be
        used, either \code{"squared"} or \code{"hexagonal"}.
        }
    \item{percentage}{
        a logical flag. Are the returns given by log or percentual log
        returns?
        }
    \item{scale}{
        an integer value, the scale, i..e number of days, in a year. Used
        by daily data sets.
        }    
    \item{skipZeros}{
        a logical,  should zeros be skipped in the histogram plot of the
        return series ?
        }
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{\dots}{
        optional arguments to be passed.
        }       
        
}


%\references{}



\seealso{

    \code{MultivariateDistribution}.

}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP2005REC -
   x = as.timeSeries(data(LPP2005REC))
   y = x[, "LPP40"]
   head(x)
   
## assetsReturnPlot -
   assetsReturnPlot(y)
   
## assetsCumulatedPlot -
   assetsCumulatedPlot(y)
   
## assetsSeriesPlot -
   assetsSeriesPlot(y)
   
## assetsHistPlot -
   assetsHistPlot(y)
   
## assetsLogDensityPlot -
   assetsLogDensityPlot(y)
   
## assetsQQNormPlot -
   assetsQQNormPlot(y)
   
## assetsRiskReturnPlot -
   assetsRiskReturnPlot(x)
   
## assetsNIGShapeTrianglePlot -
   assetsNIGShapeTrianglePlot(x)
   
## assetsHistPairsPlot -
   assetsHistPairsPlot(x[, c("LMI", "ALT")])
}

\keyword{models}
