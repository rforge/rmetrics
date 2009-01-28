\name{ExtremeIndex}

\alias{ExtremeIndex}

\alias{fTHETA}
\alias{fTHETA-class}

\alias{show,fTHETA-method}

\alias{thetaSim}

\alias{blockTheta}
\alias{clusterTheta}
\alias{runTheta}
\alias{ferrosegersTheta}

\alias{exindexPlot}
\alias{exindexesPlot}

\title{Extremal Index Estimation}


\description{
  
    A collection and description of functions to simulate
    time series with a known extremal index, and to estimate 
    the extremal index by four different kind of methods, 
    the blocks method, the reciprocal mean cluster size 
    method, the runs method, and the method of Ferro and
    Segers.
    \cr
    
    The functiona are:
    
    \tabular{ll}{
    \code{thetaSim} \tab Simulates a time Series with known theta, \cr
    \code{blockTheta} \tab Computes theta from Block Method, \cr
    \code{clusterTheta} \tab Computes theta from Reciprocal Cluster Method, \cr
    \code{runTheta} \tab Computes theta from Run Method, \cr
    \code{ferrosegersTheta} \tab Computes Theta according to Ferro and Seegers, \cr
    \code{exindexPlot} \tab Calculate and Plot Theta(1,2,3), \cr
    \code{exindexesPlot} \tab Calculate Theta(1,2) and Plot Theta(1). }
    
}


\usage{
\S4method{show}{fTHETA}(object)

thetaSim(model = c("max", "pair"), n = 1000, theta = 0.5)

blockTheta(x, block = 22, quantiles = seq(0.950, 0.995, length = 10),
    title = NULL, description = NULL)
clusterTheta(x, block = 22, quantiles = seq(0.950, 0.995, length = 10),
    title = NULL, description = NULL)
runTheta(x, block = 22, quantiles = seq(0.950, 0.995, length = 10),
    title = NULL, description = NULL)
ferrosegersTheta(x, quantiles = seq(0.950, 0.995, length = 10),
    title = NULL, description = NULL)
    
exindexPlot(x, block = c("monthly", "quarterly"), start = 5, end = NA, 
    doplot = TRUE, plottype = c("thresh", "K"), labels = TRUE, \dots)
    
exindexesPlot(x, block = 22, quantiles = seq(0.950, 0.995, length = 10), 
    doplot = TRUE, labels = TRUE, \dots)
}


\arguments{

    \item{block}{
        [*Theta] - \cr
        an integer value, the block size. Currently only integer specified
        block sizes are supported.
        \cr
        [exindex*Plot] - \cr
        the block size. Either \code{"monthly"}, \code{"quarterly"} or
        an integer value. An integer value is interpreted as the number 
        of data values in each successive block. The default value is
        \code{"monthly"} which correpsond for daily data to an approximately
        22-day periods.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{doplot}{
        a logical, should the results be plotted?
        }
    \item{labels}{
        whether or not axes should be labelled. If set to \code{FALSE}
        then user specified lables can be passed through the \code{"..."}
        argument.
        }
    \item{model}{
        [thetaSim] - \cr
        a character string denoting the name of the model. Either 
        \code{"max"} or \code{"pair"}, the first representing the 
        maximimum Frechet series, and the second the paired exponential
        series.
        }
    \item{n}{
        [thetaSim] - \cr
        an integer value, the length of the time series to be generated.
        }
    \item{object}{
        an object of class \code{"fTHETA"} as returned by the functions
        \code{*Theta}.
        }
    \item{plottype}{
        [exindexPlot] - \cr
        whether plot is to be by increasing threshold (\code{thresh}) 
        or increasing K value (\code{K}).
        }
    \item{quantiles}{
        [exindexesPlot] - \cr
        a numeric vector of quantile values.
        }
    \item{start, end}{
        [exindexPlot] - \cr
        \code{start} is the lowest value of \code{K} at which to plot 
        a point, and \code{end} the highest value; \code{K} is the
        number of blocks in which a specified threshold is exceeded.
        }
    \item{theta}{
        [thetaSim] - \cr
        a numeric value between 0 and 1 setting the value of the
        extremal index for the maximum Frechet time series. (Not used
        in the case of the paired exponential series.)
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{x}{
        a 'timeSeries' object or any other object which can be transformed
        by the function \code{as.vector} into a numeric vector. \code{"monthly"}
        and \code{"quarterly"} blocks require \code{x} to be an object of
        class \code{"timeSeries"}.
        }
    \item{\dots}{
        additional arguments passed to the plot function.
        }
        
}


\value{
  
    \code{exindexPlot}
    \cr
    returns a data frame of results with the 
    following columns: \code{N}, \code{K}, \code{un}, \code{theta2},
    and \code{theta}. A plot with \code{K} on the lower x-axis and
    threshold Values on the upper x-axis versus the extremal index
    is displayed.
  
    \code{exindexesPlot}
    \cr
    returns a data.frame with four columns: 
    \code{thresholds}, \code{theta1}, \code{theta2}, and \code{theta3}.
    A plot with quantiles on the x-axis and versus the extremal indexes
    is displayed.
    
}


\references{

Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, 
    Springer. Chapter 8, 413--429.  
    
}


\seealso{
  
    \code{hillPlot},
    \code{gevFit}. 
  
}


\author{

    Alexander McNeil, for parts of the \code{exindexPlot} function, and \cr
    Diethelm Wuertz for the \code{exindexesPlot} function.
    
}


\examples{
## Extremal Index for the right and left tails 
## of the BMW log returns:
   data(bmwRet)
   par(mfrow = c(2, 2), cex = 0.7)
   exindexPlot( as.timeSeries(bmwRet), block = "quarterly")
   exindexPlot(-as.timeSeries(bmwRet), block = "quarterly")   
   
## Extremal Index for the right and left tails 
## of the BMW log returns:
   exindexesPlot( as.timeSeries(bmwRet), block = 65)
   exindexesPlot(-as.timeSeries(bmwRet), block = 65)   
}


\keyword{hplot}

