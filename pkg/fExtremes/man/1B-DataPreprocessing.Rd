\name{DataPreprocessing}

\alias{DataPreprocessing}

\alias{blockMaxima}
\alias{findThreshold}
\alias{pointProcess}
\alias{deCluster}


\title{Extremes Data Preprocessing}


\description{

    A collection and description of functions for data 
    preprocessing of extreme values. This includes tools 
    to separate data beyond a threshold value, to compute 
    blockwise data like block maxima, and to decluster 
    point process data.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{blockMaxima} \tab Block Maxima from a vector or a time series, \cr 
    \code{findThreshold} \tab Upper threshold for a given number of extremes, \cr  
    \code{pointProcess} \tab Peaks over Threshold from a vector or a time series, \cr 
    \code{deCluster} \tab Declusters clustered point process data. }
    
}


\usage{
blockMaxima(x, block = c("monthly", "quarterly"), doplot = FALSE)
findThreshold(x, n = floor(0.05*length(as.vector(x))), doplot = FALSE)
pointProcess(x, u = quantile(x, 0.95), doplot = FALSE)
deCluster(x, run = 20, doplot = TRUE)
}


\arguments{

    \item{block}{
        [blockMaxima] - \cr
        the block size. A numeric value is interpreted as the number  
        of data values in each successive block. All the data is used,
        so the last block may not contain \code{block} observations. 
        If the \code{data} has a  \code{times} attribute containing (in
        an object of class \code{"POSIXct"}, or an object that can be
        converted to that class, see \code{\link{as.POSIXct}}) the
        times/dates of each observation, then \code{block} may instead
        take the character values \code{"month"}, \code{"quarter"},
        \code{"semester"} or \code{"year"}. By default monthly blocks
        from daily data are assumed.
        }
    \item{doplot}{
        a logical value. Should the results be plotted? By 
        default \code{TRUE}.
        }
    \item{n}{
        [findThreshold] - \cr
        a numeric value or vector giving number of extremes above 
        the threshold. By default, \code{n} is 
        set to an integer representing 5\% of the data from the 
        whole data set \code{x}.
        }
    \item{run}{
        [deCluster] - \cr
        parameter to be used in the runs method; any two consecutive 
        threshold exceedances separated by more than this number of 
        observations/days are considered to belong to different clusters.
        }  
    \item{u}{
        [pointProcess] - \cr
        a numeric value at which level the data are to be truncated. By 
        default the threshold value which belongs to the 95\% quantile,
        \code{u=quantile(x,0.95)}.       
        }
    \item{x}{
        [finThreshold][blocks][blockMaxima][deCluster] - \cr
        a numeric data vector from which \code{findThreshold} and 
        \code{blockMaxima} determine the threshold values and block 
        maxima values. 
        For the function \code{deCluster} the argument
        \code{x} represents a numeric vector of threshold exceedances
        with a \code{times} attribute which should be a numeric
        vector containing either the indices or the times/dates
        of each exceedance (if times/dates, the attribute should
        be an object of class \code{"POSIXct"} or an object that
        can be converted to that class; see \code{\link{as.POSIXct}}).
        }
        
}


\details{
  
    \bold{Finding Thresholds:} 
    \cr\cr
    The function \code{findThreshold} finds a threshold so that a given 
    number of extremes lie above. When the data are tied a threshold is 
    found so that at least the specified number of extremes lie above.
    \cr
    
  
    \bold{Computing Block Maxima:} 
    \cr\cr  
    The function \code{blockMaxima} calculates block maxima from a vector 
    or a time series, whereas the function
    \code{blocks} is more general and allows for the calculation of
    an arbitrary function \code{FUN} on blocks.
    \cr
    
        
    \bold{De-Clustering Point Processes:} 
    \cr\cr
    The function \code{deCluster} declusters clustered point process 
    data so that Poisson assumption is more tenable over a high threshold.

}


\value{
  
    \code{blockMaxima} 
    \cr
    returns a timeSeries object or a numeric vector of block 
    maxima data.
    
    \code{findThreshold} 
    \cr
    returns a numeric value or vector of suitable thresholds. 
    
    \code{pointProcess} 
    \cr
    returns a timeSeries object or a numeric vector of peaks over
    a threshold.
    
    \code{deCluster} 
    \cr
    returns a timeSeries object or a numeric vector for the 
    declustered point process. 

}


\author{

    Some of the functions were implemented from Alec Stephenson's 
    R-package \code{evir} ported from Alexander McNeil's S library 
    \code{EVIS}, \emph{Extreme Values in S}, some from Alec Stephenson's 
    R-package \code{ismev} based on Stuart Coles code from his book, 
    \emph{Introduction to Statistical Modeling of Extreme Values} and 
    some were written by Diethelm Wuertz.
    
}


\references{

Coles S. (2001);
    \emph{Introduction to Statistical Modelling of Extreme Values},
    Springer.
    
Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, Springer.  
    
}


\examples{ 
\dontrun{
## findThreshold -
   # Find threshold giving (at least) fifty exceedances 
   # for Danish Fire data
   data(danish)
   findThreshold(danish, n = c(10, 50, 100))    
   
## blockMaxima -
   # Block Maxima (Minima) for the right and left tails 
   # of the BMW log returns:
   par(mfrow = c(2, 1))
   blockMaxima( as.timeSeries(data(bmwRet)), block = 65)
   blockMaxima(-as.timeSeries(data(bmwRet)), block = 65)     
 
## deCluster -
   # Decluster the 200 exceedances of a particular  
   # threshold in the negative BMW log-return data
   PP = pointProcess(-as.timeSeries(data(bmwRet)), n = 300)
   deCluster(x = PP, run = 15, doplot = TRUE) 
}
}


\keyword{programming}

