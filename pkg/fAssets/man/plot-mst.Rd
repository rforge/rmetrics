\name{treePlot}

\alias{treePlot}

\alias{assetsTreePlot}


\title{Assets Tree Plot}


\description{
  
    Creates and displays a minimum spanning tree of assets.
 
}


\usage{    
assetsTreePlot(x, labels = TRUE, title = TRUE, box = TRUE,
    method = "euclidian", seed = NULL, \dots)
}


\arguments{
  
    \item{x}{
        a multivariate \code{timeSeries} object. 
        } 
    \item{labels}{
        a logical flag, if \code{TRUE} then default labels will be used,
        otherwise the plots will be displayed without labels and the user
        can add his own labels.
        }
    \item{title}{
        a logical flag, should a default title be added? By default 
        \code{TRUE}.
        }  
    \item{box}{
        a logical flag, should a box be added around the plot? By default 
        \code{TRUE}.
        }
    \item{method}{
        a character string, the method used to compute the distance
        matrix, see function \code{dist}.
        } 
    \item{seed}{
        an integer value setting the seed in the computation of the 
        sample ranks.
        }
    \item{\dots}{
        optional arguments to be passed.
        }
        
}



\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## LPP2005REC -
   LPP = as.timeSeries(data(LPP2005REC))
   
## assetsTreePlot -
   # par(mfrow = c(2, 2))
   assetsTreePlot(LPP)
   # new seeds ...
   for (i in 1:3) assetsTreePlot(LPP)
}


\keyword{models}

