\name{similarityPlot}

\alias{similarityPlot}


\alias{assetsDendrogramPlot}
\alias{assetsCorEigenPlot}


\title{Assets Similarity Plots}


\description{
  
    Displays plots of similariaies and dissimilarities between
    data sets of assets.

}


\usage{
assetsDendrogramPlot(x, labels = TRUE, title = TRUE, box = TRUE,
    method = c(dist = "euclidian", clust = "complete"), \dots)
    
assetsCorEigenPlot(x, labels = TRUE, title = TRUE, box = TRUE,
    method = c("pearson", "kendall", "spearman"), \dots)
}


\arguments{
  
    \item{box}{
        a logical flag, should a box be added around the plot? By default 
        \code{TRUE}.
        }
    \item{labels}{
        a logical flag, if \code{TRUE} then default labels will be used,
        otherwise the plots will be displayed without labels and the user
        can add his own labels.
        }
    \item{method}{
        [assetsCorgramPlot] - \cr
        for the function \code{assetsCorgramPlot} a character string, 
        the type of graph used in the lower panel, for the function
        \code{assetsCorEigenPlot} a character string, the method used 
        to compute the correlation matrix.\cr
        [assetsTreePlot] - \cr
        a character string, the method used to compute the distance
        matrix, see function \code{dist}.
        } 
    \item{title}{
        a logical flag, should a default title be added? By default 
        \code{TRUE}.
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


\details{ 


    \code{assetsDendrogramPlot}\cr
    displays a hierarchical clustering dendrogram, 
    
    \code{assetsCorEigenPlot}\cr
    displays ratio plot of the largest two eigenvalues.
    
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
   
## assetsDendrogramPlot -
   assetsDendrogramPlot(LPP)

## assetsCorEigenPlot -
   assetsCorEigenPlot(LPP)
}


\keyword{models}

