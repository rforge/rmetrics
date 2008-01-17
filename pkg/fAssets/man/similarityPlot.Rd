\name{similarityPlot}

\alias{similarityPlot}

\alias{assetsDendrogramPlot}
\alias{assetsCorEigenPlot}
\alias{assetsTreePlot} 


\title{Assets Similarity Plots}


\description{
  
    A collection and description of functions which 
    display several different kind of views on
    multivariate data sets of assets.
    \cr
    
    The functions are:
    
    \tabular{ll}{
   
    % SIMILARITY PLOTS:
    \code{assetsDendrogramPlot} \tab Displays hierarchical clustering dendrogram, \cr
    \code{assetsCorEigenPlot} \tab Displays ratio of the largest two eigenvalues, \cr
    \code{assetsTreePlot} \tab Displays minimum spanning tree of assets. }
}


\usage{
assetsDendrogramPlot(x, method = c(dist = "euclidian", clust = "complete"))
assetsCorEigenPlot(x, method = c("pearson", "kendall", "spearman"), \dots)
assetsTreePlot(x, method = "euclidian", seed = NULL)
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{method}{
        [assetsCorgramPlot] - \cr
        a character string, the type of graph used in the lower panel.\cr
        [assetsCorEigenPlot] - \cr
        a character string, the method used to compute the correlation
        matrixs, ee function \code{cor}.\cr
        [assetsTreePlot] - \cr
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
        
\seealso{

    \code{MultivariateDistribution}.

}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP2005REC -
   x = as.timeSeries(data(LPP2005REC))
   head(x)
   
## assetsDendrogramPlot -
   assetsDendrogramPlot(x)

## assetsCorEigenPlot -
   assetsCorEigenPlot(x)

## assetsTreePlot - 
   assetsTreePlot(x)
}


\keyword{models}

