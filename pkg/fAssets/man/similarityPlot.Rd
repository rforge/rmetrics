\name{similarityPlot}

\alias{similarityPlot}

\alias{assetsDendrogramPlot}
\alias{assetsCorEigenPlot}


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
    \code{assetsCorEigenPlot} \tab Displays ratio of the largest two eigenvalues. }
}


\usage{
assetsDendrogramPlot(x, labels = TRUE, title = TRUE, box = TRUE,
    method = c(dist = "euclidian", clust = "complete"), \dots)
    
assetsCorEigenPlot(x, labels = TRUE, title = TRUE, box = TRUE,
    method = c("pearson", "kendall", "spearman"), \dots)
    
% moved to Rmetrics Addon:
%assetsTreePlot(x, labels = TRUE, title = TRUE, box = TRUE,
%    method = "euclidian", seed = NULL, \dots)
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
        to compute the correlation matrix.
        % [assetsTreePlot] - \cr
        % a character string, the method used to compute the distance
        % matrix, see function \code{dist}.
        }   
    \item{title}{
        a logical flag, should a default title be added? By default 
        \code{TRUE}.
        }
    %item{seed}{
        % an integer value setting the seed in the computation of the 
        % sample ranks.
        % }
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\note{

    The function \code{assetsTreePlot} displaying a minimum spanning tree
    plot has bee moved to the Rmetrics addon package.
    
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
}


\keyword{models}

