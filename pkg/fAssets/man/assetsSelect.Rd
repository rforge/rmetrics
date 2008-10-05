\name{AssetsSelection}

\alias{AssetsSelection}


\alias{assetsSelect}


\title{Selecting Assets from Multivariate Asset Sets}


\description{
  
    A collection and description of functions which 
    allow for the selection and clustering of individual 
    assets from portfolios using several kinds of  
    clustering approaches.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsSelect} \tab Asset Selection from Portfolios. }
    
}
    

\usage{
assetsSelect(x, method = c("hclust", "kmeans"), 
    kmeans.centers = 5, kmeans.maxiter = 10, doplot = TRUE, \dots)
}


\arguments{
  
    \item{doplot}{
        [assetsSelect] - \cr
        a logical, should a plot be displayed?
        }
    \item{kmeans.centers}{
        [assetsSelect] - \cr
        either the number of clusters or a set of initial cluster centers. 
        If the first, a random set of rows in \code{x} are chosen as the 
        initial centers.   
        }
    \item{kmeans.maxiter}{
        [assetsSelect] - \cr
        the maximum number of iterations allowed.
        }
    \item{method}{
        [assetsSelect] - \cr
        a character string, which clustering method should be applied? 
        Either \code{hclust} for hierarchical clustering of dissimilarities,
        or \code{kmeans} for k-means clustering.
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


\value{
  
    \code{assetsSelect}
    \cr
    if \code{method="hclust"} was selected then the function returns a
    S3 object of class "hclust", otherwise if \code{method="kmeans"} was 
    selected then the function returns an obkject of class list. For
    details we refer to the help pages of \code{hclust} and \code{kmeans}.
    
}


\details{
    
    \bold{Assets Selection:}
    \cr\cr   
    The function \code{assetsSelect} calls the functions \code{hclust}
    or \code{kmeans} from R's \code{"stats"} package. \code{hclust}
    performs a hierarchical cluster analysis on the set of dissimilarities 
    \code{hclust(dist(t(x)))} and \code{kmeans} performs a k-means
    clustering on the data matrix itself. 
}



\seealso{

    \code{MultivariateDistribution}.

}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPPt -
   LPP = as.timeSeries(data(LPP2005REC))
   colnames(LPP)
    
## assetsSelect -
   # Hierarchical Clustering:
   assetsSelect(LPP)  
}


\keyword{models}

