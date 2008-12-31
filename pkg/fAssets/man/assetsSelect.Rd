\name{AssetsSelection}

\alias{AssetsSelection}


\alias{assetsSelect}


\title{Selecting Assets from Multivariate Asset Sets}


\description{
  
    Allows to selet assets  from clustering.
    
}
    

\usage{
assetsSelect(x, use = c("hclust", "kmeans"), 
    kmeans.centers = 5, kmeans.maxiter = 10, doplot = TRUE, \dots)
}


\arguments{
  
    \item{doplot}{
        a logical, should a plot be displayed?
        }
    \item{kmeans.centers}{
        either the number of clusters or a set of initial cluster centers. 
        If the first, a random set of rows in \code{x} are chosen as the 
        initial centers.   
        }
    \item{kmeans.maxiter}{
        the maximum number of iterations allowed.
        }
    \item{use}{
        a character string, which clustering algorithm should be used? 
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
  
    if \code{use="hclust"} was selected then the function returns a
    S3 object of class "hclust", otherwise if \code{use="kmeans"} was 
    selected then the function returns an object of class "list". For
    details we refer to the help pages of \code{hclust} and \code{kmeans}.
    
}


\details{
      
    The function \code{assetsSelect} calls the functions \code{hclust}
    or \code{kmeans} from R's \code{"stats"} package. \code{hclust}
    performs a hierarchical cluster analysis on the set of dissimilarities 
    \code{hclust(dist(t(x)))} and \code{kmeans} performs a k-means
    clustering on the data matrix itself. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))
   colnames(LPP)
    
## assetsSelect -
   assetsSelect(LPP, "hclust") 
   assetsSelect(LPP, "kmeans") 
}


\keyword{models}

