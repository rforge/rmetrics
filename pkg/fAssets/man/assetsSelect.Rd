\name{AssetsSelection}

\alias{AssetsSelection}


\alias{assetsSelect}


\title{Selecting Assets from Multivariate Asset Sets}


\description{
  
    Allows to selet assets  from clustering.
    
}
    

\usage{
assetsSelect(x, method = c("hclust", "kmeans"), control = NULL, \dots)
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{method}{
        a character string, which clustering method should be used? 
        Either \code{hclust} for hierarchical clustering of dissimilarities,
        or \code{kmeans} for k-means clustering.
        }
    \item{control}{
        a character string with two entries controlling the parameters used
        in the underlying cluster algorithms. If set to NULL, then  
        default settings are taken: For hierarchical clustering this is 
        \code{method=c(measure="euclidean", method="complete")}, 
        and for kmeans clustering this is
        \code{method=c(centers=3, algorithm="Hartigan-Wong")}.
        }
    \item{\dots}{
        optional arguments to be passed. Note, for the k-means algorithm
        the number of centers has to be specified!
        }
        
}


\value{
  
    if \code{use="hclust"} was selected then the function returns a
    S3 object of class "hclust", otherwise if \code{use="kmeans"} was 
    selected then the function returns an object of class "kmeans". 
    
    For details we refer to the help pages of \code{hclust} and 
    \code{kmeans}.
    
}


\details{
      
    The function \code{assetsSelect} calls the functions \code{hclust}
    or \code{kmeans} from R's \code{"stats"} package. \code{hclust}
    performs a hierarchical cluster analysis on the set of dissimilarities 
    \code{hclust(dist(t(x)))} and \code{kmeans} performs a k-means
    clustering on the data matrix itself. 
    
    Note, the hierarchical cluster method has in addition a plot method.
    
}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))
   colnames(LPP)
    
## assetsSelect -
   hclust = assetsSelect(LPP, "hclust") 
   plot(hclust)
   
   assetsSelect(LPP, "kmeans", control = 
     c(centers = 3, algorithm = "Hartigan-Wong"))
}


\keyword{models}

