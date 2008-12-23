\name{AssetsArrange}


\alias{assetsArrange}


\title{Rearranging Assets Columnwise}


\description{
  
    Allows to rearrange a set of assets columnwise.
    
}
    

\usage{
assetsArrange(x, method = c("pca", "hclust", "abc"), ...)
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        }   
    \item{method}{
        a character string, which method should be applied to reaarnage
        the assests? 
        Either 
        \code{"pca"} which arranges the columns by an eigenvalue decomposition,
        \code{"hclust"} which arrangtes the columns by hierarchical clustering,
        or \code{"abc"} which arrangtes the columns alphabetically.
        }    
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\value{
  
    a character vector with the ordered assets names.
    
}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))
   colnames(LPP)
    
## assetsArrange -
   assetsArrange(LPP, "pca")
   assetsArrange(LPP, "hclust")
   assetsArrange(LPP, "abc")
}


\keyword{models}

