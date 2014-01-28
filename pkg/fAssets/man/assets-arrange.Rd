\name{assets-arrange}


\alias{assetsArrange}
\alias{pcaArrange}
\alias{hclustArrange}
\alias{abcArrange}
\alias{orderArrange}
\alias{sampleArrange}
\alias{statsArrange}

\title{Rearranging Assets Columnwise}


\description{
  
    Allows to rearrange a set of assets columnwise.
    
}
    

\usage{
assetsArrange(x, method = c("pca", "hclust", "abc"), ...)

pcaArrange(x, robust = FALSE, ...)
hclustArrange(x, method = c("euclidean", "complete"), ...)
abcArrange(x, ...)
orderArrange(x, ...)
sampleArrange(x, ...)
statsArrange(x, FUN = colMeans, ...)
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
        \code{"abc"} which arrangtes the columns alphabetically,
        \code{"order"} which arrangtes the columns by the order function,
        \code{"sample"} which arranges the columns randomly, or
        \code{"stats"} which arranges by an statistical strategy.
        }   
    \item{robust}{
        a logical flag. Should robust statistics applied?
    }
    \item{FUN}{
        function anme of the statistical function to be applied.
    }
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\value{
  
    a character vector with the rearranged assets names.
    
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
## LPP -
   # Load Swiss Pension Fund Data:
   LPP <- LPP2005REC[, 1:3]
   head(LPP)
    
## assetsArrange -
   # Arrange Assets Columns:
   assetsArrange(x=LPP, "pca")
   assetsArrange(x=LPP, "hclust")
   assetsArrange(x=LPP, "abc")
   
## Alternative Usage -
   pcaArrange(x=LPP, robust=FALSE)
   pcaArrange(x=LPP, robust=TRUE)
   hclustArrange(x=LPP, method = c("euclidean", "complete"))
   abcArrange(x=LPP)
   orderArrange(x=LPP)
   sampleArrange(x=LPP)
   statsArrange(x=LPP, FUN=colMeans)
}


\keyword{models}

