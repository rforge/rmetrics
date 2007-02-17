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
        xxx
        }  
    \item{\dots}{
        optional arguments to be passed.
        }
        
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


\value{
  
    \code{assetsSelect}
    \cr
    if \code{method="hclust"} was selected then the function returns a
    S3 object of class "hclust", otherwise if \code{method="kmeans"} was 
    selected then the function returns an obkject of class list. For
    details we refer to the help pages of \code{hclust} and \code{kmeans}.
    
}


\references{
    
N. N. (1989); 
    \emph{Title},
     Lecture Notes, Budapest Institute of Technology, TechnicalUniversity.

}


\seealso{

    \code{MultivariateDistribution}.

}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{

## berndtInvest -
   data(berndtInvest)
   # Market and Interest Rate columns from data frame,
   berndtAssets.tS = as.timeSeries(berndtInvest)[, -c(10, 17)]
   head(berndtAssets.tS)
    
## assetsSelect -
   # Hierarchical Clustering:
   clustered = assetsSelect(berndtAssets.tS, doplot = FALSE)
   clusteredAssets.tS = berndtAssets.tS[, c(clustered$order[1:4])]
   colnames(clusteredAssets.tS)
   # Cluster Dendogram:
   par(mfrow = c(1, 1))
   plot(clustered)  
 
}


\keyword{models}

