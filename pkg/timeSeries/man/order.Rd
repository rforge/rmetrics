\name{orderColnames}


\title{Reorder Column Names of a Time Series}


\alias{orderColnames}
\alias{sortColnames}
\alias{sampleColnames}
\alias{statsColnames}
\alias{pcaColnames}
\alias{hclustColnames}


\description{
    
    Functions and methods dealing with the rearrangement
    of column names of 'timeSeries' objects. 
    \cr
    
    \tabular{ll}{
    \code{orderColnames} \tab Returns ordered column names of a time Series, \cr
    \code{sortColnames} \tab Returns sorted column names of a time Series, \cr
    \code{sampleColnames} \tab Returns sampled column names of a time Series, \cr
    \code{statsColnames} \tab Returns statistically rearranged column names, \cr
    \code{pcaColnames} \tab Returns PCA correlation ordered column names, \cr
    \code{hclustColnames} \tab Returns hierarchical clustered column names. }

}


\usage{
orderColnames(x, \dots)
sortColnames(x, \dots)  
sampleColnames(x, \dots) 
statsColnames(x, FUN = colMeans, \dots)
pcaColnames(x, robust = FALSE, \dots)
hclustColnames(x, method = c("euclidean", "complete"), \dots)
}


\arguments{
        
    \item{FUN}{ 
        a character string indicating which statistical function 
        should be applied. By default statistical ordering operates 
        on the column means of the time series.
        }
    \item{method}{
        a character string with two elements. The first determines
        the choice of the distance measure, see \code{dist}, and the
        second determines the choice of the agglomeration method, see
        \code{hclust}.
        }
    \item{robust}{
        a logical flag which indicates if robust correlations
        should be used.
        }
    \item{x}{
        an object of class \code{timesSeries} or any other rectangular 
        object which can be transformed by the function \code{as.matrix}
        into a numeric matrix. 
        }
    \item{\dots}{
        further arguments to be passed, see details.
        }
       
}


\details{

    \bold{Statistically Motivated Rearrangement} 
    
    The function \code{statsColnames} rearranges the column names
    according to a statical measure. These measure must operate on the
    columns of the time series and return a vector of values which
    can be sorted. Typical functions ar those listed in in help
    page \code{colStats} but one can also crete his own
    functions which compute for example risk or any other statistical
    measure. The \code{\dots} argument allows to pass additional
    arguments to the underlying function \code{FUN}.\cr
    
    \bold{PCA Ordering of the Correlation Matrix} 
    
    The function \code{pcaColnames} rearranges the column names
    according to the PCA ordered correlation matrix. The argument
    \code{robust} allsows to select between the use of the standard
    \code{cor} and computation of robust correlations using 
    the function \code{covMcd} from contributed R package
    \code{robustbase}. The \code{\dots} argument allows to pass 
    additional arguments to the two underlying functions \code{cor} 
    or \code{covMcd}. E.g. adding \code{method="kendall"} 
    to the argument list calculates Kendall's rank correlations
    instead the default which calculates Person's correlations.\cr
    
    \bold{Ordering by Hierarchical Clustering} 
    
    The function \code{pcaColnames} uses the hierarchical clustering
    approach \code{hclust} to rearrange the column names of the
    time series. 
    
}


\value{

    returns a vector of character string, the rearranged column names.
        
}


\examples{
## data -
   require(fEcofin)
   edhec.tS = as.timeSeries(data(edhec.tS))
   colnames(edhec.tS) = abbreviate(colnames(edhec.tS), 6)

## sortColnames -
   # Sort alphabetically
   sortColnames(edhec.tS)
  
## hclustColnames -
   head(edhec.tS[, hclustColnames(edhec.tS)])  
}


\keyword{chron}

