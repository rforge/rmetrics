\name{timeSeries-method-stats}

\docType{methods}

\alias{sd-methods}
\alias{var-methods}
\alias{cov-methods}
\alias{cor-methods}

\alias{dcauchy-methods}
\alias{dnorm-methods}
\alias{dt-methods}

\alias{sd,timeSeries-method}
\alias{var,timeSeries-method}
\alias{cov,timeSeries-method}
\alias{cor,timeSeries-method}
\alias{dcauchy,timeSeries-method}
\alias{dnorm,timeSeries-method}
\alias{dt,timeSeries-method}

\title{Time Series Correlations}


\description{
    S4 methods of stats package for \code{timeSeries} objects.
    \cr
    
    \tabular{ll}{
    \code{cov} \tab Computes Covariance from a 'timeSeries' object, \cr
    \code{cor} \tab Computes Correlations from a 'timeSeries' object. \cr
    \code{dcauchy} \tab  ... \cr
    \code{dnorm} \tab ... \cr
    \code{dt} \tab ... \cr
  }
  
  
}


\usage{
\S4method{cov}{timeSeries}(x, y = NULL, use = "all.obs", 
    method = c("pearson", "kendall", "spearman"))
    
\S4method{cor}{timeSeries}(x, y = NULL, use = "all.obs", 
    method = c("pearson", "kendall", "spearman"))
}


\arguments{
  
  \item{method}{ 
    a character string indicating which correlation coefficient (or 
    covariance) is to be computed. One of \code{"pearson"} (default), 
    \code{"kendall"}, or \code{"spearman"}, can be abbreviated.
  }
  \item{use}{
    an optional character string giving a method for computing
    covariances in the presence of missing values. This must 
    be (an abbreviation of) one of the strings \code{"all.obs"},
    \code{"complete.obs"} or \code{"pairwise.complete.obs"}.
  }
  \item{x}{
    an univariate object of class \code{timeSeries}.
  }
  \item{y}{
    NULL (default) or a \code{timeSeries} object with compatible 
    dimensions to \code{x}. The default is equivalent to y = x (but 
    more efficient).
  }  
  
}


\value{

    returns the covariance or correlation matrix.
        
}


\examples{
## data -
   require(fEcofin)
   x = as.timeSeries(data(msft.dat))[, 1:4]
   x = 100*returns(x)

## cov -
   cov(x[, "Open"], x[, "Close"])
   cov(x)
}

\keyword{methods}
\keyword{chron}

