\name{assetsMeanCov}


\alias{assetsMeanCov}
\alias{getCenterRob}
\alias{getCovRob}


\title{Estimation of Mean and Covariances of Asset Sets}


\description{
  
    Allows to estimate the mean and/or covariance matrix of a 
    time series of assets by traditional and robust methods.
    
}
    

\usage{
assetsMeanCov(x, 
    method = c("cov", "mve", "mcd", "MCD", "OGK", "nnve", "shrink", "bagged"), 
    check = TRUE, force = TRUE, baggedR = 100, sigmamu = scaleTau2, 
    alpha = 1/2, ...)
    
getCenterRob(object)
getCovRob(object)
}


\arguments{
  
    \item{alpha}{ 
        when \code{methode="MCD"}, a numeric parameter controlling the size 
        of the subsets over which the determinant is minimized, i.e., 
        \code{alpha*n} observations are used for computing the determinant. 
        Allowed values are between 0.5 and 1 and the default is 0.5.
        For details we refer to the help pages of the R-package
        \code{robustbase}.
        }
    \item{baggedR}{
        when \code{methode="bagged"}, an integer value, the number of 
        bootstrap replicates, by default 100.
        }
    \item{check}{
        a logical flag. Should the covariance matrix be tested to be
        positive definite? By default \code{TRUE}.
        }
    \item{force}{
        a logical flag. Should the covariance matrix be forced to be
        positive definite? By default \code{TRUE}.
        }
    \item{method}{
        a character string, whicht determines how to compute the covariance
        matix. If \code{method="cov"} is selected then the standard 
        covariance will be computed by R's base function \code{cov}, if 
        \code{method="shrink"} is selected then the covariance will be
        computed using the shrinkage approach as suggested in Schaefer and
        Strimmer [2005], if \code{method="bagged"} is selected then the 
        covariance will be calculated from the bootstrap aggregated (bagged)
        version of the covariance estimator.
        }  
    \item{object}{
        a list as returned by the function \code{assetsMeanCov}.
        } 
    \item{sigmamu}{ 
        when \code{methode="OGK"}, a function that computes univariate robust 
        location and scale estimates. By default it should return a single 
        numeric value containing the robust scale (standard deviation) 
        estimate. When \code{mu.too} is true (the default), \code{sigmamu()} 
        should return a numeric vector of length 2 containing robust location 
        and scale estimates. See \code{scaleTau2}, \code{s_Qn}, \code{s_Sn}, 
        \code{s_mad} or \code{s_IQR} for examples to be used as \code{sigmamu} 
        argument. 
        For details we refer to the help pages of the R-package
        \code{robustbase}.
        }
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        }   
    \item{\dots}{
        optional arguments to be passed to the underlying estimators. 
        For details we refer to the manual pages of the functions 
        \code{cov.rob} for arguments \code{"mve"} and \code{"mcd"} in 
        the R package \code{MASS}, to the functions
        \code{covMcd} and \code{covOGK} in the R package \code{robustbase}.
        }
        
}


\value{
  
  
    \code{assetsMeanCov}

    returns a list with for entries named \code{center} \code{cov},
    \code{mu} and \code{Sigma}. The list has a character vector attributed
    with additional control parameters.
    
    Note, the third entry has the same value as the first, and the second
    the same as the fourth entry.
  
}


\references{
    
Breiman L. (1996); 
    \emph{Bagging Predictors},
    Machine Learning 24, 123--140.

Ledoit O., Wolf. M. (2003);
    \emph{ImprovedEestimation of the Covariance Matrix of Stock Returns 
    with an Application to Portfolio Selection},
    Journal of Empirical Finance 10, 503--621. 

Schaefer J., Strimmer K. (2005);  
    \emph{A Shrinkage Approach to
    Large-Scale Covariance Estimation and Implications for Functional Genomics},
    Statist. Appl. Genet. Mol. Biol. 4, 32.

}


\author{

    Juliane Schaefer and Korbinian Strimmer for R's \code{corpcov} package, \cr
    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
   colnames(LPP)
   
## Sample Covariance Estimation:
   assetsMeanCov(LPP)
   
## MASS Robust mcd Covariance Estimation:
   mcd = assetsMeanCov(LPP, "mcd")
   mcd
   
## Extract Covariance Matrix:
   getCovRob(object = mcd)
}


\keyword{models}

