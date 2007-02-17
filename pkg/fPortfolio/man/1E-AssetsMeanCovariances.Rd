\name{AssetsMeanCovariances}

\alias{AssetsMeanCovariances}

\alias{assetsMeanCov}
\alias{assetsStats}


\title{Estimation of Mean and Covariances of Asset Sets}


\description{
  
    A collection and description of functions which 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsStats} \tab Computes basic statistics of a set of assets, \cr
    \code{assetsMeanCov} \tab Computes mean and covariance matrix. }
    
}
    

\usage{
assetsStats(x)

assetsMeanCov(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
    check = TRUE, force = TRUE, baggedR = 100, \dots)
}

\arguments{
  

    \item{baggedR}{
        an integer value, the number of bootstrap replicates, by
        default 100. This value is only used if \code{method="bagged"}.
        }
    \item{check}{
        a logical flag. Should the covariance matrix be tested to be
        positive definite? By default \code{TRUE}.
        }
    \item{force}{
        [assetsMeanCov] - \cr
        a logical flag. Should the covariance matrix be forced to be
        positive definite? By default \code{TRUE}.
        }
    \item{method}{
        [assetsMeanVar] - \cr
        a character string, whicht determines how to compute the covariance
        matix. If \code{method="cov"} is selected then the standard 
        covariance will be computed by R's base function \code{cov}, if 
        \code{method="shrink"} is selected then the covariance will be
        computed using the shrinkage approach as suggested in Schaefer and
        Strimmer [2005], if \code{method="bagged"} is selected then the 
        covariance will be calculated from the bootstrap aggregated (bagged)
        version of the covariance estimator.\cr
        }     
    \item{x}{
        ...
        }  
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\details{

    \bold{Assets Mean and Covariance:}
    \cr\cr   
    The function \code{assetsMeanCov} computes the mean vector and covariance
    matrix of an assets set. For the covariance matrix one can select from
    three choicdes: The standard covariance computation through R's base
    function \code{cov} and a shrinked and bagged version for the covariance.
    The latter two choices implement the covariance computation from the
    functions \code{cov.shrink()} and \code{cov.bagged()} which are part
    of the contributed R package \code{corpcov}.
  
}


\value{
  
  
    \code{assetsMeanCov}
    \cr
    returns a list with two entries named \code{mu} and Sigma{Sigma}.
    The first denotes the vector of assets means, and the second the 
    covariance matrix. Note, that the output of this function can be
    used as data input for the portfolio functions to compute the
    efficient frontier.
  
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


\seealso{

    \code{MultivariateDistribution}.

}


\author{

    Juliane Schaefer and Korbinian Strimmer for R's \code{corpcov} package, \cr
    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{

## berndtInvest -
   data(berndtInvest)
   # Select "CONTIL" "DATGEN" "TANDY" and "DEC" Stocks:
   select = c("CONTIL", "DATGEN", "TANDY", "DEC")
   # Convert into a timeSeries object:
   berndtAssets.tS = as.timeSeries(berndtInvest)[, select]
   head(berndtAssets.tS)
   
## Classical Covariance Estimation:
   assetsMeanCov(berndtAssets.tS, method = "cov")
   
## mcd Covariance Estimation:
   assetsMeanCov(berndtAssets.tS, method = "mcd")
   
## mve Covariance Estimation:
   assetsMeanCov(berndtAssets.tS, method = "mve")
   
## nnve Covariance Estimation:
   assetsMeanCov(berndtAssets.tS, method = "nnve")
   
## shrinkage Covariance Estimation:
   assetsMeanCov(berndtAssets.tS, method = "shrink")
   
## bagged Covariance Estimation:
   assetsMeanCov(berndtAssets.tS, method = "bagged")

}


\keyword{models}

