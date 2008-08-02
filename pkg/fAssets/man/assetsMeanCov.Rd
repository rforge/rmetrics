\name{assetsMeanCov}


\alias{assetsMeanCov}

\alias{covEstimate}
\alias{mcdEstimate}
\alias{mveEstimate}

\alias{kendallEstimate}
\alias{spearmanEstimate}


\title{Estimation of Mean and Covariances of Asset Sets}


\description{
  
    A collection and description of functions which allow
    to estimate the mean and/or covariance matrix of a 
    time series of assets by traditional and robust methods.
    
}
    

\usage{
assetsMeanCov(x, FUN = "covEstimate", check = TRUE, force = TRUE, ...)

covEstimate(x)
mcdEstimate(x)
mveEstimate(x)

kendallEstimate(x)
spearmanEstimate(x)
}

\arguments{
  

    %item{alpha}{ 
        % when \code{methode="MCD"}, a numeric parameter controlling the size 
        % of the subsets over which the determinant is minimized, i.e., 
        % \code{alpha*n} observations are used for computing the determinant. 
        % Allowed values are between 0.5 and 1 and the default is 0.5.
        % For details we refer to the help pages of the R-package
        % \code{robustbase}.
        % }
    %item{baggedR}{
        % when \code{methode="bagged"}, an integer value, the number of 
        % bootstrap replicates, by default 100.
        % }
    \item{check}{
        a logical flag. Should the covariance matrix be tested to be
        positive definite? By default \code{TRUE}.
        }
    \item{force}{
        a logical flag. Should the covariance matrix be forced to be
        positive definite? By default \code{TRUE}.
        }
    \item{FUN}{
        a character string, which determines how to compute the mean and 
        the covariance matix. 
        If \code{method="cov"} is selected then the standard 
        covariance will be computed by R's base function \code{cov}, if 
        \code{method="shrink"} is selected then the covariance will be
        computed using the shrinkage approach as suggested in Schaefer and
        Strimmer [2005], if \code{method="bagged"} is selected then the 
        covariance will be calculated from the bootstrap aggregated (bagged)
        version of the covariance estimator.\cr
        }  
    %item{sigmamu}{ 
        % when \code{methode="OGK"}, a function that computes univariate robust 
        % location and scale estimates. By default it should return a single 
        % numeric value containing the robust scale (standard deviation) 
        % estimate. When \code{mu.too} is true (the default), \code{sigmamu()} 
        % should return a numeric vector of length 2 containing robust location 
        % and scale estimates. See \code{scaleTau2}, \code{s_Qn}, \code{s_Sn}, 
        % \code{s_mad} or \code{s_IQR} for examples to be used as \code{sigmamu} 
        % argument. 
        % For details we refer to the help pages of the R-package
        % \code{robustbase}.
        % }
    \item{x}{
        a \code{timeSeries} object or any other rectangular object which 
        can be converted by the function \code{as.matrix()} into a matrix 
        object. 
        }   
    \item{\dots}{
        optional arguments to be passed to the underlying estimators
        \code{FUN}.
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


\details{

    \bold{Assets Mean and Covariance:}
  
    The function \code{assetsMeanCov} computes the mean vector and 
    covariance matrix of an assets set. The function \code{FUN} determines 
    which estimator should be used.
    
    Three estimators are availalble in this package: The sample estimator
    \code{covEstimate} and the two robust estimators \code{mcdEstimate}
    and \code{mveEstimate} based on the robust estimators from the 
    recommende MASS package.
    
    Further Estimators are available in the Rmetrics addon package.
    "MCD" and "OGK" based on the robust estimators from the contributed 
    package \code{robustbase}, or "shrink" based on the shrinkage estimator
    from the contributed package \code{corpcor} among others.
    
    
    \bold{How to add your own estimator?}
    
    You can add your alternative function to compute mean and covariance.
    This is is a function of the kind \code{myEstimator(x, ...)} which
    returns a list with at least three named entries, the mean \code{mu}, 
    the covariance \code{Sigma} and a charactar control vector with at 
    at least one element, the name of the function \code{control="myFunction"}.
    
    Here comes an example how to implement the MCD Estimator from the 
    contributed R package \code{robustbase}:
    
    \preformatted{
      require(robustbase)
      myEstimate <- function(x, alpha = 0.5)
        \{
        estimate = robustbase::covMcd(as.matrix(x), alpha = alpha)
        list(
            mu = estimate$center,
            Sigma = Sigma = estimate$cov, 
            control = c("myEstimate", as.character(alpha))
        \}
      assetsMeanCov(x = assetsSim(100), FUN = "myEstimate")
    }
  
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
   assetsMeanCov(berndtAssets.tS)
   
## MASS mcd Covariance Estimation:
   assetsMeanCov(berndtAssets.tS, FUN = "mcdEstimate")
}


\keyword{models}

