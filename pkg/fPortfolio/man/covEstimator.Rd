\name{covEstimator}


\alias{covEstimator}
\alias{mveEstimator}
\alias{mcdEstimator}

\alias{lpmEstimator}

\alias{kendallEstimator}
\alias{spearmanEstimator}

\alias{covMcdEstimator}
\alias{covOGKEstimator}
\alias{shrinkEstimator}
\alias{nnveEstimator}


\title{Covariance Estimators}


\description{
  
    Functions to estimate and robustify the sample 
    mean and covariance of rectangular objects.
    
}
    

\usage{
covEstimator(x, spec = NULL, \dots)
mveEstimator(x, spec = NULL, \dots)
mcdEstimator(x, spec = NULL, \dots)

lpmEstimator(x, spec = NULL, \dots)

kendallEstimator(x, spec = NULL, \dots)
spearmanEstimator(x, spec = NULL, \dots)

covMcdEstimator(x, spec = NULL, \dots)
covOGKEstimator(x, spec = NULL, \dots)
shrinkEstimator(x, spec = NULL, \dots)
nnveEstimator(x, spec = NULL, \dots)

}

\arguments{
  
    \item{x}{
        an object of class \code{timeSeries}. 
        }   
    \item{spec}{
        unused, may be used to pass information from the portfolio
        specification object to the mean and covariance estimator
        function.
        }
    \item{\dots}{
        optional arguments to be passed to the underlying estimators. 
        }
        
}


\value{
  
    the functions return a list with two entries named \code{mu} and 
    \code{Sigma}.
    The first denotes the vector of column means, and the second the 
    covariance matrix. Note, that the output of this function can be
    used as data input for the portfolio functions to compute the
    efficient frontier.
  
}


\details{

    The functions are underlying the following algorithms:\cr
    
    \code{covEstimator} uses standard covariance estimation,\cr
    \code{mveEstimator} uses the function "cov.mve" from the MASS package,\cr
    \code{mcdEstimator} uses the function "cov.mcd" from the MASS package,\cr
    \code{lpmEstimator} returns lower partial moment estimator,\cr
    \code{kendallEstimator} returns Kendall's rank estimator,\cr
    \code{spearmanEstimator} returns Spearman's rankestimator,\cr
    \code{covMcdEstimator} requires "covMcd" from package robustbase,\cr
    \code{covOGKEstimator} requires "covOGK" from package robustbase,\cr
    \code{nnveEstimator} uses builtin from package covRobust,\cr
    \code{shrinkEstimator} uses builtin from package corpcor.

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

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\author{

    ... for R's \code{MASS} package, \cr
    ... for R's \code{robustbase} package, \cr
    ... for R's \code{covRobust} package, \cr
    Juliane Schaefer and Korbinian Strimmer for R's \code{corpcor} package, \cr
    Diethelm Wuertz for this Rmetrics port.
    
}


\examples{
## data -
   colnames(SMALLCAP.RET)
   
## covEstimator -
   covEstimator(SMALLCAP.RET)
   
## shrinkEstimator -
   # shrinkEstimator(SMALLCAP.RET)
}


\keyword{models}

