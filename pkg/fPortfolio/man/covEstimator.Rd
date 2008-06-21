\name{covEstimator}


\alias{covEstimator}
%\alias{covMcdEstimator}
%\alias{covOGKEstimator}
\alias{mveEstimator}
\alias{mcdEstimator}
%\alias{shrinkEstimator}
%\alias{baggedEstimator}
%\alias{nnveEstimator}


\alias{lpmEstimator}


\title{Covariance Estimators}


\description{
  
    Functions to estimate the sample mean and covariance of
    rectangular objects.
    
}
    

\usage{
covEstimator(x, spec = NULL, ...)
%covMcdEstimator(x, spec = NULL, ...)
%covOGKEstimator(x, spec = NULL, ...)
mveEstimator(x, spec = NULL, ...)
mcdEstimator(x, spec = NULL, ...)
%shrinkEstimator(x, spec = NULL, ...)
%baggedEstimator(x, spec = NULL, ...)
%nnveEstimator(x, spec = NULL, ...)

lpmEstimator(x, spec = NULL, ...)
}

\arguments{
  
    \item{spec}{
        unused.
        }
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
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
    %\code{covMcdEstimator} requires "covMcd" from package robustbase,\cr
    %\code{covOGKEstimator} requires "covOGK" from package robustbase,\cr
    %\code{nnveEstimator} uses builtin from package covRobust,\cr
    %\code{shrinkEstimator} uses builtin from package corpcor,\cr
    %\code{baggedEstimator} uses builtin from package corpcor, \cr
    \code{lpmEstimator} returns lower partial moment estimator.

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
   X = as.timeSeries(data(berndtInvest))
   select = c("CONTIL", "DATGEN", "TANDY", "DEC")
   
   # Convert into a timeSeries object:
   berndtAssets.tS = as.timeSeries(berndtInvest)[, select]
   head(berndtAssets.tS)
   
## Classical Covariance Estimation:
   mcdEstimator(berndtAssets.tS)
}


\keyword{models}

