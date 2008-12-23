\name{AssetsTests}

\alias{AssetsTests}

\alias{assetsTest}
\alias{mvshapiroTest}


\title{Testing Multivariate Asset Sets}


\description{
  
    A collection and description of functions which 
    allow to test if a set of assets is multivariate
    normally distributed.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsTest} \tab Test Suite for multivariate Normal distribution, \cr
    \code{mvshapiroTest} \tab multivaraite Shapiro Test. }
    
}
    

\usage{
assetsTest(x, method = c("shapiro", "energy"), Replicates = 100, 
    title = NULL, description = NULL)
}


\arguments{
  

    \item{description}{
        a character string, assigning a brief description to the
        returned object.
        }
    \item{method}{
        a character string, which allows to select the test.
        If \code{method="shapiro"} then Shapiro's multivariate Normality 
        test will be applied as implemented in R's contributed package
        \code{mvnormtest}. If \code{method="energy"} then the E-statistic 
        (energy) for testing multivariate Normality will be used as proposed 
        and implemented by Szekely and Rizzo [2005] using parametric 
        bootstrap.
        } 
    \item{Replicates}{
        an integer value, the number of bootstrap replicates, by
        default 100. This value is only used if \code{method="energy"}.
        }
    \item{title}{
        a character string, assigning a title to an 
        \code{"fASSETS"} object.
        }
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        }   
        
}


\value{
  
    returns an object of class \code{fHTEST}.     
  
}


\references{

Rizzo M.L. (2002);
    \emph{A New Rotation Invariant Goodness-of-Fit Test}, 
    PhD dissertation, Bowling Green State University.

Szekely G.J., Rizzo, M.L. (2005); 
    \emph{A New Test for Multivariate Normality},
    Journal of Multivariate Analysis 93, 58--80.
    
Szekely G.J. (1989); 
    \emph{Potential and Kinetic Energy in Statistics},
     Lecture Notes, Budapest Institute of Technology, TechnicalUniversity.

}


\seealso{

    \code{MultivariateDistribution}.

}


\author{

    Diethelm Wuertz for this Rmetrics port.
    
}


\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
   colnames(LPP)
   
   # Multivariate Shapiro Test:
   assetsTest(LPP, "shapiro")
   
   # Multivariate Energy Test:
   assetsTest(LPP, "energy")
}


\keyword{models}
