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
assetsTest(x, FUN = "mvshapiroTest", title = NULL, description = NULL, \dots)

mvshapiroTest(x, title = NULL, description = NULL)
}


\arguments{
  

    \item{description}{
        a character string, assigning a brief description to the
        returned object.
        }
    \item{FUN}{
        a character string, which allows to select the test.
        If \code{FUN="mvshapiroTest"} then Shapiro's multivariate Normality 
        test will be applied as implemented in R's contributed package
        \code{mvnormtest}. 
        % If \code{method="energy"} then the E-statistic (energy) for 
        % testing multivariate Normality will be used as proposed 
        % and implemented by Szekely and Rizzo [2005] using parametric 
        % bootstrap.
        } 
    %item{Replicates}{
        % an integer value, the number of bootstrap replicates, by
        % default 100. This value is only used if \code{method="energy"}.
        % }
    \item{title}{
        a character string, assigning a title to an 
        \code{"fASSETS"} object.
        }
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        }   
    \item{\dots}{
        optional arguments passed to the function \code{FUN}.
        }
        
}


\note{
    
    The contributed package \code{energy} contains an alternative function
    for testing multivariate normality, see the references given below.
     
    An implementation of the \code{mvenergyTest} which was previously 
    available through this package as an alternative to the Shapiro test 
    has been moved to the Rmetrics Addon package. 
}


\value{
  
    \code{assetsTest}
    \cr
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
## berndtInvest -
   data(berndtInvest)
   # Exclude Date, Market and Interest Rate columns from data frame,
   berndtAssets = berndtInvest[, -c(1, 11, 18)]
   rownames(berndtAssets) = berndtInvest[, 1]
   head(berndtAssets)  
}


\keyword{models}
