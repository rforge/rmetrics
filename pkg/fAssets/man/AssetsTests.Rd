\name{AssetsTests}

\alias{AssetsTests}

\alias{assetsTest}


\title{Testing Multivariate Asset Sets}


\description{
  
    A collection and description of functions which 
    allow to test if a set of assets is multivariate
    normally distributed.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsTest} \tab Test for multivariate Normal distribution. }
    
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


\note{
    
    The usage of the alternative \code{method="energy"} requires to
    load the contributed R package \code{energy}. This is done by the
    function \code{assetsTest} itself, but it is important to know, 
    that the packages can only loaded if they are installed.
    
}


\value{
  
    \code{assetsTest}
    \cr
    returns an object of class \code{fHTEST}.     
  
}


\details{
  
    \bold{Assets Tests:}
    \cr\cr   
    The function \code{assetsTest} performs two tests for multivariate
    Normality of an assets Set.

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

    Maria Rizzoand Gabor Szekely for R's \code{energy} package, \cr
    Diethelm Wuertz for the Rmetrics port.
    
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
