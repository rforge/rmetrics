\name{assets-testing}

\alias{assetsTest}
\alias{mvshapiroTest}
\alias{mvenergyTest}


\title{Testing Normality of Multivariate Asset Sets}


\description{
  
    Tests if the returns of a set of assets are
    normally distributed.
    
}
    

\usage{
assetsTest(x, method = c("shapiro", "energy"), Replicates = 99)

mvshapiroTest(x)
mvenergyTest(x, Replicates = 99)
}


\arguments{
  

    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
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
}


\value{
  
    returns an object of class \code{htest}.     
  
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

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
}


\author{

    Diethelm Wuertz for this Rmetrics port.
    
}


\examples{
## LPP -
   # Load Swiss Pension Fund Data:
   LPP <- LPP2005REC[, 1:6]
   head(LPP)
   
## assetsTest -
   # Multivariate Shapiro Test - 
   assetsTest(LPP, "shapiro")
  
## assetsTest -
   # Multivariate Energy Test - 
   assetsTest(LPP, "energy")
}


\keyword{models}

