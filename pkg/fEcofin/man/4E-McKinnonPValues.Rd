\name{McKinnonPValues}

\alias{McKinnonPValues}

\alias{punitroot}
\alias{qunitroot}
\alias{unitrootTable}


\title{McKinnon's Unit Root p Values}


\description{
    
    A collection and description of functions 
    to compute the distribution and and quantile 
    function for McKinnon's unit root test statistics. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{punitroot} \tab the returns cumulative probability, \cr
    \code{qunitroot} \tab the returns quantiles of the unit root test statistics, \cr
    \code{unitrootTable} \tab tables p values from McKinnon's response surface.}
    
}
 

\usage{
punitroot(q, N = Inf, trend = c("c", "nc", "ct", "ctt"), 
    statistic = c("t", "n"), na.rm = FALSE)
qunitroot(p, N = Inf, trend = c("c", "nc", "ct", "ctt"), 
    statistic = c("t", "n"), na.rm = FALSE)
unitrootTable(trend = c("c", "nc", "ct", "ctt"), statistic = c("t", "n"))
   
}


\arguments{ 

    \item{N}{ 
        the number of observations in the sample from which the 
        quantiles are to be computed.\cr
        }
    \item{na.rm}{
        a logical value. If set to \code{TRUE}, missing values will 
        be removed otherwise not, the default is \code{FALSE}. 
        }
    \item{p}{ 
        a numeric vector of probabilities. Missing values are 
        allowed. 
        }
    \item{q}{   
        vector of quantiles or test statistics. Missing values 
        are allowed. 
        }       
    \item{statistic}{ 
        a character string describing the type of test statistic. 
        Valid choices are \code{"t"} for t-statistic, and \code{"n"} 
        for normalized statistic, sometimes referred to as the 
        rho-statistic. The default is \code{"t"}. 
        }
    \item{trend}{
        a character string describing the regression from which the 
        quantiles are to be computed. Valid choices are: \code{"nc"} 
        for a regression with no intercept (constant) nor time trend, 
        and \code{"c"} for a regression with an intercept (constant) 
        but no time trend, \code{"ct"} for a regression with an intercept 
        (constant) and a time trend. The default is \code{"c"}. 
        }
}


\value{ 
      
    The function \code{punitroot} returns the cumulative probability 
    of the asymptotic or finite sample distribution of the unit root 
    test statistics. 
    
    The function \code{qunitroot} returns the quantiles of the 
    asymptotic or finite sample distribution of the unit root test 
    statistics, given the probabilities. 

}


\note{
       
    The function \code{punitroot} and \code{qunitroot} use Fortran 
    routines and the response surface approach from J.G. McKinnon (1988). 
    Many thanks to J.G. McKinnon putting his code and tables under the 
    GPL license, which made this implementation possible.
    
}


\author{
    
    J.G. McKinnon for the underlying Fortran routine and the tables, \cr
    Diethelm Wuertz for the Rmetrics \R-port.

}
    
    
\references{ 
   
Dickey, D.A., Fuller, W.A. (1979);
    \emph{Distribution of the estimators for autoregressive time 
        series with a unit root}, 
    Journal of the American Statistical Association 74, 427--431. 

MacKinnon, J.G. (1996);
    \emph{Numerical distribution functions for unit root and 
        cointegration tests},
    Journal of Applied Econometrics 11, 601--618.

Phillips, P.C.B., Perron, P. (1988);
    \emph{Testing for a unit root in time series regression}, 
    Biometrika 75, 335--346.

}


\examples{ 
## qunitroot -
   # Asymptotic quantile of t-statistic
   qunitroot(0.95, trend = "nc", statistic = "t")

## qunitroot -
   # Finite sample quantile of n-statistic
   qunitroot(0.95, N = 100, trend = "nc", statistic = "n") 
   
## punitroot -
   # Asymptotic cumulative probability of t-statistic
   punitroot(1.2836, trend = "nc", statistic = "t")

## punitroot -
   # Finite sample cumulative probability of n-statistic
   punitroot(1.2836, N = 100, trend = "nc", statistic = "n")
   
## Mc Kinnon's unitrootTable -
   unitrootTable(trend = "nc")
}


\keyword{distribution}

