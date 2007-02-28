\name{UnitrootTests}

\alias{UnitrootTests}


\alias{unitrootTest}
\alias{adfTest}


\title{Unit Root Time Series Tests}


\description{

    A collection and description of functions 
    for unit root testing. The family of tests 
    includes ADF tests based on Banerjee's et al.
    tables and on J.G. McKinnons' numerical 
    distribution functions.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{adfTest} \tab Augmented Dickey--Fuller test for unit roots, \cr
    \code{unitrootTest} \tab the same based on McKinnons's test statistics. }
    
}


\usage{
unitrootTest(x, lags = 1, type = c("nc", "c", "ct"), title = NULL, 
    description = NULL)
adfTest(x, lags = 1, type = c("nc", "c", "ct"), title = NULL, 
    description = NULL)
}


\arguments{
  
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{lags}{
        the maximum number of lags used for error term correction.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{type}{    
        a character string describing the type of the unit root 
        regression. Valid choices are \code{"nc"} for a regression 
        with no intercept (constant) nor time trend, and \code{"c"} 
        for a regression with an intercept (constant) but no time 
        trend, \code{"ct"} for a regression with an intercept 
        (constant) and a time trend. The default is \code{"c"}. 
        } 
    \item{x}{
        a numeric vector or time series object.
        }  
        
}


\details{
    
    The function \code{adfTest()} computes test statistics and p values 
    along the implementation from Trapletti's augmented Dickey--Fuller 
    test for unit roots. In contrast to Trapletti's function three kind 
    of test types can be selected.
    
    The function \code{unitrootTest()} computes test statistics and p values 
    using McKinnon's response surface approach.
 
}


\value{

    The tests return an object of class \code{"fHTEST"} with the
    following slots:


    \item{@call}{
        the function call.      
        }
    \item{@data}{
        a data frame with the input data.
        }
    \item{@data.name}{
        a character string giving the name of the data frame.
        }
    \item{@test}{
        a list object which holds the output of the underlying
        test function.
        }
    \item{@title}{
        a character string with the name of the test.
        }
    \item{@description}{
        a character string with a brief description of the
        test.
        }
        
    The entries of the \code{@test} slot include the following components:

    \item{$statistic}{
        the value of the test statistic.
        }
    \item{$parameter}{
        the lag order.
        }
    \item{$p.value}{
        the p-value of the test.
        }
    \item{$method}{
        a character string indicating what type of test was
        performed.
        } 
    \item{$data.name}{
        a character string giving the name of the data.
        }
    \item{$alternative}{
        a character string describing the alternative
        hypothesis.
        }
    \item{$name}{
        the name of the underlying function, which may be wrapped.
        }
    \item{$output}{
        additional test results to be printed.
        }
        
}


\references{

Banerjee A., Dolado J.J., Galbraith J.W., Hendry D.F. (1993);
    \emph{Cointegration, Error Correction, and the Econometric 
        Analysis of Non-Stationary Data},
    Oxford University Press, Oxford. 
    
Dickey, D.A., Fuller, W.A. (1979);
    \emph{Distribution of the estimators for autoregressive time 
        series with a unit root}, 
    Journal of the American Statistical Association 74, 427--431. 
    
MacKinnon, J.G. (1996);
    \emph{Numerical distribution functions for unit root and 
        cointegration tests},
    Journal of Applied Econometrics 11, 601--618.
    
Said S.E., Dickey D.A. (1984);
    \emph{Testing for Unit Roots in Autoregressive-Moving Average 
        Models of Unknown Order},
    Biometrika 71, 599--607.
 
}


\author{

    Adrian Trapletti for the tests adapted from R's "tseries" package, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
}


\examples{
## adfTest - 
   # A time series which contains no unit-root:
   x = rnorm(1000)  
   # A time series which contains a unit-root:
   y = cumsum(c(0, x))
   # Test:
   adfTest(x)
   adfTest(y)
   
## unitrootTest - 
   unitrootTest(x)
   unitrootTest(y)     
}


\keyword{htest}

