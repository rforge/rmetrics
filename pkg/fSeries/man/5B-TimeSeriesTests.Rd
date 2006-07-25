\name{TimeSeriesTests}

\alias{TimeSeriesTests}

\alias{tsTest}
\alias{bdsTest}
\alias{tnnTest}
\alias{wnnTest}

\title{Time Series Tests}


\description{

    A collection and description of functions 
    for testing various aspects of univariate 
    time series, including independence, and neglected 
    nonlinearities.
    \cr
    
    The family of time series tests includes the following 
    hypothesis tests:
        
    \tabular{ll}{
    \code{bdsTest} \tab Brock--Dechert--Scheinkman test for iid series, \cr
    \code{tnnTest} \tab Teraesvirta NN test for neglected nonlinearity, \cr
    \code{wnnTest} \tab White NN test for neglected nonlinearity.}
    
}


\note{

    There is nothing really new in this package. The benefit you will 
    get from this help page, that we bring together a collection of 
    time series tests from several \R-packages which may be of interest 
    for economists and financial engineers.
    
    On the other hand the user can still use the underlying function
    calls from the imported \R-packages.
    
    The output of the various hypothesis tests is an object of class
    \code{htest}. The associated \code{print} method gives an unique 
    report about the test results.
    
}


\usage{
bdsTest(x, m = 3, eps = NULL, title = NULL, description = NULL) 
tnnTest(x, lag = 1, title = NULL, description = NULL) 
wnnTest(x, lag = 1, qstar = 2, q = 10, range = 4, title = NULL, description = NULL) 
}


\arguments{
  
    \item{description}{
        optional description string, or a vector of character strings.
        }
    \item{eps}{
        [bdsTest] - \cr
        a numeric vector of epsilon values for close points. The
        BDS test is computed for each element of \code{eps}. It should 
        be set in terms of the standard deviation of \code{x}. If
        \code{eps} is \code{NULL}, then the four default values
        \code{seq(0.5*sd(x), 2*sd(x), length = 4)} are used.
        }
    \item{lag}{
        [tnnTest][wnnTest] -\cr
        an integer which specifies the model order in terms of lags.
        }
    \item{m}{
        [bdsTest] - \cr
        an integer indicating that the BDS test statistic is computed
        for embedding dimensions \code{2}, \dots, \code{m}.
        }
    \item{q}{
        [wnnTest] -\cr
        an integer representing the number of phantom hidden units
        used to compute the test statistic.
        }
    \item{qstar}{
        [wnnTest] -\cr
        the test is conducted using \code{qstar} principal components
        of the phantom hidden units. The first principal component is 
        omitted since in most cases it appears to be collinear with the 
        input vector of lagged variables. This strategy preserves power 
        while still conserving degrees of freedom.
        }
    \item{range}{
        [wnnTest] -\cr
        the input to hidden unit weights are initialized uniformly
        over \code{[-range/2, range/2]}.
        }
    \item{title}{
        an optional title string, if not specified the inputs data 
        name is deparsed.
        }
    \item{x}{
        a numeric vector or an object of class \code{"timeseries"}.
        }       

}


\details{
    
    \bold{Brock--Dechert--Sheinkman Test:}
    \cr\cr
    The \code{bdsTest} test examines the \emph{spatial dependence} of the 
    observed series. To do this, the series is embedded in \code{m}-space 
    and the dependence of \code{x} is examined by counting \emph{near} 
    points. Points for which the distance is less than \code{eps} are 
    called near. The BDS test statistic is asymptotically standard Normal.
    Note, that missing values are not allowed.
    There is a special print method for objects of class \code{"bdsTest"}
    which by default uses 4 digits to format real numbers. 
    \cr
    \code{[tseries:bds.test]}
    \cr
  
    \bold{Teraesvirta Neural Network Test:}
    \cr\cr  
    The null is the hypotheses of linearity in \code{mean}. This test 
    uses a Taylor series expansion of the activation function to 
    arrive at a suitable test statistic. If \code{type} equals \code{"F"}, 
    then the F-statistic instead of the Chi-Squared statistic
    is used in analogy to the classical linear regression. 
    Missing values are not allowed. 
    \cr 
    \code{[tseries:teraesvirta.test]}
    \cr
    
    \bold{White Neural Network Test:}
    \cr\cr
    The null is the hypotheses of linearity in ``mean''. This
    type of test is consistent against arbitrary nonlinearity
    in mean. If \code{type} equals \code{"F"}, then the F-statistic
    instead of the Chi-Squared statistic is used in analogy to the
    classical linear regression. 
    \cr
    \code{[tseries:white.test]}
    
}


\value{

    In contrast to R's output report from S3 objects of class \code{"htest"}
    a different output report is produced. The tests here return an S4 
    object of class \code{"fHTEST"}. The object contains the following slots:
    
    \item{@call}{
        the function call.  
        }
    \item{@data}{
        the data as specified by the input argument(s).
        }
    \item{@test}{
        a list whose elements contail the results from the statistical
        test. The information provided is similar to a list object of
        class{"htest"}.
        }
    \item{@title}{
        a character string with the name of the test. This can be 
        overwritten specifying a user defined input argument.
        }
    \item{@description}{
        a character string with an optional user defined description. 
        By default just the current date when the test was applied will
        be returned.
        }
        
    The slot \code{@test} returns an object of class \code{"list"} 
    containing the following (otionally empty) elements:

    \item{statistic}{
        the value(s) of the test statistic.
        }
    \item{p.value}{
        the p-value(s) of the test.
        }
    \item{parameters}{
        a numeric value or vector of parameters.
        }
    \item{estimate}{
        a numeric value or vector of sample estimates.
        }
    \item{conf.int}{
        a numeric two row vector or matrix of 95% confidence levels.
        }
    \item{method}{
        a character string indicating what type of test was performed.
        }
    \item{data.name}{
        a character string giving the name(s) of the data.
        }
        
}


\references{

Brock, W.A., Dechert W.D., Sheinkman J.A. (1987); 
    \emph{A Test of Independence Based on the Correlation 
    Dimension}, 
    SSRI no. 8702, Department of Economics, University of 
    Wisconsin, Madison.

Conover, W.J. (1980);
    \emph{Practical Nonparametric Statistics}, 
    New York, Wiley. 

Cromwell J.B., Labys W.C., Terraza M. (1994);
    \emph{Univariate Tests for Time Series Models},
    Sage, Thousand Oaks, CA, pages 32--36.
        
Lee T.H., White H., Granger C.W.J. (1993); 
    \emph{Testing for neglected nonlinearity in time series models},
    Journal of Econometrics 56, 269--290.

Teraesvirta T., Lin C.F., Granger C.W.J. (1993);
    \emph{Power of the Neural Network Linearity Test},
    Journal of Time Series Analysis 14, 209--220.
    
}


\author{

    Adrian Trapletti for the tests from \R's tseries package,\cr
    Blake LeBaron for the \code{bds} C program,\cr 
    Diethelm Wuertz for the Rmetrics \R-port.
}


\examples{
## SOURCE("fSeries.035B-TimeSeriesTests")

## bdsTest -
   xmpSeries("\nNext: Brock-Dechert-Sheinkman Test for iid Series >")
   # iid Time Series:
   par(mfrow = c(3, 1))
   x = rnorm(100)
   plot(x, type = "l", main = "iid Time Series")
   bdsTest(x, m = 3)
   # Non Identically Distributed Time Series:
   x = c(rnorm(50), runif(50))
   plot(x, type = "l", main = "Non-iid Time Series")
   bdsTest(x, m = 3)  
   # Non Independent Innovations from Quadratic Map:
   x = rep(0.2, 100)
   for (i in 2:100) x[i] = 4*(1-x[i-1])*x[i-1]
   plot(x, type = "l", main = "Quadratic Map")
   bdsTest(x, m = 3)
   
## tnnTest -
   xmpSeries("\nNext: Teraesvirta NN test for Neglected Nonlinearity >")
   # Time Series Non-linear in "mean" regression 
   par(mfrow = c(2, 1))
   n = 1000
   x = runif(1000, -1, 1)  
   tnnTest(x)
   # Generate time series which is nonlinear in "mean"
   x[1] = 0.0
   for (i in (2:n)) {
     x[i] = 0.4*x[i-1] + tanh(x[i-1]) + rnorm (1, sd = 0.5) }
   plot(x, main = "Teraesvirta Test", type = "l")
   tnnTest(x)
   
## wnnTest -
   xmpSeries("\nNext: White NN test for Neglected Nonlinearity >")
   # Time Series Non-Linear in "mean" Regression
   par(mfrow = c(2, 1))
   n = 1000
   x = runif(1000, -1, 1)
   wnnTest(x)
   # Generate time series which is nonlinear in "mean"
   x[1] = 0.0
   for (i in (2:n)) {
     x[i] = 0.4*x[i-1] + tanh(x[i-1]) + rnorm (1, sd = 0.5) }
   plot(x, main = "White Test", type = "l")
   wnnTest(x)                     
}


\keyword{htest}

