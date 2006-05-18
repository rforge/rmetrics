\name{UnitrootTests}

\alias{UnitrootTests}

\alias{urTest}

\alias{adfTest}
\alias{unitrootTest}


% from Bernhard Pfaff's urca package:
\alias{urdfTest}
\alias{urersTest}
\alias{urkpssTest}
\alias{urppTest}
\alias{urspTest}
\alias{urzaTest}


\title{Unit Root Time Series Tests}


\description{

    A collection and description of functions 
    for unit root testing. The family of tests 
    includes ADF tests based on Banerjee's et al.
    tables and on J.G. McKinnons' numerical 
    distribution functions. In addition we have 
    included tests written by B. Pfaff.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{adfTest} \tab Augmented Dickey--Fuller test for unit roots, \cr
    \code{unitrootTest} \tab the same based on McKinnons's test statistics. }
    
    Added functions based on the 'urca' package include:
    
    \tabular{ll}{
    \code{urdfTest} \tab Augmented Dickey--Fuller test for unit roots, \cr
    \code{urersTest} \tab Elliott--Rothenberg--Stock test for unit roots, \cr
    \code{urkpssTest} \tab KPSS unit root test for stationarity, \cr
    \code{urppTest} \tab Phillips--Perron test for unit roots, \cr  
    \code{urspTest} \tab Schmidt--Phillips test for unit roots, \cr 
    \code{urzaTest} \tab Zivot--Andrews test for unit roots. }
    
    Note, that the contributed R package \code{urca} is required!
    
}


\note{

    The \code{ur*Test} wrapper functions fullfill the naming conventions 
    of Rmetrics, return an S4 object named \code{fHTEST} as any other 
    hypothesis test from Rmetrics, and allow for \code{timeSeries} objects 
    as input. These are the only differences. The Rmetrics wrappers 
    were tested with \code{urca} version 0.7.9.
    
    If you are running Rmetrics under an operating system where the
    R-package \code{urca} or one of the other packages from the 
    dependency tree is not available you can load the required
    functions through the command xmpfSeries() and select 
    \emph{funUrca} from the menu. A minimalist copy of \code{"urca"}
    and required dependent functions are saved in the demo 
    file \code{funUrca.R}.
    
    Fur further details we refer to the manual pages of the 
    \code{"urca"} package.
    
}


\usage{
urTest(x, method = c("unitroot", "adf", "urers", "urkpss", "urpp", 
    "ursp", "urza"), title = NULL, description = NULL, \dots)

% Rmetrics:
unitrootTest(x, lags = 1, type = c("nc", "c", "ct"), title = NULL, 
    description = NULL)
adfTest(x, lags = 1, type = c("nc", "c", "ct"), title = NULL, 
    description = NULL)

% urca Package - Pfaff:
urdfTest(x, lags = 1, type = c("nc", "c", "ct"), doplot = TRUE)
urersTest(x, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
    lag.max = 4, doplot = TRUE)
urkpssTest(x, type = c("mu", "tau"), lags = c("short", "long", "nil"),
    use.lag = NULL, doplot = TRUE)
urppTest(x, type = c("Z-alpha", "Z-tau"), model = c("constant", "trend"),
    lags = c("short", "long"), use.lag = NULL, doplot = TRUE)
urspTest(x, type = c("tau", "rho"), pol.deg = c(1, 2, 3, 4),
    signif = c(0.01, 0.05, 0.1), doplot = TRUE)
urzaTest(x, model = c("intercept", "trend", "both"), lag, doplot = TRUE)


% uroot Package
% ---
}


\arguments{
  
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{doplot}{
        [ur*Test] - \cr
        a logical flag, by default \code{TRUE}. Should a diagnostical
        plot be displayed?
        }
    \item{lag.max}{
        [urersTest] - \cr
        the maximum numbers of lags used for testing of a decent lag 
        truncation for the \code{"P-test"}, BIC used, or the maximum 
        number of lagged differences to be included in the test 
        regression for \code{"DF-GLS"}.
        }
    \item{lag}{
        [urzaTest] - \cr
        the highest number of lagged endogenous differenced variables 
        to be included in the test regression.
        }
    \item{lags}{
        [urkpssTest][urppTest] - \cr
        the maximum number of lags used for error term correction.
        }
    \item{method}{
        [urTest] - \cr
        a character string describing the desired method, one of:
        \code{"unitroot"}, \code{"adf"}, \code{"urers"}, \code{"urkpss"}, 
        \code{"urpp"}, \code{"ursp"}, or \code{"urza"}.
        }
    \item{model}{
        [urersTest] - \cr
        a character string dennoting the deterministic model used for 
        detrending, either \code{"constant"}, the default, or 
        \code{"trend"}. \cr
        [urppTest] - \cr
        a character string which determines the deterministic part in 
        the test regression, either \code{"constant"}, the default, or 
        \code{"trend"}. \cr
        [urzaTest] - \cr
        a character string specifying if the potential break occured 
        in either the \code{"intercept"}, the linear \code{"trend"} or 
        in \code{"both"}.
        }
    \item{pol.deg}{
        [urspTest] - \cr
        the polynomial degree in the test regression.
        }
    \item{signif}{
        [urspTest] - \cr
        the significance level for the critical value of the test 
        statistic.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{type}{    
        [adfTest][unitrootTest] - \cr
        a character string describing the type of the unit root 
        regression. Valid choices are \code{"nc"} for a regression 
        with no intercept (constant) nor time trend, and \code{"c"} 
        for a regression with an intercept (constant) but no time 
        trend, \code{"ct"} for a regression with an intercept 
        (constant) and a time trend. The default is \code{"c"}. 
        \cr
        [urkpssTest] - \cr
        a character string which denotes the type of deterministic part,
        either \code{"mu"}, the default, or \code{"tau"}.
        \cr
        [urppTest] - \cr
        a character string which specifies the test type, either 
        \code{"Z-alpha"}, the default, or \code{"Z-tau"}.
        \cr
        [urspTest] - \cr
        a character string which specifies the test type, either 
        \code{"tau"}, the default, or \code{"rho"}.
        } 
    \item{use.lag}{
        [urkpssTest] - \cr
        a character string specifying the number of lags. Allowed
        arguments are \code{lags=c("short", "long", "nil")}, for more 
        information see the details section.\cr
        [urppTest] - \cr      
        Use of a different lag number, specified by the user.
        }
    \item{x}{
        a numeric vector or time series object.
        }
    \item{\dots}{
        [urTest] - \cr
        optional arguments passed to the underlying test functions.
        }   
        
}


\details{

    \bold{ADF Tests:}
    
    The \code{adftest} computes test statistics and p values along
    the implementation from Trapletti's augmented Dickey--Fuller test 
    for unit roots. In contrast to Trapletti's function three kind 
    of test types can be selected.
    \cr   

    \bold{Unit Root Tests from Berhard Pfaff's "urca" Package:}
    \cr

    \emph{Elliott--Rothenberg--Stock Test for Unit Roots:} 
    \cr
    To improve the power of the unit root test, Elliot, Rothenberg and 
    Stock proposed a local to unity detrending of the time series. ERS 
    developed a feasible point optimal test, \code{"P-test"}, which 
    takes serial correlation of the error term into account. The second 
    test type is the \code{"DF-GLS"} test, which is an ADF-type test 
    applied to the detrended data without intercept. Critical values 
    for this test are taken from MacKinnon in case of \code{model="constant"}
    and else from Table 1 of Elliot, Rothenberg and Stock. \cr
    \code{[urca:ur.ers]} \cr
        
    \emph{KPSS Test for Unit Roots:} 
    \cr
    Performs the KPSS unit root test, where the Null hypothesis is 
    stationarity. The test types specify as deterministic component 
    either a constant \code{"mu"} or a constant with linear trend 
    \code{"tau"}. \code{lags="short"} sets the number of lags to 
    \emph{root 4 of [4 times (n/100)}, whereas \code{lags="long"} 
    sets the number of lags to \emph{root 4 of [12 times (n/100)]}. 
    If \code{lags="nil"} is choosen, then no error correction is made. 
    Furthermore, one can specify a different number of maximum lags 
    by setting use.lag accordingly. \cr
    \code{[urca:ur.kpss]} \cr
    
    \emph{Phillips--Perron Test for Unit Roots:} 
    \cr
    Performs the Phillips and Perron unit root test. Beside the 
    Z statistics Z-alpha and Z-tau, the Z statistics for the 
    deterministic part of the test regression are computed, too. 
    For correction of the error term a Bartlett window is used. \cr
    \code{[urca:ur.pp]} \cr
        
    \emph{Schmidt--Phillips Test for Unit Roots:} 
    \cr
    Performs the Schmidt and Phillips unit root test, where under 
    the Null and Alternative Hypothesis the coefficients of the 
    deterministic variables are included. Two test types are available: 
    the \code{"rho-test"} and the \code{"tau-test"}. Both tests are 
    extracted from the LM principle. \cr
    \code{[urca:ur.sp]} \cr
        
    \emph{Zivot--Andrews Test for Unit Roots:} 
    \cr
    Performs the Zivot and Andrews unit root test, which allows a 
    break at an unknown point in either the intercept, the linear 
    trend or in both. This test is based upon the recursive estimation 
    of a test regression. The test statistic is defined as the 
    minimum t-statistic of the coeffcient of the lagged endogenous 
    variable. \cr
    \code{[urca:ur.za]} 
 
}


\value{

    All tests return an object of class \code{"fHTEST"} with the
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
    
Kwiatkowski D., Phillips P.C.B, Schmidt P., Shin Y. (1992);
    \emph{Testing the Null Hypothesis of Stationarity against 
    the Alternative of a Unit Root},
    Journal of Econometrics 54, 159--178.
    
MacKinnon, J.G. (1996);
    \emph{Numerical distribution functions for unit root and 
        cointegration tests},
    Journal of Applied Econometrics 11, 601--618.
    
Perron P. (1988);
    \emph{Trends and Random Walks in Macroeconomic Time Series},
    Journal of Economic Dynamics and Control 12, 297--332.
    
Phillips P.C.B., Perron P. (1988);
    \emph{Testing for a unit root in time series regression}, 
    Biometrika 75, 335--346.
    
Said S.E., Dickey D.A. (1984);
    \emph{Testing for Unit Roots in Autoregressive-Moving Average 
        Models of Unknown Order},
    Biometrika 71, 599--607.
    
Schwert G.W. (1989);
    \emph{Tests for Unit Roots: A Monte Carlo Investigation},
    Journal of Business and Economic Statistics 2, 147--159.
 
}


\author{

    Adrian Trapletti for the tests adapted from R's "tseries" package, \cr
    Bernhard Pfaff for the tests wrapped from R's "urca" package,\cr
    Diethelm Wuertz for the Rmetrics \R-port.
}


\examples{
## SOURCE("fSeries.032B-UnitrootTests")

\dontrun{
## adfTest - 
   xmpSeries("\nStart: Augmented Dickey-Fuller Test for Unit Roots >")
   # A time series which contains no unit-root:
   x = rnorm(1000)  
   adfTest(x)
   # A time series which contains a unit-root:
   y = cumsum(c(0, x))
   adfTest(y)
   
## unitrootTest - 
   xmpSeries("\nNext: ADF Test using McKinnon's Tables >")
   # A time series which contains no unit-root:
   x = rnorm(1000)  
   unitrootTest(x)
   # A time series which contains a unit-root:
   y = cumsum(c(0, x))
   unitrootTest(y)    
 
## Unit Root Tests build on Bernhard Pfaff's Implementation:
    
## ur*Test - 
   # Examples can be found in the demo file "xmpTestUnitRoots".  
}

}

\keyword{htest}

