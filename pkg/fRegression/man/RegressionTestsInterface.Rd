\name{RegressionTestsInterface}

\alias{RegressionTestsInterface}

\alias{lmTest}

\alias{bgTest}
\alias{bpTest}
\alias{dwTest}
\alias{gqTest}
\alias{harvTest}
\alias{hmcTest}
\alias{rainTest}
\alias{resetTest}

\title{Regression Tests}


\description{

    A collection and description of functions 
    to test linear regression  models, including
    tests for higher serial correlations, for 
    heteroskedasticity, for autocorrelations 
    of disturbances, for linearity, and functional 
    relations.
    \cr
        
    The methods are:
    
    \tabular{ll}{
    \code{"bg"} \tab Breusch--Godfrey test for higher order serial correlation, \cr
    \code{"bp"} \tab Breusch--Pagan test for heteroskedasticity, \cr
    \code{"dw"} \tab Durbin--Watson test for autocorrelation of disturbances, \cr
    \code{"gq"} \tab Goldfeld--Quandt test for heteroskedasticity, \cr
    \code{"harv"} \tab Harvey--Collier test for linearity, \cr
    \code{"hmc"} \tab Harrison--McCabe test for heteroskedasticity, \cr
    \code{"rain"} \tab Rainbow test for linearity, and \cr
    \code{"reset"} \tab Ramsey's RESET test for functional relation. }
    
    There is nothing new, it's just a wrapper to the underlying test
    functions from R's contributed package \code{lmtest}. The functions
    are available as "Builtin" functions. Nevertheless, the user can 
    still install and use the original functions from \R's \code{lmtest} 
    package.
    
}


\usage{
lmTest(formula, method = c("bg", "bp", "dw", "gq", "harv", "hmc", 
    "rain", "reset"), data = list(), \dots)
    
bgTest(formula, order = 1, type = c("Chisq", "F"), data = list())
bpTest(formula, varformula = NULL, studentize = TRUE, data = list())
dwTest(formula, alternative = c("greater", "two.sided", "less"),
    iterations = 15, exact = NULL, tol = 1e-10, data = list())
gqTest(formula, point=0.5, order.by = NULL, data = list())
harvTest(formula, order.by = NULL, data = list())
hmcTest(formula, point = 0.5, order.by = NULL, simulate.p = TRUE, 
    nsim = 1000, plot = FALSE, data = list()) 
rainTest(formula, fraction = 0.5, order.by = NULL, center = NULL, 
    data = list())
resetTest(formula, power = 2:3, type = c("fitted", "regressor", "princomp"), 
    data = list())
}


\arguments{

    \item{alternative}{
        [dwTest] - \cr
        a character string specifying the alternative hypothesis, either
        \code{"greater"}, \code{"two.sided"}, or \code{"less"}.
        }
    \item{center}{
        [rainTest] - \cr
        a numeric value. If center is smaller than \code{1} it is 
        interpreted as percentages of data, i.e. the subset is chosen 
        that \code{n*fraction} observations are around observation 
        number \code{n*center}. If \code{center} is greater than 
        \code{1} it is interpreted to be the index of the center of 
        the subset. By default center is \code{0.5}. If the Mahalanobis 
        distance is chosen center is taken to be the mean regressor, 
        but can be specified to be a k-dimensional vector if k is the 
        number of regressors and should be in the range of the 
        respective regressors. 
        }
    \item{data}{
        an optional data frame containing the variables in the model. 
        By default the variables are taken from the environment which 
        \code{lmTest} and the other tests are called from.
        }
    \item{exact}{
        [dwTest] - \cr
        a logical flag. If set to \code{FALSE} a normal approximation 
        will be used to compute the p value, if \code{TRUE} the "pan" 
        algorithm is used. The default is to use "pan" if the sample size 
        is \code{< 100}. 
        }
    \item{formula}{
        a symbolic description for the linear model to be tested.
        }
    \item{fraction}{
        [rainTest] - \cr
        a numeric value, by default 0.5. The percentage of observations 
        in the subset is determined by \code{fraction*n} if \code{n} 
        is the number of observations in the model. 
        }
    \item{iterations}{
        [dwTest] - \cr
        an integer specifying the number of iterations when calculating
        the p-value with the "pan" algorithm. By default 15.
        }
    \item{method}{
        the test method which should be applied.
        }
    \item{nsim}{
        [hmcTest] - \cr
        an integer value. Determins how many runs are used to 
        simulate the p value, by default 1000.
        }
    \item{order}{ 
        [bgTest] - \cr
        an integer. The maximal order of serial correlation to be 
        tested. By default 1.
        }
    \item{order.by}{
        [gqTest][harvTest] - \cr
        a formula. A formula with a single explanatory variable like 
        \code{~ x}. Then the observations in the model are ordered by 
        the size of \code{x}. If set to \code{NULL}, the default, the 
        observations are assumed to be ordered (e.g. a time series). \cr
        [rainTest] - \cr
        either a formula or a string. A formula with a single explanatory 
        variable like \code{~ x}. The observations in the model are 
        ordered by the size of \code{x}. If set to \code{NULL}, the default, 
        the observations are assumed to be ordered (e.g. a time series). 
        If set to \code{"mahalanobis"} then the observations are ordered 
        by their Mahalanobis distance of the data. 
        }       
    \item{plot}{
        [hmcTest] - \cr
        a logical flag. If \code{TRUE} the test statistic for all  
        possible breakpoints is plotted, the default is \code{FALSE}. 
        }   
    \item{point}{
        [gqTest][hmcTest] - \cr
        a numeric value. If point is smaller than \code{1} it is 
        interpreted as percentages of data, i.e. \code{n*point} is 
        taken to be the (potential) breakpoint in the variances, if 
        \code{n} is the number of observations in the model. If 
        \code{point} is greater than \code{1} it is interpreted to 
        be the index of the breakpoint. By default \code{0.5}.
        }
    \item{power}{
        [resetTest] - \cr
        integers, by default \code{2:3}. A vector of positive integers 
        indicating the powers of the variables that should be included. 
        By default it is tested for a quadratic or cubic influence of 
        the fitted response. 
        }
    \item{simulate.p}{
        [hmcTest] - \cr
        a logical. If \code{TRUE}, the default, a p-value will be 
        assessed by simulation, otherwise the p-value is \code{NA}. 
        }
    \item{studentize}{
        [bpTest] - \cr 
        a logical value. If set to \code{TRUE} 
        Koenker's studentized version of the test statistic will 
        be used. By default set to \code{TRUE}.
        } 
    \item{tol}{
        [dwTest] - \cr
        the tolerance value. Eigenvalues computed have to be greater than 
        \code{tol=1e-10} to be treated as non-zero. 
        }
    \item{type}{
        [bgTest] - \cr
        the type of test statistic to be returned. Either \code{"Chisq"} 
        for the Chi-squared test statistic or \code{"F"} for the F test 
        statistic. \cr
        [resetTest] - \cr
        a string indicating whether powers of the \code{"fitted"} 
        response, the \code{"regressor"} variables (factors are left 
        out) or the first principal component, \code{"princomp"}, of 
        the regressor matrix should be included in the extended model. 
        }           
    \item{varformula}{
        [bpTest] - \cr
        a formula describing only the potential explanatory variables 
        for the variance, no dependent variable needed. By default the 
        same explanatory variables are taken as in the main regression 
        model. 
        }   
    \item{\dots}{
        [regTest] - \cr
        additional arguments passed to the underlying lm test. Some of 
        the tests can specify additional optional arguments like for
        alternative hypothesis, the type of test statistic to be returned,
        or others. All the optional arguments have default settings. 
        }   
  
}


\details{
    
    \bold{bg -- Breusch Godfrey Test:}
    \cr\cr  
    Under \eqn{H_0} the test statistic is asymptotically Chi-squared 
    with degrees of freedom as given in \code{parameter}.
    If \code{type} is set to \code{"F"} the function returns
    the exact F statistic which, under \eqn{H_0}, follows an \eqn{F}
    distribution with degrees of freedom as given in \code{parameter}.
    The starting values for the lagged residuals in the supplementary
    regression are chosen to be 0.\cr
    \code{[lmtest:bgtest]}
    \cr
    
    
    \bold{bp -- Breusch Pagan Test:}
    \cr\cr
    The Breusch--Pagan test fits a linear regression model to the 
    residuals of a linear regression model (by default the same 
    explanatory variables are taken as in the main regression
    model) and rejects if too much of the variance
    is explained by the additional explanatory variables.
    Under \eqn{H_0} the test statistic of the Breusch-Pagan test 
    follows a chi-squared distribution with \code{parameter} 
    (the number of regressors without the constant in the model) 
    degrees of freedom.\cr  
    \code{[lmtest:bptest]}
    \cr
    
    
    \bold{dw -- Durbin Watson Test:}
    \cr\cr
    The Durbin--Watson test has the null hypothesis that the autocorrelation
    of the disturbances is 0; it can be tested against the alternative 
    that it is greater than, not equal to, or less than 0 respectively. 
    This can be specified by the \code{alternative} argument.
    The null distribution of the Durbin-Watson test statistic is a linear
    combination of chi-squared distributions. The p value is computed using a
    Fortran version of the Applied Statistics Algorithm AS 153 by Farebrother
    (1980, 1984). This algorithm is called "pan" or "gradsol". For large sample
    sizes the algorithm might fail to compute the p value; in that case a 
    warning is printed and an approximate p value will be given; this p 
    value is computed using a normal approximation with mean and variance 
    of the Durbin-Watson test statistic.\cr
    \code{[lmtest:dwtest]}
    \cr
        
    
    \bold{gq -- Goldfeld Quandt Test:}
    \cr\cr
    The Goldfeld--Quandt test compares the variances of two submodels
    divided by a specified breakpoint and rejects if the variances differ.
    Under \eqn{H_0} the test statistic of the Goldfeld-Quandt test 
    follows an F distribution with the degrees of freedom as given in 
    \code{parameter}.\cr
    \code{[lmtest:gqtest]}
    \cr
    
    
    \bold{harv - Harvey Collier Test:}
    \cr\cr
    The Harvey-Collier test performs a t-test (with \code{parameter} 
    degrees of freedom) on the recursive residuals. If the true relationship 
    is not linear but convex or concave the mean of the recursive residuals 
    should differ from 0 significantly.\cr
    \code{[lmtest:harvtest]}
    \cr
    
    
    \bold{hmc -- Harrison McCabe Test:}
    \cr\cr  
    The Harrison--McCabe test statistic is the fraction of the residual 
    sum of squares that relates to the fraction of the data before the 
    breakpoint. Under \eqn{H_0} the test statistic should be close to 
    the size of this fraction, e.g. in the default case close to 0.5. 
    The null hypothesis is reject if the statistic is too small.\cr
    \code{[lmtest:hmctest]}
    \cr
    
    
    \bold{rain -- Rainbow Test:}
    \cr\cr  
    The basic idea of the Rainbow test is that even if the true 
    relationship is non-linear, a good linear fit can be achieved 
    on a subsample in the "middle" of the data. The null hypothesis 
    is rejected whenever the overall fit is significantly inferious 
    to the fit of the subsample. The test statistic under \eqn{H_0} 
    follows an F distribution with \code{parameter} degrees of 
    freedom.\cr
    \code{[lmtest:raintest]}
    \cr
    
    
    \bold{reset -- Ramsey's RESET Test}
    \cr\cr  
    RESET test is popular means of diagnostic for correctness of 
    functional form. The basic assumption is that under the alternative, 
    the model can be written by the regression
    \eqn{ y = X\beta + Z\gamma + u}{y=X * beta + Z * gamma}.
    \code{Z} is generated by taking powers either of the fitted response, 
    the regressor variables or the first principal component of \code{X}. 
    A standard F-Test is then applied to determin whether these additional 
    variables have significant influence. The test statistic under 
    \eqn{H_0} follows an F distribution with \code{parameter} degrees 
    of freedom.\cr
    \code{[lmtest:reset]}

}


\value{

    A list with class \code{"htest"} containing the following components:

    \item{statistic}{
        the value of the test statistic.
        }
    \item{parameter}{
        the lag order.
        }
    \item{p.value}{
        the p-value of the test.
        }
    \item{method}{
        a character string indicating what type of test was
        performed.
        } 
    \item{data.name}{
        a character string giving the name of the data.
        }
    \item{alternative}{
        a character string describing the alternative
        hypothesis.
        }
        
}


\note{

    The underlying \code{lmtest} package comes wit a lot of helpful
    examples. We highly recommend to install the \code{lmtest} package
    and to study the examples given therein.

}


\references{

Breusch, T.S. (1979);
    \emph{Testing for Autocorrelation in Dynamic Linear Models}, 
    Australian Economic Papers 17, 334--355.
 
Breusch T.S. and Pagan A.R. (1979);
    \emph{A Simple Test for Heteroscedasticity and Random 
    Coefficient Variation},
    Econometrica 47, 1287--1294
     
Durbin J. and Watson G.S. (1950);
    \emph{Testing for Serial Correlation in Least Squares Regression I},
    Biometrika 37, 409--428.

Durbin J. and Watson G.S. (1951);
    \emph{Testing for Serial Correlation in Least Squares Regression II},
    Biometrika 38, 159--178.

Durbin J. and Watson G.S. (1971);
    \emph{Testing for Serial Correlation in Least Squares Regression III},
    Biometrika 58, 1--19.
    
Farebrother R.W. (1980);
    \emph{Pan's Procedure for the Tail Probabilities of the
    Durbin-Watson Statistic},
    Applied Statistics 29, 224--227.

Farebrother R.W. (1984);
    \emph{The Distribution of a Linear Combination of
    \eqn{\chi^2}{chi^2} Random Variables}, 
    Applied Statistics 33, 366--369.

Godfrey, L.G. (1978);
    \emph{Testing Against General Autoregressive and
    Moving Average Error Models when the Regressors Include Lagged
    Dependent Variables}, 
    Econometrica 46, 1293--1302.

Goldfeld S.M. and Quandt R.E. (1965);
    \emph{Some Tests for Homoskedasticity}
    Journal of the American Statistical Association 60, 539--547.
    
Harrison M.J. and McCabe B.P.M. (1979);
    \emph{A Test for Heteroscedasticity based on Ordinary Least 
    Squares Residuals}
    Journal of the American Statistical Association 74, 494--499.

Harvey A. and Collier P. (1977);
    \emph{Testing for Functional Misspecification in Regression 
    Analysis},
    Journal of Econometrics 6, 103--119.
    
Johnston, J. (1984); 
    \emph{Econometric Methods}, 
    Third Edition, McGraw Hill Inc.

Kraemer W. and Sonnberger H. (1986);
    \emph{The Linear Regression Model under Test}, 
    Heidelberg: Physica.
    
Racine J. and Hyndman R. (2002);
    \emph{Using R To Teach Econometrics},
    Journal of Applied Econometrics 17, 175--189.
    
Ramsey J.B. (1969);
    \emph{Tests for Specification Error in Classical Linear Least 
    Squares Regression Analysis},
    Journal of the Royal Statistical Society, Series B 31, 350--371.
    
Utts J.M. (1982);
    \emph{The Rainbow Test for Lack of Fit in Regression},
    Communications in Statistics - Theory and Methods 11, 1801--1815.
    
}


\author{

    Achim Zeileis and Torsten Hothorn for the \code{lmtest} package, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## bg | dw -
   # Generate a Stationary and an AR(1) Series:
   x = rep(c(1, -1), 50)
   y1 = 1 + x + rnorm(100)
   # Perform Breusch-Godfrey Test for 1st order serial correlation:
   lmTest(y1 ~ x, "bg")
   # ... or for fourth order serial correlation:
   lmTest(y1 ~ x, "bg", order = 4)    
   # Compare with Durbin-Watson Test Results:
   lmTest(y1 ~ x, "dw")
   y2 = filter(y1, 0.5, method = "recursive")
   lmTest(y2 ~ x, "bg") 
   
## bp -
   # Generate a Regressor:
   x = rep(c(-1, 1), 50)
   # Generate heteroskedastic and homoskedastic Disturbances
   err1 = rnorm(100, sd = rep(c(1, 2), 50))
   err2 = rnorm(100)
   # Generate a Linear Relationship:
   y1 = 1 + x + err1
   y2 = 1 + x + err2
   # Perform Breusch-Pagan Test
   bp = lmTest(y1 ~ x, "bp")
   bp
   # Calculate Critical Value for 0.05 Level
   qchisq(0.95, bp$parameter)
   lmTest(y2 ~ x, "bp")
   
## dw -
   # Generate two AR(1) Error Terms 
   # with parameter rho = 0 (white noise) 
   # and rho = 0.9 respectively
   err1 = rnorm(100)
   # Generate Regressor and Dependent Variable
   x = rep(c(-1,1), 50)
   y1 = 1 + x + err1
   # Perform Durbin-Watson Test:
   lmTest(y1 ~ x, "dw")
   err2 = filter(err1, 0.9, method = "recursive")
   y2 = 1 + x + err2
   lmTest(y2 ~ x, "dw")
   
## gq -
   # Generate a Regressor:
   x = rep(c(-1, 1), 50)
   # Generate Heteroskedastic and Homoskedastic Disturbances:
   err1 = c(rnorm(50, sd = 1), rnorm(50, sd = 2))
   err2 = rnorm(100)
   # Generate a Linear Relationship:
   y1 = 1 + x + err1
   y2 = 1 + x + err2
   # Perform Goldfeld-Quandt Test:
   lmTest(y1 ~ x, "gq")
   lmTest(y2 ~ x, "gq")
   
## harv -
   # Generate a Regressor and Dependent Variable:
   x = 1:50
   y1 = 1 + x + rnorm(50)
   y2 = y1 + 0.3*x^2
   # Perform Harvey-Collier Test:
   harv = lmTest(y1 ~ x, "harv")
   harv
   # Calculate Critical Value vor 0.05 level:
   qt(0.95, harv$parameter)
   lmTest(y2 ~ x, "harv")
   
## hmc -
   # Generate a Regressor:
   x = rep(c(-1, 1), 50)
   # Generate Heteroskedastic and Homoskedastic Disturbances:
   err1 = c(rnorm(50, sd = 1), rnorm(50, sd = 2))
   err2 = rnorm(100)
   # Generate a Linear Relationship:
   y1 = 1 + x + err1
   y2 = 1 + x + err2
   # Perform Harrison-McCabe Test:
   lmTest(y1 ~ x, "hmc")
   lmTest(y2 ~ x, "hmc")
   
## rain -
   # Generate Series:
   x = c(1:30)
   y = x^2 + rnorm(30, 0, 2)
   # Perform rainbow Test
   rain = lmTest(y ~ x, "rain")
   rain
   # Compute Critical Value:
   qf(0.95, rain$parameter[1], rain$parameter[2]) 
   
## reset -
   # Generate Series:
   x = c(1:30)
   y1 = 1 + x + x^2 + rnorm(30)
   y2 = 1 + x + rnorm(30)
   # Perform RESET Test:
   lmTest(y1 ~ x , "reset", power = 2, type = "regressor")
   lmTest(y2 ~ x , "reset", power = 2, type = "regressor")          
}


\keyword{htest}

