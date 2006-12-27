\name{NormalityTests}

\alias{NormalityTests}

\alias{normalTest}
\alias{ksnormTest}
\alias{shapiroTest}
\alias{jarqueberaTest} 

\alias{dagoTest} 

\alias{jbTest}

% from nortest:

\alias{adTest}             
\alias{cvmTest}     
\alias{lillieTest} 
\alias{pchiTest}    
\alias{sfTest}

\alias{runsTest}


\title{Normality Tests}


\description{
    
    A collection and description of functions of one
    sample tests for testing normality of financial
    return series.
    \cr
    
    The functions for testing normality are:

    \tabular{ll}{
    \code{ksnormTest} \tab Kolmogorov-Smirnov normality test, \cr
    \code{shapiroTest} \tab Shapiro-Wilk's test for normality, \cr
    \code{jarqueberaTest} \tab Jarque--Bera test for normality, \cr
    \code{dagoTest} \tab D'Agostino normality test. }
    
    Functions for high precision Jarque Bera LM and ALM tests:
      
    \tabular{ll}{
    \code{jbTest} \tab Performs finite sample adjusted JB LM and ALM test. }       
    
    Additional functions for testing normality from the 'nortest' package:
    
    \tabular{ll}{
    \code{adTest} \tab Anderson--Darling normality test, \cr
    \code{cvmTest} \tab Cramer--von Mises normality test, \cr
    \code{lillieTest} \tab Lilliefors (Kolmogorov-Smirnov) normality test, \cr
    \code{pchiTest} \tab Pearson chi--square normality test, \cr
    \code{sfTest} \tab Shapiro--Francia normality test. }  
       
    For SPlus/Finmetrics Compatibility:
    
    \tabular{ll}{
    \code{normalTest} \tab test suite for some normality tests. }     
    
    More tests ...
    
    \tabular{ll}{
    \code{runsTest} \tab Runs test for detecting non-randomness.}
 
}


\usage{ 
ksnormTest(x, title = NULL, description = NULL)

jbTest(x, title = NULL, description = NULL)
shapiroTest(x, title = NULL, description = NULL)
normalTest(x, method = c("sw", "jb"), na.rm = FALSE) 

jarqueberaTest(x, title = NULL, description = NULL)
dagoTest(x, title = NULL, description = NULL)

adTest(x, title = NULL, description = NULL)            
cvmTest(x, title = NULL, description = NULL)      
lillieTest(x, title = NULL, description = NULL) 
pchiTest(x, title = NULL, description = NULL)    
sfTest(x, title = NULL, description = NULL) 

runsTest(x)
}


\arguments{
  
    \item{description}{
        optional description string, or a vector of character strings.
        }
    \item{method}{
        [normalTest] - \cr
        indicates four different methods for the normality test, 
        \code{"ks"} for the Kolmogorov-Smirnov one--sample test, 
        \code{"sw"} for the Shapiro-Wilk test,
        \code{"jb"} for the Jarque-Bera Test, and
        \code{"da"} for the D'Agostino Test. 
        The default value is \code{"ks"}.
        } 
    \item{na.rm}{ 
        [normalTest] - \cr
        a logical value. Should missing values removed before computing
        the tests? The default value is \code{FALSE}. 
        }         
    \item{title}{
        an optional title string, if not specified the inputs data 
        name is deparsed.
        }
    \item{x}{
        a numeric vector of data values or a S4 object of class 
        \code{timeSeries}.
        }
       
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
        
    The meaning of the elements of the \code{@test} slot is the following:
        
    \code{ksnormTest} 
    \cr
    returns the values for the 'D' statistic and p-values for the three 
    alternatives 'two-sided, 'less' and 'greater'. 
    
    \code{shapiroTest} 
    \cr
    returns the values for the 'W' statistic and the p-value.  
    
    \code{jarqueberaTest}\cr
    \code{jbTest}
    \cr
    returns the values for the 'Chi-squared' statistic with 2 degrees of
    freedom, and the asymptotic p-value. \code{jbTest} is the finite sample
    version of the Jarque Bera Lagrange multiplier, LM, and adjusted
    Lagrange multiplier test, ALM.
          
    \code{dagoTest} 
    \cr
    returns the values for the 'Chi-squared', the 'Z3' (Skewness) and 'Z4'
    (Kurtosis) statistic together with the corresponding p values.
    
    \code{adTest} 
    \cr
    returns the value for the 'A' statistic and the p-value. 
    
    \code{cvmTest} 
    \cr
    returns the value for the 'W' statistic and the p-value.  
    
    \code{lillieTest} 
    \cr
    returns the value for the 'D' statistic and the p-value.  
    
    \code{pchiTest} 
    \cr
    returns the value for the 'P' statistic and the p-values for the
    adjusted and not adjusted test cases.  In addition the number of 
    classes is printed, taking the default value due to Moore (1986)
    computed from the expression \code{n.classes = ceiling(2 * (n^(2/5)))},
    where \code{n} is the number of observations.
    
    \code{sfTest} 
    \cr
    returns the value for the 'W' statistic and the p-value.  
    
}


\details{

    The hypothesis tests may be of interest for many financial
    and economic applications, especially for the investigation 
    of univariate time series returns. 
    \cr
    
    \bold{Normal Tests:}
    \cr\cr
    Several tests for testing if the records from a data set are normally
    distributed are available. The input to all these functions may be
    just a vector \code{x} or a univariate time series object \code{x} 
    of class \code{timeSeries}. 
    
    First there exists a wrapper function which allows to call one from 
    two normal tests either the Shapiro--Wilks test or the Jarque--Bera 
    test. This wrapper was introduced for compatibility with S-Plus' 
    FinMetrics package. 
    
    Also available are the Kolmogorov--Smirnov one sample test and the 
    D'Agostino normality test. 
    
    The remaining five normal tests are the Anderson--Darling test, 
    the Cramer--von Mises test, the Lilliefors (Kolmogorov--Smirnov) 
    test, the Pearson chi--square test, and the Shapiro--Francia test. 
    They are calling functions from R's contributed package \code{nortest}.   
    The difference to the original test functions implemented in R and 
    from contributed R packages is that the Rmetrics functions accept
    time series objects as input and give a more detailed output report.
    
    The Anderson-Darling test is used to test if a sample of data came 
    from a population with a specific distribution, here the normal 
    distribution. The \code{adTest} goodness-of-fit test can be
    considered as a modification of the Kolmogorov--Smirnov test which 
    gives more weight to the tails than does the \code{ksnormTest}. 
    \cr
    
    \bold{Runs Test:}
    \cr\cr
    The runs test can be used to decide if a data set is from a random 
    process. A run is defined as a series of increasing values or a 
    series of decreasing values. The number of increasing, or decreasing, 
    values is the length of the run. In a random data set, the probability 
    that the \emph{(i+1)-th} value is larger or smaller than the \emph{i-th} 
    value follows a binomial distribution, which forms the basis of the 
    runs test. 

}


\note{

    Some of the test implementations are selected from R's \code{ctest} 
    and \code{nortest} packages. 
    
}


\references{
Anderson T.W., Darling D.A. (1954); 
    \emph{A Test of Goodness of Fit},
    JASA 49:765--69.
    
Conover, W. J. (1971);
    \emph{Practical nonparametric statistics},
    New York: John Wiley & Sons.

D'Agostino R.B., Pearson E.S. (1973); 
    \emph{Tests for Departure from Normality},
    Biometrika 60, 613--22.

D'Agostino R.B., Rosman B. (1974); 
    \emph{The Power of Geary's Test of Normality},
    Biometrika 61, 181--84.

Durbin J. (1961); 
    \emph{Some Methods of Constructing Exact Tests},
    Biometrika 48, 41--55. 

Durbin,J. (1973);
    \emph{Distribution Theory Based on the Sample Distribution Function},
    SIAM, Philadelphia.

Geary R.C. (1947); 
    \emph{Testing for Normality}; 
    Biometrika 36, 68--97.

Lehmann E.L. (1986); 
    \emph{Testing Statistical Hypotheses}, 
    John Wiley and Sons, New York.

Linnet K. (1988); 
    \emph{Testing Normality of Transformed Data},
    Applied Statistics 32, 180--186. 
    
Moore, D.S. (1986);
    \emph{Tests of the chi-squared type}, 
    In: D'Agostino, R.B. and Stephens, M.A., eds., 
    Goodness-of-Fit Techniques, Marcel Dekker, New York. 

Shapiro S.S., Francia R.S. (1972); 
    \emph{An Approximate Analysis of Variance Test for Normality},
    JASA 67, 215--216.

Shapiro S.S., Wilk M.B., Chen V. (1968); 
    \emph{A Comparative Study of Various Tests for Normality},
    JASA 63, 1343--72.

Thode H.C. (2002);
    \emph{Testing for Normality}, 
    Marcel Dekker, New York. 

Weiss M.S. (1978); 
    \emph{Modification of the Kolmogorov-Smirnov 
        Statistic for Use with Correlated Data}, 
    JASA 73, 872--75.
    
Wuertz D., Katzgraber H.G. (2005);
    \emph{Precise finite-sample quantiles of the
        Jarque-Bera adjusted Lagrange multiplier test},
    ETHZ Preprint.
}


\author{

    R-core team for the tests from \R's ctest package,\cr
    Adrian Trapletti for the runs test from \R's tseries package,\cr
    Juergen Gross for the normal tests from \R's nortest package,\cr
    James Filliben for the Fortran program producing the runs report,\cr
    Diethelm Wuertz and Helmut Katzgraber for the finite sample JB tests, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.5B-OneSampleTests")

## Series:
   x = rnorm(100)
   
## ksnormTests - 
   # Kolmogorov - Smirnov One-Sampel Test
   ksnormTest(x)

## shapiroTest - Shapiro-Wilk Test
   shapiroTest(x)

## jarqueberaTest - 
## jbTest - Jarque-Bera Test
   # jarqueberaTest(x)
   # jbTest(x)
   
## runsTest -
   runsTest(x)

}


\keyword{htest}

