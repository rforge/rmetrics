\name{TwoSampleTests}

\alias{TwoSampleTests}

% two sample tests:

\alias{ks2Test}

%\alias{.locationTest}
\alias{tTest}
\alias{kw2Test}

%\alias{.varianceTest}
\alias{varfTest}
\alias{bartlett2Test}
\alias{fligner2Test}

%\alias{.scaleTest}
\alias{ansariTest}
\alias{moodTest}

%\alias{.correlationTest}
\alias{pearsonTest}
\alias{kendallTest}
\alias{spearmanTest}

% Distribution

\alias{dansariw}
\alias{pansariw}
\alias{qansariw}


\title{Two Sample Tests}


\description{
    
    A collection and description of functions for 
    two sample statistical tests. The functions allow 
    to test for distributional equivalence, for difference
    in location, variance and scale, and for correlations.
    \cr
      
    Distributional Equivalence:
    
    \tabular{ll}{
    \code{ks2Test} \tab Two sample Kolmogorov--Smirnov test.}
    
    Difference in Locations:
    
    \tabular{ll}{ 
    \code{tTest} \tab The t test, \cr
    \code{kw2Test} \tab the Kruskal--Wallis test. } 
    
    Difference in Variance:
    
    \tabular{ll}{ 
    \code{varfTest} \tab The variance F test, \cr
    \code{bartlett2Test} \tab the Bartlett test, \cr
    \code{fligner2Test} \tab the Fligner--Killeen test.}
     
    Difference in Scale:
    
    \tabular{ll}{
    \code{ansariTest} \tab  The Ansari--Bradley test, \cr
    \code{moodTest} \tab the Mood test.}
    
    Correlations:
    
    \tabular{ll}{
    \code{pearsonTest} \tab Pearson's coefficient, \cr
    \code{kendallTest} \tab Kendall's rho, \cr
    \code{spearmanTest} \tab Spearman's rho.}
   
    
    Test Distributions:
    
    \tabular{ll}{       
    \code{[dpq]ansariw} \tab Distribution of the Ansari W statistic. }
     
}


\usage{
ks2Test(x, y, title = NULL, description = NULL)

tTest(x, y, title = NULL, description = NULL) 
kw2Test(x, y, title = NULL, description = NULL)

varfTest(x, y, title = NULL, description = NULL)
bartlett2Test(x, y, title = NULL, description = NULL)
fligner2Test(x, y, title = NULL, description = NULL)

ansariTest(x, y, title = NULL, description = NULL)
moodTest(x, y, title = NULL, description = NULL)

pearsonTest(x, y, title = NULL, description = NULL)
kendallTest(x, y, title = NULL, description = NULL)
spearmanTest(x, y, title = NULL, description = NULL)

dansariw(x = NULL, m, n = m)
pansariw(q = NULL, m, n = m)
qansariw(p, m, n = m)
}


\arguments{
  
    \item{description}{
        optional description string, or a vector of character strings.
        }
    \item{m, n}{
        [*ansariw] - \cr
        } 
    \item{p}{
        [qansariw] - \cr
        a numeric vector of quantiles.
        }
    \item{q}{
        [pansariw] - \cr
        a numeric vector of quantiles.
        }
    \item{title}{
        an optional title string, if not specified the inputs data 
        name is deparsed.
        }
    \item{x, y}{
        a numeric vector of data values.
        \cr
        [bartlett2Test][fligner2Test][kw2Test] - \cr
        here \code{x} is a list, where each element is either a vector
        or an object of class \code{timeSeries}. \code{y} is only used
        for the two--sample test situation, where \code{x} and \code{y}
        are two vectors or objects of class \code{timeSeries}.
        \cr
        [dansariw] - \cr
        a numeric vector of quantiles.
        }
       
}


\value{

    In contrast to R's output report from S3 objects of class \code{"htest"}
    a different output report is produced. The classical tests presented
    here return an S4 object of class \code{"fHTEST"}. The object contains
    the following slots:
    
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
    containing (at least) the following elements:

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
        
    %The meaning of the elements of the \code{@test} slot is the following:
    
    %\code{ks2Test} 
    %\cr
    %returns the values for the \code{D}, \code{D^-} and \code{D^+} 
    %statistic and the corresponding p-values for the 'two-sided', 'less' 
    %and 'greater' alternatives.  
    
}


\details{
    
    The tests may be of interest for many financial 
    and economic applications, especially for the 
    comparison of two time series. The tests are grouped 
    according to their functionalities.
    \cr
    
    
    \bold{Distributional Equivalence:}
    \cr\cr  
    The test \code{ks2Test} performs a Kolmogorov--Smirnov two sample test 
    that the two data samples \code{x} and \code{y} come from the same 
    distribution, not necessarily a normal distribution. That means that 
    it is not specified what that common distribution is. 
    \cr
    
    \bold{Differences in Location:}
    \cr\cr  
    The function \code{tTest} can be used to determine if the two sample 
    means are equal for unpaired data sets. Two variants are used,
    assuming equal or unequal variances. 
  
    The function \code{kw2Test} performs a Kruskal-Wallis rank sum 
    test of the null hypothesis that the central tendencies or medians of 
    two samples are the same. The alternative is that they differ. 
    Note, that it is not assumed that the two samples are drawn from the 
    same distribution. It is also worth to know that the test assumes 
    that the variables under consideration have underlying continuous 
    distributions.
    \cr

    \bold{Differences in Variances:}
    \cr\cr    
    The function \code{varfTest} can be used to compare variances of two 
    normal samples performing an F test. The null hypothesis is that 
    the ratio of the variances of the populations from which they were 
    drawn is equal to one. 
    
    The function \code{bartlett2Test} performs the Bartlett's test of the 
    null hypothesis that the variances in each of the samples are the 
    same. This fact of equal variances across samples is also called 
    \emph{homogeneity of variances}. Note, that Bartlett's test is 
    sensitive to departures from normality. That is, if the samples 
    come from non-normal distributions, then Bartlett's test may simply 
    be testing for non-normality. The Levene test (not yet implemented)
    is an alternative to the Bartlett test that is less sensitive to 
    departures from normality. 
    
    The function \code{fligner2Test} performs the Fligner-Killeen test of 
    the null that the variances in each of the two samples are the same. 
    \cr
    
    \bold{Differences in Scale:}
    \cr\cr  
    The function \code{ansariTest} performs the Ansari--Bradley two--sample 
    test for a difference in scale parameters. Note, that we have completely 
    reimplemented this test based on the statistcs and p-values computed 
    from algorithm AS 93. The test returns for any sizes of the series 
    \code{x} and \code{y} the exact p value together with its asymptotic 
    limit. The test procedure is not limited to sizes shorter of length 50 
    as this is the case for the function \code{ansari.Test} implemented in 
    R's \code{stats} package. For the test statistics the following
    functions are available: \code{dansariw}, \code{pansariw}, and 
    \code{qansariw}.
    
    The function code{moodTest}, is another test which performs a 
    two--sample test for a difference in scale parameters. The underlying 
    model is that the two samples are drawn from \emph{f(x-l)} and 
    \emph{f((x-l)/s)/s}, respectively, where \emph{l} is a common 
    location parameter and \emph{s} is a scale parameter. The null 
    hypothesis is \emph{s=1}. 
    \cr
    
    
    \bold{Correlations:}
    \cr\cr  
    The function \code{correlationTest} tests for association 
    between paired samples, using Pearson's product moment 
    correlation coefficient, 
    
    The function \code{kendallTest} performs Kendall's tau test
    
    The function \code{spearmanTest} performs Spearman's rho test.
    
}

\note{

    Some of the test implementations are selected from \R's \code{ctest} 
    package.  
       
}


\references{

Conover, W. J. (1971);
    \emph{Practical nonparametric statistics},
    New York: John Wiley & Sons.

Durbin J. (1961); 
    \emph{Some Methods of Constructing Exact Tests},
    Biometrika 48, 41--55. 

Durbin,J. (1973);
    \emph{Distribution Theory Based on the Sample Distribution Function},
    SIAM, Philadelphia.

Lehmann E.L. (1986); 
    \emph{Testing Statistical Hypotheses}, 
    John Wiley and Sons, New York.
    
Moore, D.S. (1986);
    \emph{Tests of the chi-squared type}, 
    In: D'Agostino, R.B. and Stephens, M.A., eds., 
    Goodness-of-Fit Techniques, Marcel Dekker, New York. 

}


\author{

    R-core team for the tests from \R's ctest package,\cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.015E-TwoSampleTests")

## x, y -
   xmpBasics("\nStart: Create two Samples > ")
   x = rnorm(50)
   y = rnorm(50)
  
## ks2Test - 
   xmpBasics("\nNext: Distributional Tests > ")
   ks2Test(x, y)
   
## tTest | kw2Test - 
   xmpBasics("\nNext: Location Tests > ")
   tTest(x, y)
   kw2Test(x, y)
   
## varfTest, bartlett2Test | fligner2Test -
   xmpBasics("\nNext: Variance Tests > ")
   varfTest(x, y)
   bartlett2Test(x, y)
   fligner2Test(x, y)

## ansariTest | moodTest -
   xmpBasics("\nNext: Scale Tests > ")
   ansariTest(x, y)
   moodTest(x, y)
   
## pearsonTest | kendallTest | spearmanTest -
   xmpBasics("\nNext: Correlation Tests > ")
   pearsonTest(x, y)
   kendallTest(x, y)
   spearmanTest(x, y)
}


\keyword{htest}

