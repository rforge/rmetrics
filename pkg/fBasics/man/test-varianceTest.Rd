\name{varianceTest}


\alias{varianceTest}


\title{Two Sample Variance Tests}


\description{
    
    Tests if two series differ in their distributional
    variance parameter.
    
}


\usage{ 
varianceTest(x, y, method = c("varf", "bartlett", "fligner"), 
    title = NULL, description = NULL)
}


\arguments{
  
    \item{x, y}{
        numeric vectors of data values.
        }
    \item{method}{
        a character string naming which test should be applied.
        } 
    \item{title}{
        an optional title string, if not specified the inputs data 
        name is deparsed.
        }
    \item{description}{
        optional description string, or a vector of character strings.
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
        class \code{"htest"}.
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
}


\details{
    
    The \code{method="varf"} can be used to compare variances of two 
    normal samples performing an F test. The null hypothesis is that 
    the ratio of the variances of the populations from which they were 
    drawn is equal to one. 
    
    The \code{method="bartlett"} performs the Bartlett test of the 
    null hypothesis that the variances in each of the samples are the 
    same. This fact of equal variances across samples is also called 
    \emph{homogeneity of variances}. Note, that Bartlett's test is 
    sensitive to departures from normality. That is, if the samples 
    come from non-normal distributions, then Bartlett's test may simply 
    be testing for non-normality. The Levene test (not yet implemented)
    is an alternative to the Bartlett test that is less sensitive to 
    departures from normality. 
    
    The \code{method="fligner"} performs the Fligner-Killeen test of 
    the null that the variances in each of the two samples are the same. 
    
}


\note{

    Some of the test implementations are selected from \R's \code{ctest} 
    package.  
       
}


\references{

Conover, W. J. (1971);
    \emph{Practical nonparametric statistics},
    New York: John Wiley & Sons.

Lehmann E.L. (1986); 
    \emph{Testing Statistical Hypotheses}, 
    John Wiley and Sons, New York.

}


\author{

    R-core team for hypothesis tests implemented from \R's 
    package \code{ctest}.
    
}


\examples{
## rnorm - 
   # Generate Series:
   x = rnorm(50)
   y = rnorm(50)
   
## varianceTest -
   varianceTest(x, y, "varf")
   varianceTest(x, y, "bartlett")
   varianceTest(x, y, "fligner")
}


\keyword{htest}

