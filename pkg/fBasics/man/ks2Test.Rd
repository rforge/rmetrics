\name{ks2Test}


\alias{ks2Test}


\title{Two Sample Kolmogorov--Smirnov Test}


\description{
    
    Tests if two series are distributional equivalent.
    
}


\usage{
ks2Test(x, y, title = NULL, description = NULL)
}


\arguments{
  
    \item{x, y}{
        numeric vectors of data values.
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
        
}


\details{
     
    The test \code{ks2Test} performs a Kolmogorov--Smirnov two sample test 
    that the two data samples \code{x} and \code{y} come from the same 
    distribution, not necessarily a normal distribution. That means that 
    it is not specified what that common distribution is. 

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
  
## ks2Test - 
   ks2Test(x, y)
}


\keyword{htest}

