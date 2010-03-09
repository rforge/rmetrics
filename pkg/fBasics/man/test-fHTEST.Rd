\name{fHTEST}


\alias{fHTEST}
\alias{fHTEST-class}

% \alias{show.fHTEST}
\alias{show,fHTEST-method}


\title{Tests Class Representation and Utilities}


\description{
    
    Class representation, methods and utility 
    functions for objects of class 'fHTEST'.
    \cr
    
    The class representation and methods are:

    \tabular{ll}{
    \code{fHTEST} \tab Representation for an S4 object of class "fHTEST", \cr
    \code{show} \tab S4 print method. }
         
}


\usage{
\S4method{show}{fHTEST}(object)
}


\arguments{
  
    \item{object}{
        [show] - \cr
        an S4 object of class \code{"fHTEST"}.
        }
       
}


\value{

    In contrast to R's output report from S3 objects of class \code{"htest"}
    a different output report is produced. The tests return an S4 object 
    of class \code{"fHTEST"}. The object contains the following slots:
    
    \item{@call}{
        the function call.  
        }
    \item{@data}{
        the data as specified by the input argument(s).
        }
    \item{@test}{
        a list whose elements contain the results from the statistical
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
        be returned.}
    
    
    The slot \code{@test} returns an object of class \code{"list"} 
    containing the following elements:

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


\examples{
## fHTEST -
   getClass("fHTEST")
   getSlots("fHTEST")

}


\keyword{htest}

