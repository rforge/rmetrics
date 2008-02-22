\name{show-methods}

\alias{show}


\alias{show.fWEBDATA}
\alias{show,fWEBDATA-method}


\title{Print Method for fWEBDATA objects}


\description{
    
    A collection and description of functions to import 
    financial and economic market data from the Internet.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{fWEBDATA} \tab Represents objects of class WEBDATA, \cr
    \code{show|print} \tab prins objects of class fWEBDATA.}
   
}


\usage{           
show.fWEBDATA(object)
}


\arguments{

    \item{object}{
        an S4 object of class \code{"fWEBDATA"}.
        }
}


\value{

    An S4 object of class \code{fWEBDATA} has the following slots:
    
    \item{@call}{
        the function call.  
        }
    \item{@data}{
        the data as downloaded formatted as a data.frame.
        }
    \item{@param}{
        a character vector whose elements contain the values of selected 
        parameters of the argument list.
        }
    \item{@title}{
        a character string with the name of the download. This can be 
        overwritten specifying a user defined input argument.
        }
    \item{@description}{
        a character string with an optional user defined description. 
        By default just the current date when the test was applied will
        be returned.}
   
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


%\examples{
%## 
%}


\keyword{data}

