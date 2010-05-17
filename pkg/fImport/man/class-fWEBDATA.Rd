\name{fWEBDATA-class}


\docType{class}


\alias{fWEBDATA-class}


\title{Class "fWEBDATA"}


\description{ 

    The class fWEBDATA represents a download from the internet.
}


\section{Objects from the Class}{

    Objects can be created by calls of the import or series
    functions.
    
}


\section{Slots}{
    \describe{
    
    \item{\code{call}:}{Object of class \code{"call"}: 
        the call of the applied function.
        } 
    \item{\code{data}:}{Object of class \code{"data.frame"}:
        the data as downloaded formatted as a data.frame.
        }
    \item{\code{param}:}{Object of class \code{"character"}:
        a character vector whose elements contain the values of selected 
        parameters of the argument list.
        }
    \item{\code{title}:}{Object of class \code{"character"}:
        a character string with the name of the download. This can be 
        overwritten specifying a user defined input argument.
        }
    \item{\code{description}:}{Object of class \code{" character"}:
        a character string with an optional user defined description. 
        By default just the current date and user when the test was 
        applied will be returned.}  
        
    }
    
}       


\section{Methods}{
    \describe{
    
    \item{show}{\code{signature(object = "fWEBDATA")}: 
        prints an object of class 'fWEBDATA'. 
        }
        
    }
}


\references{

Diethelm Wuertz, Yohan Chalabi, and Andrew Ellis, (2010);
    \emph{Financial Market Data for R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich,
    www.rmetrics.org.
    
}


\keyword{data}

