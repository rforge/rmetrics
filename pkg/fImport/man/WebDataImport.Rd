\name{fWEBDATA-class}


\docType{class}


\alias{fWEBDATA-class}


\title{Class "fWEBDATA"}


\description{ 

    The class fWEBDATA represents a download from the internet.
}


\section{Objects from the Class}{

    Objects can be created by calls of the functions \code{*Import}.
    
}


\section{Slots}{
    \describe{

    An S4 object of class \code{fWEBDATA} has the following slots:
    
    \item{\code{call}:}{Object of class \code{"call"}: 
        the call of the \code{garch} function.
        }
    \item{\call{data}:}{Object of class \code{"data.frame"}:
        the data as downloaded formatted as a data.frame.
        }
    \item{\call{param}:}{Object of class \code{"character"}:
        a character vector whose elements contain the values of selected 
        parameters of the argument list.
        }
    \item{\call{title}:}{Object of class \code{"character"}:
        a character string with the name of the download. This can be 
        overwritten specifying a user defined input argument.
        }
    \item{\call{description}:}{Object of class \code{" character"}:
        a character string with an optional user defined description. 
        By default just the current date when the test was applied will
        be returned.}  
    }
    
}       


\section{Methods}{
    \describe{
    
    \item{show}{\code{signature(object = "fWEBDATA")}: 
        prints an object of class 'fGARCHSPEC'. 
        }
        
    }
}


\keyword{data}

