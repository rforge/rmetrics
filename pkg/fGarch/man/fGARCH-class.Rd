\name{fGARCH-class}


\docType{class}


\alias{fGARCH-class}


\title{Class "fGARCH"}


\description{ 

    The class fGARCH represents a model of an heteroskedastic
    time series process.
}


\section{Objects from the Class}{
    Objects can be created by calls of the form 
    \code{garchFit(formula, data)}.
    This object is a parameter estimate of an empirical GARCH process.
}


\section{Slots}{
    \describe{

    \item{\code{call}:}{Object of class \code{"call"}: 
        the call of the \code{garch} function.
        }\
    \item{\code{formula}:}{Object of class \code{"list"}: 
        a list with two formula entries, one for the mean and the other
        one for the variance equation.
        }
    \item{\code{method}:}{Object of class \code{"character"}: 
        a string denoting the optimization method, by default the 
        returneds string is "Max Log-Likelihood Estimation".
        }
    \item{\code{data}:}{Object of class \code{"list"}: 
        a list with one entry named \code{x}, containing the data of
        the time series to be estimated, the same as given by the
        input argument \code{series}.
        }
    \item{\code{fit}:}{Object of class \code{"list"}: 
        a list with the results from the parameter estimation. The entries
        of the list depend on the selected algorithm, see below.
        }
    \item{\code{residuals}:}{Object of class \code{"numeric"}: 
        a numeric vector with the residual values.
        }
    \item{\code{fitted}:}{Object of class \code{"numeric"}: 
        a numeric vector with the fitted values.
        }
    \item{\code{h.t}:}{Object of class \code{"numeric"}: 
        a numeric vector with the conditional variances.
        }
    \item{\code{sigma.t}:}{Object of class \code{"numeric"}: 
        a numeric vector with the conditional standard deviations.
        }
    \item{\code{title}:}{Object of class \code{"character"}: 
        a title string.
        }
    \item{\code{description}:}{Object of class \code{"character"}: 
        a string with a brief description.
        }
    
    }
}       


\section{Methods}{
    \describe{
    
    \item{plot}{\code{signature(x = "fGARCH", y = "missing)}: 
        plots an object of class 'fGARCH'
        }
    \item{show}{\code{signature(object = "fGARCH")}: 
        prints an object of class 'fGARCH'
        }      
    \item{summary}{\code{signature(object = "fGARCH")}: 
        summarizes an object of class 'fGARCH'
        }

    }
}


\author{

    DW
    
}


%\seealso{
%\code{\link{LogisParameter-class}}
%}


%\examples{
%XXX
%}



\keyword{distribution}


%\concept{xxx}

