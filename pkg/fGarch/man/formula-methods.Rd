\name{formula-methods}


\docType{methods}


\alias{formula-methods}
\alias{formula,ANY-method}
\alias{formula,fGARCH-method}


\title{Extract GARCH Model formula}


\description{

    Extracts formula from a formula GARCH object. 
    
}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function.
        }
    \item{object = "fGARCH"}{
        Extractor function for formula expression.
        }
        
    }
}


\details{

    The function extracts the \code{@formula} expression slot from 
    an object of class \code{"fGARCH"} as returned by the function
    \code{garchFit}. 
    
    Note, the returned formula has always a left hand 
    side. If the argument \code{data} was an univariate time series
    and no name was specified to the series, then the left hand side 
    has assigned the name of the data.set. In the multivariate case
    the rectangular \code{data} object must always have column names,
    otherwise the fitting will be stopped and you get the error 
    message  
    
    
    The class of the returned value depends on the input to the
    function \code{garchFit} who created the object. The returned
    value is always of the same class as the input object to the
    argument \code{data} in the function \code{garchFit}, i.e. if
    you fit a \code{"timeSeries"} object, you will get back from 
    the function \code{fitted} also a \code{"timeSeries"} object, 
    if you fit an object of class \code{"zoo"}, you will get back 
    again a \code{"zoo"} object. The same holds for a \code{"numeric"}
    vector, for a \code{"data.frame"}, and for objects of class
    \code{"ts", "mts"}.
    
    In contrast, the slot itself returns independent of the class 
    of the data input always a numceric vector, i.e. the function 
    call r\code{slot(object, "fitted")} will return a numeric vector.
    
}


\note{

    \code{formula} is a generic function which extracts the formula
    expression from objects returned by modeling functions. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchFit -
   fit = garchFit(~garch(1, 1), data = garchSim())
   
## formula - 
   formula(fit)

\dontrun{
## A Bivariate series and mis-specified formula:
   x = garchSim(n = 500)
   y = garchSim(n = 500)
   z = cbind(x, y)
   colnames(z)
   class(z)
   garchFit(z ~garch(1, 1), data = z, trace = FALSE)
   # Returns:
   # Error in .garchArgsParser(formula = formula, data = data, trace = FALSE) :  
   #   Formula and data units do not match.
   
## Doubled column names in data set - formula can't fit:
   colnames(z) <- c("x", "x")
   z[1:6,]
   garchFit(x ~garch(1, 1), data = z, trace = FALSE)
   # Again the error will be noticed:
   # Error in garchFit(x ~ garch(1, 1), data = z) : 
   #   Column names of data are not unique.

## Missing column names in data set - formula can't fit:
   colnames(z) <- NULL
   z[1:6,]
   garchFit(x ~ garch(1, 1), data = z, trace = FALSE)
   # Again the error will be noticed:
   # Error in .garchArgsParser(formula = formula, data = data, trace = FALSE) : 
   #   Formula and data units do not match
   # Note, you can use default columnames, the following call would work ...
   garchFit(V1 ~garch(1, 1), data = z, trace = FALSE)
}
}


\keyword{models}

