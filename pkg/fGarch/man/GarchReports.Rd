\name{garchReports}

\alias{garchReports}
}

\alias{fGARCH-class}

\alias{show.fGARCH}
\alias{show,fGARCH-method}

\alias{plot.fGARCH}
\alias{summary.fGARCH}


\title{GARCH Reports and Graphs}


\description{

    Creates reports and graphs of a GARCH modelling process. 

}


\usage{
show.fGARCH(object)

\method{plot}{fGARCH}(x, which = "ask", \dots)
\method{summary}{fGARCH}(object, \dots)
}


\arguments{
      
    \item{object, x}{
        an object of class \code{fGARCH} as returned from the 
        function \code{garchFit()}.
        }   
    \item{which}{
        if \code{which} is set to \code{"ask"} the function will 
        interactively ask which plot should be displayed. This is
        the default value and then those plots will
        be displayed for which the elements in the logical vector
        \code{which} ar set to \code{TRUE}; by default all four
        elements are set to \code{"all"}.
        }
    \item{\dots}{
        additional arguments to be passed.
        }   

}

    
\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSpec -
   spec = garchSpec()
   spec

## garchSim -
   x = garchSim(model = spec@model, n = 500)
   head(x) 

## garchFit - 
   # fit = garchFit(~garch(1, 1), data = x)
   # print(fit)
   ## Interactive Plot:
   ## plot(fit)
   ## Batch Plot:
   # plot(fit, which = 3)
   # summary(fit)
}


\keyword{models}

