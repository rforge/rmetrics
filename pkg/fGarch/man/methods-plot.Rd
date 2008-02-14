\name{plot-methods}


\docType{methods}


\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,fGARCH,missing-method}


\title{GARCH Plot Methods}


\description{

    Plot methods for GARCH Modelling. 

}


\usage{
\S4method{plot}{fGARCH,missing}(x, which = "ask", \dots)
}


\arguments{

    \item{x}{
        an object of class \code{"fREG"}
        } 
    \item{which}{
        a character string denoting which plot should be displayed.
        }
    \item{\dots}{
        optional arguments to be passed.
        }
 
}


\section{Methods}{
    \describe{

    \item{x = "ANY", y = "ANY"}{
        Generic function.
        }
    \item{x = "fGARCH", y = "missing"}{
        Plot function for objects of class \code{"fGARCH"}.
        }
        
    }
}


\details{

    The generic function \code{plot} allows to display 13 graphs. These
    are the \cr
    
            Time SeriesPlot\cr
            Conditional Standard Deviation Plot\cr
            Series Plot with 2 Conditional SD Superimposed\cr
            Autocorrelation function Plot of Observations\cr
            Autocorrelation function Plot of Squared Observations\cr
            Cross Correlation Plot\cr
            Residuals Plot\cr
            Conditional Standard Deviations Plot\cr
            Standardized Residuals Plot\cr
            ACF Plot of Standardized Residuals\cr
            ACF Plot of Squared Standardized Residuals\cr
            Cross Correlation Plot between $r^2$ and r\cr
            Quantile-Quantile Plot of Standardized Residuals\cr
            
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSim -
   # Default Garch(1,1) Model:
   x = garchSim(n = 200)
   head(x) 

## garchFit - 
   fit = garchFit(formula = ~ garch(1, 1), data = x, trace = FALSE)
   
## Batch Plot:
   plot(fit, which = 3)
   
\dontrun{
## Plot:
   # Interactive Plot:
   plot(fit) 
}
}


\keyword{models}

