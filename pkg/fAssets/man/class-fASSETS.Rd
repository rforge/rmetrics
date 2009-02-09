\name{fAssets}


\alias{fASSETS}
\alias{fASSETS-class}


\alias{show,fASSETS-method}
\alias{plot.fASSETS}
\alias{summary.fASSETS}


\title{fAssets class and methods}


\description{
    fAssets class and methods.
}
    
   
\usage{    
\S4method{show}{fASSETS}(object)
\method{plot}{fASSETS}(x, which = "ask", \dots)
\method{summary}{fASSETS}(object, which = "all", \dots)
}


\arguments{

    \item{object}{
        An object of class \code{fASSETS}. 
        }
    \item{x}{
        a numeric matrix of returns or any other rectangular object like 
        a data.frame or a multivariate time series object which can be 
        transformed by the function as.matrix to an object of class 
        matrix.   
        }   
    \item{which}{
        which of the five plots should be displayed? \code{which} can 
        be either a character string, "all" (displays all plots) or 
        "ask" (interactively asks which one to display), or a vector 
        of 5 logical values, for those elements which are set TRUE the 
        correponding plot will be displayed. 
        }      
    \item{\dots}{
        arguments to be passed. 
        } 
}
  

\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}

\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))[, 1:3]
   colnames(LPP)
   
## assetsFit -
   # Fit a Skew-Student-t Distribution:
   fit = assetsFit(LPP)
   
## fASSETS -
   class(fit)
   print(fit)
   plot(fit, 1)
   
## @model
   # Show Model Slot:
   print(fit@model)
}