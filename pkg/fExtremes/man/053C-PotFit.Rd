\name{PotFit}

\alias{PotFit}

\alias{potSim}
\alias{potFit}

\alias{print.potFit}
\alias{plot.potFit}
\alias{summary.potFit}

\title{Modelling Peaks Over a Threshold}


\description{
  
    a collection and description of functions to model 
    point processes over a threshold, POT, based on \R's 
    'evir' package. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{potSim} \tab generates data from a point process, \cr
    \code{potFit} \tab fits empirical or simulated data to a point process, \cr 
    \code{print} \tab print method for a fitted POT object of class ..., \cr
    \code{plot} \tab plot method for a fitted GEV object, \cr 
    \code{summary} \tab summary method for a fitted GEV object, \cr 
    \code{gevrlevelPlot} \tab k-block return level with confidence intervals. }
  
}


\usage{
potSim(x, threshold, nextremes = NA, run = NA)
potFit(x, threshold = NA, nextremes = NA, run = NA, \dots) 

\method{print}{potFit}(x, \dots)
\method{plot}{potFit}(x, which = "all", \dots)
\method{summary}{potFit}(object, doplot = TRUE, which  = "all", \dots)
}


\arguments{


    \item{doplot}{
        a logical. Should the results be plotted?
        }
    \item{nextremes}{
        the number of upper extremes to be used (either
        this or \code{threshold} must be given but not both).
        }
    \item{object}{
        [summary] - \cr
        a fitted object of class \code{"potFit"}.
        }
    \item{run}{
        if the data are to be declustered the run length parameter for 
        the runs method, see \code{\link{deCluster}}, should be entered 
        here.
        }
    \item{threshold}{
        a threshold value, either \code{threshold} or \code{nextremes}
        must be given, but not both).
        }
    \item{which}{
        if \code{which} is set to \code{ask} the function will 
        interactively ask which plot should be displayed. By default
        this value is set to \code{FALSE} and then those plots will
        be displayed for which the elements in the logical vector
        \code{which} ar set to \code{TRUE}; by default all four
        elements are set to \code{"all"}.
        }
    \item{x}{
        numeric vector of data, which may have a \code{times} attribute 
        containing (in an object of class \code{"POSIXct"}, or an object 
        that can be converted to that class; see \code{\link{as.POSIXct}}) 
        the times/dates of each observation.
        If no \code{times} attribute exists, the data are assumed to be
        equally spaced. Note, the argument name is different for 
        \code{potFit} and \code{pot}.
        \cr
        [print][plot] - \cr
        a fitted object of class \code{"potFit"}.
        }
    \item{\dots}{
        control parameters and plot parameters optionally passed to the 
        optimization and/or plot function. Parameters for the optimization
        function are passed to components of the \code{control} argument 
        of \code{optim}.
        }  
      
}


\value{
  
    Both, \code{potFit} and \code{pot} return an object of class \code{"pot"}
    describing the fit including parameter estimates and standard errors. 

}


\details{
  
    \bold{Parameter Estimation:}
    \cr\cr
    \code{potFit} uses \code{optim} for point process likelihood 
    maximization. 
    \cr
  
    \bold{Methods:}
    \cr\cr
    The plot method \code{plot.pot} provides seven different plots for 
    assessing fitted POT model. The user selects the plot type from a 
    menu. Plot 1 displays the exceedance process of the chosen threshold. 
    Plots 2-4  assess the Poisson nature of the exceedance process 
    by looking at the scaled gaps between exceedances, which should
    be iid unit exponentially distributed. Plots 5-6 assess the GPD
    nature of the excesses by looking at suitably defined residuals,
    which should again be iid unit exponentially distributed. Option
    8 allows the user to call GPD plotting functions. If plot 1 or 2 
    from the GPD plots is selected as the final plot (i.e. option 8 is 
    selected, followed by option 1 or 2), a list object containing 
    details of the plot is returned invisibly. This object should be 
    used as the first argument of \code{gpdqPlot} or \code{gpdsfallPlot} 
    to add quantile estimates or expected shortfall estimates to the plot.
}

  
\examples{
## SOURCE("fExtremes.53C-PotFit")

## Use Danish Fire Insurance Loss Data:
   data(danish)
   
## Fit:
   xmpExtremes("\nStart: POT Parameter Estimate >")
   fit = potFit(danish, threshold = 10)  
   print(fit)
   
## Summary with Diagnostic Plots:
   xmpExtremes("\nNext: Diagnostic Analysis >")
   par(mfrow = c(3, 3), cex = 0.5)
   summary(fit)
}


\keyword{models}
