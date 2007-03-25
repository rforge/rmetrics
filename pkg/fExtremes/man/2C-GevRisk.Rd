\name{GevRisk}

\alias{GevRisk}

\alias{gevrlevelPlot}


\title{Generalized Extreme Value Modelling}


\description{
  
    A collection and description functions to estimate 
    the parameters of the GEV distribution. To model
    the GEV three types of approaches for parameter 
    estimation are provided: Maximum likelihood
    estimation, probability weighted moment method,
    and estimation by the MDA approach. MDA includes
    functions for the Pickands, Einmal-Decker-deHaan, 
    and Hill estimators together with several plot 
    variants.
    \cr
    
    The GEV modelling functions are:
    
    \tabular{ll}{
    \code{gevrlevelPlot} \tab k-block return level with confidence intervals. }
     
}


\usage{
gevrlevelPlot(object, kBlocks = 20,  ci = c(0.90, 0.95, 0.99), 
    plottype = c("plot", "add"), labels = TRUE,...)
}


\arguments{

    \item{add}{
        [gevrlevelPlot] - \cr
        whether the return level should be added graphically to a
        time series plot; if \code{FALSE} a graph of the profile
        likelihood curve showing the return level and its confidence
        interval is produced.
        }
    \item{ci}{
        [hillPlot] - \cr
        probability for asymptotic confidence band; for no
        confidence band set \code{ci} to zero.
        }
    \item{kBlocks}{
        [gevrlevelPlot] - \cr
        specifies the particular return level to be estimated; default 
        set arbitrarily to 20.
        }
    \item{labels}{
        [hillPlot] - \cr
        whether or not axes should be labelled.
        } 
    \item{object}{
        [summary][grlevelPlot] - \cr
        a fitted object of class \code{"gevFit"}.
        }
    \item{plottype}{
        [hillPlot] - \cr
        whether \code{alpha}, \code{xi} (1/alpha) or
        \code{quantile} (a quantile estimate) should be plotted.
        }
    \item{\dots}{
        arguments passed to the plot function.
        }    

}


\value{

    \code{gevSim}
    \cr
    returns a vector of data points from the simulated series.
    \cr

    \code{gevFit}
    \cr
    returns an object of class \code{gev} describing the fit.
    \cr
  
    \code{print.summary}
    \cr
    prints a report of the parameter fit.
    \cr
    
    \code{summary}
    \cr
    performs diagnostic analysis. The method provides two different 
    residual plots for assessing the fitted GEV model.  
    \cr
  
    \code{gevrlevelPlot}
    \cr
    returns a vector containing the lower 95\% bound of the confidence 
    interval, the estimated return level and the upper 95\% bound. 
    \cr
    
    \code{hillPlot}
    \cr
    displays a plot.
    \cr
   
    \code{shaparmPlot} 
    \cr
    returns a list with one or two entries, depending on the
    selection of the input variable \code{both.tails}. The two 
    entries \code{upper} and \code{lower} determine the position of 
    the tail. Each of the two variables is again a list with entries 
    \code{pickands}, \code{hill}, and \code{dehaan}. If one of the 
    three methods will be discarded the printout will display zeroes.
    
}


\details{
        
    \bold{Parameter Estimation:}
    \cr\cr
    \code{gevFit} and \code{gumbelFit} estimate the parameters either 
    by the probability weighted moment method, \code{method="pwm"} or 
    by maximum log likelihood estimation \code{method="mle"}. The 
    summary method produces diagnostic plots for fitted GEV or Gumbel 
    models.
    \cr
  
    \bold{Methods:}
    \cr\cr
    \code{print.gev}, \code{plot.gev} and \code{summary.gev} are
    print, plot, and summary methods for a fitted object of class 
    \code{gev}. Concerning the summary method, the data are 
    converted to unit exponentially distributed residuals under null 
    hypothesis that GEV fits. Two diagnostics for iid exponential data 
    are offered. The plot method provides two different residual plots 
    for assessing the fitted GEV model. Two diagnostics for 
    iid exponential data are offered. 
    \cr
  
    \bold{Return Level Plot:}
    \cr\cr
    \code{gevrlevelPlot} calculates and plots the k-block return level 
    and 95\% confidence interval based on a GEV model for block maxima, 
    where \code{k} is specified by the user. The k-block return level 
    is that level exceeded once every \code{k} blocks, on average. The 
    GEV likelihood is reparameterized in terms of the unknown return 
    level and profile likelihood arguments are used to construct a 
    confidence interval. 
    \cr
    
    \bold{Hill Plot:}
    \cr\cr
    The function \code{hillPlot} investigates the shape parameter and 
    plots the Hill estimate of the tail index of heavy-tailed data, or 
    of an associated quantile estimate. This plot is usually calculated 
    from the alpha perspective. For a generalized Pareto analysis of 
    heavy-tailed data using the \code{gpdFit} function, it helps to 
    plot the Hill estimates for \code{xi}. 
    \cr
  
    \bold{Shape Parameter Plot:}
    \cr\cr
    The function \code{shaparmPlot} investigates the shape parameter and 
    plots for the upper and lower tails the shape parameter as a function 
    of the taildepth. Three approaches are considered, the \emph{Pickands} 
    estimator, the \emph{Hill} estimator, and the
    \emph{Decker-Einmal-deHaan} estimator.
    
}
  

\note{
    
    \bold{GEV Parameter Estimation:}
    \cr\cr
    If method \code{"mle"} is selected the parameter fitting in \code{gevFit} 
    is passed to the internal function \code{gev.mle} or \code{gumbel.mle}
    depending on the value of \code{gumbel}, \code{FALSE} or \code{TRUE}.
    On the other hand, if method \code{"pwm"} is selected the parameter 
    fitting in \code{gevFit} is passed to the internal function 
    \code{gev.pwm} or \code{gumbel.pwm} again depending on the value of 
    \code{gumbel}, \code{FALSE} or \code{TRUE}.
        
}


\references{

Coles S. (2001);
    \emph{Introduction to Statistical Modelling of Extreme Values},
    Springer.
    
Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, 
    Springer.
    
}


\author{
  
    Alec Stephenson for R's \code{evd} and \code{evir} package, and \cr
    Diethelm Wuertz for this \R-port.
    
}

  
\examples{  
## Load Data:
   # BMW Stock Data - negative returns
   x = -as.timeSeries(data(bmwRet))
   colnames(x)<-"BMW"
   head(x)
   
## gevFit -
   # Fit GEV to monthly Block Maxima:
   fit = gevFit(x, block = "month")  
}


\keyword{models}

