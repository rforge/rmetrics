\name{GevModelling}

\alias{GevModelling}

\alias{fGEVFIT}
\alias{fGEVFIT-class}

\alias{show,fGEVFIT-method}

\alias{gevSim}
\alias{gumbelSim}

\alias{gevFit}
\alias{gumbelFit}

\alias{plot.fGEVFIT}
\alias{summary.fGEVFIT}


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
    \code{gevSim} \tab generates data from the GEV distribution, \cr
    \code{gumbelSim} \tab generates data from the Gumbel distribution, \cr
    \code{gevFit} \tab fits data to the GEV distribution, \cr 
    \code{gumbelFit} \tab fits data to the Gumbel distribution, \cr 
    \code{print} \tab print method for a fitted GEV object, \cr
    \code{plot} \tab plot method for a fitted GEV object, \cr 
    \code{summary} \tab summary method for a fitted GEV object, \cr
    \code{gevrlevelPlot} \tab k-block return level with confidence intervals. }
     
}


\usage{
gevSim(model = list(xi = -0.25, mu = 0, beta = 1), n = 1000, seed = NULL)
gumbelSim(model = list(mu = 0, beta = 1), n = 1000, seed = NULL)

gevFit(x, block = 1, type = c("mle", "pwm"), title = NULL, description = NULL, \dots)
gumbelFit(x, block = 1, type = c("mle", "pwm"), title = NULL, description = NULL, \dots)

\S4method{show}{fGEVFIT}(object)
\method{plot}{fGEVFIT}(x, which = "ask", \dots)
\method{summary}{fGEVFIT}(object, doplot = TRUE, which = "all", \dots)
}


\arguments{

    \item{block}{
        block size.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{doplot}{
        a logical. Should the results be plotted?
        \cr
        [shaparmPlot] - \cr
        a vector of logicals of the same lengths as tails
        defining for wich tail depths plots should be created,
        by default plots will be generated for a tail depth of 5
        percent. By default \code{c(FALSE, FALSE, FALSE, FALSE, 
        TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)}.
        }
    \item{model}{
        [gevSim][gumbelSim] - \cr
        a list with components \code{shape}, \code{location} and 
        \code{scale} giving the parameters of the GEV distribution.
        By default the shape parameter has the value -0.25, the
        location is zero and the scale is one.
        To fit random deviates from a Gumbel distribution set 
        \code{shape=0}.
        }
    \item{n}{
        [gevSim][gumbelSim] - \cr
        number of generated data points, an integer value.
        \cr
        [rgev] - \cr        
        the number of observations.
        } 
    \item{object}{
        [summary][grlevelPlot] - \cr
        a fitted object of class \code{"gevFit"}.
        }
    \item{seed}{
        [gevSim] - \cr
        an integer value to set the seed for the random number generator.
        }
    \item{title}{
        [gevFit] - \cr
        a character string which allows for a project title.
        }
    \item{type}{
        a character string denoting the type of parameter estimation,
        either by maximum likelihood estimation \code{"mle"}, the
        default value, or by the probability weighted moment menthod
        \code{"pwm"}.
        }
    \item{which}{
        [plot][summary] - \cr
        a vector of logicals, one for each plot, denoting which plot 
        should be displayed. Alkternatively if \code{which="ask"} the
        user will be interactively asked which of the plots should be
        desplayed. By default \code{which="all"}.
        }
    \item{x}{
        [dgev][devd] - \cr
        a numeric vector of quantiles.
        \cr
        [gevFit] - \cr
        data vector. In the case of \code{method="mle"} the interpretation 
        depends on the value of block: if no block size is specified then 
        data are interpreted as block maxima; if block size is set, then data
        are interpreted as raw data and block maxima are calculated. 
        \cr
        [hillPlot][shaparmPlot] - \cr
        the data from which to calculate the shape parameter, a
        numeric vector.
        \cr
        [print][plot] - \cr
        a fitted object of class \code{"gevFit"}.
        }
    \item{xi, mu, beta}{
        [*gev] - \cr
        \code{xi} is the shape parameter, \code{mu} the location parameter,
        and \code{sigma} is the scale parameter. The default values are 
        \code{xi=1}, \code{mu=0}, and \code{beta=1}. Note, if \code{xi=0} 
        the distribution is of type Gumbel.
        }
    \item{\dots}{
        [gevFit] - \cr
        control parameters optionally passed to the 
        optimization function. Parameters for the optimization
        function are passed to components of the \code{control} argument of
        \code{optim}. 
        \cr
        [hillPlot] - \cr
        other graphics parameters.
        \cr
        [plot][summary] - \cr
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
## gevSim -
   # Simulate GEV Data, use default length n=1000
   x = gevSim(model = list(xi = 0.25, mu = 0 , beta = 1))
   
## gevFit -
   # Fit GEV Data by Probability Weighted Moments:
   fit = gevFit(x, type = "pwm") 
   print(fit)
   
## summary -
   # Summarize Results:
   par(mfcol = c(2, 2))
   summary(fit)
}


\keyword{models}

