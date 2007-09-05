\name{GpdModelling}

\alias{GpdModelling}

\alias{fGPDFIT}
\alias{fGPDFIT-class}

\alias{show.fGPDFIT}
\alias{show,fGPDFIT-method}

\alias{gpdSim}
\alias{gpdFit}

\alias{plot.fGPDFIT}
\alias{summary.fGPDFIT}


\title{GPD Distributions for Extreme Value Theory}


\description{

    A collection and description to functions to compute
    the generalized Pareto distribution and to
    estimate its parameters. The functions compute 
    density, distribution function, quantile function 
    and generate random deviates for the GPD. Two 
    approaches for parameter estimation are provided: 
    Maximum likelihood estimation and the probability 
    weighted moment method.
    \cr

    The GPD modelling functions are:
     
    \tabular{ll}{
    \code{gpdSim} \tab generates data from the GPD, \cr
    \code{gpdFit} \tab fits empirical or simulated data to the distribution, \cr 
    \code{print} \tab print method for a fitted GPD object of class ..., \cr
    \code{plot} \tab plot method for a fitted GPD object, \cr 
    \code{summary} \tab summary method for a fitted GPD object. }
     
}


\usage{
gpdSim(model = list(xi = 0.25, mu = 0, beta = 1), n = 1000,
    seed = NULL)
gpdFit(x, u = quantile(x, 0.95), type = c("mle", "pwm"), information = 
    c("observed", "expected"), title = NULL, description = NULL, \dots)

show.fGPDFIT(object)
\method{plot}{fGPDFIT}(x, which = "ask", \dots)
\method{summary}{fGPDFIT}(object, doplot = TRUE, which = "all", \dots)
}


\arguments{
    
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{doplot}{
        a logical. Should the results be plotted?
        }
    \item{information}{
        whether standard errors should be calculated with
        \code{"observed"} or \code{"expected"} information. This only applies
        to the maximum likelihood method; for the probability-weighted moments
        method \code{"expected"} information is used if possible.
        }
    \item{model}{
        [gpdSim] - \cr
        a list with components \code{shape}, \code{location} and 
        \code{scale} giving the parameters of the GPD distribution.
        By default the shape parameter has the value 0.25, the
        location is zero and the scale is one.}
    \item{n}{
        [rgpd][gpdSim\ - \cr
        the number of observations to be generated.
        } 
    \item{object}{
        [summary] - \cr
        a fitted object of class \code{"gpdFit"}.
        }
    \item{seed}{
        [gpdSim] - \cr
        an integer value to set the seed for the random number generator.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{type}{
        a character string selecting the desired estimation mehtod, either
        \code{"mle"} for the maximum likelihood mehtod or \code{"pwm"} for 
        the probability weighted moment method. By default, the first will 
        be selected. Note, the function \code{gpd} uses \code{"ml"}.    
        }
    \item{u}{
        the threshold value.    
        }
    \item{which}{
        if \code{which} is set to \code{"ask"} the function will 
        interactively ask which plot should be displayed. By default
        this value is set to \code{FALSE} and then those plots will
        be displayed for which the elements in the logical vector
        \code{which} ar set to \code{TRUE}; by default all four
        elements are set to \code{"all"}.
        }
    \item{x}{
        [dgpd] - \cr
        a numeric vector of quantiles.
        \cr
        [gpdFit] - \cr
        the data vector. Note, there are two different names
        for the first argument \code{x} and \code{data} depending 
        which function name is used, either \code{gpdFit} or the 
        EVIS synonyme \code{gpd}.
        \cr
        [print][plot] - \cr
        a fitted object of class \code{"gpdFit"}.
        }
    \item{xi, mu, beta}{
        \code{xi} is the shape parameter, 
        \code{mu} the location parameter,
        and \code{beta} is the scale parameter.
        }
    \item{\dots}{
        control parameters and plot parameters optionally passed to the 
        optimization and/or plot function. Parameters for the optimization
        function are passed to components of the \code{control} argument of
        \code{optim}.  
        }

}


\value{
    
    \code{gpdSim}
    \cr
    returns a vector of datapoints from the simulated 
    series.
    
    \code{gpdFit} 
    \cr
    returns an object of class \code{"gpd"} describing the 
    fit including parameter estimates and standard errors. 
  
    \code{gpdQuantPlot}
    \cr
    returns invisible a table of results.
  
    \code{gpdShapePlot}
    \cr
    returns invisible a table of results.
  
    \code{gpdTailPlot}
    \cr
    returns invisible a list object containing 
    details of the plot is returned invisibly. This object should be 
    used as the first argument of \code{gpdqPlot} or \code{gpdsfallPlot} 
    to add quantile estimates or expected shortfall estimates to the 
    plot. 
    
}


\details{
    
    \bold{Generalized Pareto Distribution:}
    \cr\cr
    Compute density, distribution function, quantile function and 
    generates random variates for the Generalized Pareto Distribution.
    
    \bold{Simulation:}
    \cr\cr
    \code{gpdSim} simulates data from a Generalized Pareto 
    distribution.
    \cr
    
    \bold{Parameter Estimation:}
    \cr\cr
    \code{gpdFit} fits the model parameters either by the probability 
    weighted moment method or the maxim log likelihood method. 
    The function returns an object of class \code{"gpd"} 
    representing the fit of a generalized Pareto model to excesses over 
    a high threshold. The fitting functions use the probability weighted 
    moment method, if method \code{method="pwm"} was selected, and the 
    the general purpose optimization function \code{optim} when the 
    maximum likelihood estimation, \code{method="mle"} or \code{method="ml"} 
    is chosen.
    \cr
  
    \bold{Methods:}
    \cr\cr
    \code{print.gpd}, \code{plot.gpd} and \code{summary.gpd} are print, 
    plot, and summary methods for a fitted object of class \code{gpdFit}. 
    The plot method provides four different plots for assessing fitted 
    GPD model. 
    \cr
    
    \bold{gpd* Functions:}
    \cr\cr
    \code{gpdqPlot} calculates quantile estimates and confidence intervals 
    for high quantiles above the threshold in a GPD analysis, and adds a 
    graphical representation to an existing plot. The GPD approximation in 
    the tail is used to estimate quantile. The \code{"wald"} method uses 
    the observed Fisher information matrix to calculate confidence interval. 
    The \code{"likelihood"} method reparametrizes the likelihood in terms 
    of the unknown quantile and uses profile likelihood arguments to 
    construct a confidence interval. 
    \cr
    
    \code{gpdquantPlot} creates a plot showing how the estimate of a 
    high quantile in the tail of a dataset based on the GPD approximation 
    varies with threshold or number of extremes. For every model 
    \code{gpdFit} is called. Evaluation may be slow. Confidence intervals 
    by the Wald method may be fastest.
    \cr
    
    \code{gpdriskmeasures} makes a rapid calculation of point estimates 
    of prescribed quantiles and expected shortfalls using the output of the
    function \code{gpdFit}. This function simply calculates point estimates 
    and (at present) makes no attempt to calculate confidence intervals for 
    the risk measures. If confidence levels are required use \code{gpdqPlot} 
    and \code{gpdsfallPlot} which interact with graphs of the tail of a loss
    distribution and are much slower.  
    \cr
    
    \code{gpdsfallPlot} calculates expected shortfall estimates, in other
    words tail conditional expectation and confidence intervals for high  
    quantiles above the threshold in a GPD analysis. A graphical 
    representation to an existing plot is added. Expected shortfall is 
    the expected size of the loss, given that a particular quantile of the 
    loss distribution is exceeded. The GPD approximation in the tail is used 
    to estimate expected shortfall. The likelihood is reparametrised  in 
    terms of the unknown expected shortfall and profile likelihood arguments 
    are used to construct a confidence interval. 
    \cr
    
    \code{gpdshapePlot} creates a plot showing how the estimate of shape 
    varies with threshold or number of extremes. For every model 
    \code{gpdFit} is called. Evaluation may be slow.  
    \cr
    
    \code{gpdtailPlot} produces a plot of the tail of the underlying 
    distribution of the data.
        
}


\author{

    Alec Stephenson for the functions from R's \code{evd} package, \cr
    Alec Stephenson for the functions from R's \code{evir} package, \cr
    Alexander McNeil for the EVIS functions underlying the \code{evir} package, \cr
    Diethelm Wuertz for this \R-port.
    
}


\references{

Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, Springer. 
    
Hosking J.R.M., Wallis J.R., (1987);
    \emph{Parameter and quantile estimation for the generalized
        Pareto distribution},   
    Technometrics 29, 339--349.
     
}


\examples{
## gpdSim  -
   x = gpdSim(model = list(xi = 0.25, mu = 0, beta = 1), n = 1000)
  
## gpdFit - 
   par(mfrow = c(2, 2), cex = 0.7)  
   fit = gpdFit(x, u = min(x), type = "pwm") 
   print(fit)
   summary(fit)   
}


\keyword{distribution}

