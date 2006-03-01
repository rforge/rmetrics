\name{GpdFit}

\alias{GpdFit}

\alias{gpdSim}
\alias{gpdFit}

\alias{print.gpdFit}
\alias{plot.gpdFit}
\alias{summary.gpdFit}

\alias{gpdqPlot}
\alias{gpdquantPlot}
\alias{gpdriskmeasures}
\alias{gpdsfallPlot}
\alias{gpdshapePlot}
\alias{gpdtailPlot}


\title{Modelling the Generalized Pareto Distribution}


\description{
  
    A collection and description of functions to model 
    the Generalized Pareto Distribution, GPD, based on 
    \R's 'evir' package. Two approaches for parameter 
    estimation are provided: Maximum likelihood estimation 
    and the probability weighted moment method.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{gpdSim} \tab generates data from the GPD, \cr
    \code{gpdFit} \tab fits empirical or simulated data to the distribution, \cr 
    \code{print} \tab print method for a fitted GPD object of class ..., \cr
    \code{plot} \tab plot method for a fitted GPD object, \cr 
    \code{summary} \tab summary method for a fitted GPD object, \cr
    \code{gpdqPlot} \tab estimation of high quantiles, \cr
    \code{gpdquantPlot} \tab variation of high quantiles with threshold, \cr
    \code{gpdriskmeasures} \tab prescribed quantiles and expected shortfalls, \cr
    \code{gpdsfallPlot} \tab expected shortfall with confidence intervals, \cr
    \code{gpdshapePlot} \tab variation of shape with threshold, \cr
    \code{gpdtailPlot} \tab plot of the tail. }
    
}


\usage{
gpdSim(model = list(shape = 0.25, location = 0, scale = 1), n = 1000)
gpdFit(x, threshold = NA, nextremes = NA, type = c("mle", "pwm"),
    information = c("observed", "expected"), \dots)

\method{print}{gpdFit}(x, \dots)
\method{plot}{gpdFit}(x, which = "all", \dots)
\method{summary}{gpdFit}(object, doplot = TRUE, which = "all", \dots)

gpdqPlot(x, pp = 0.99, ci.type = c("likelihood", "wald"), ci.p = 0.95, 
    like.num = 50)
gpdquantPlot(data, p = 0.99, models = 30, start = 15, end = 500, 
    reverse = TRUE, ci = 0.95, autoscale = TRUE, labels = TRUE, \dots)
gpdriskmeasures(x, plevels = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
gpdsfallPlot(x, pp = 0.99, ci.p = 0.95, like.num = 50)
gpdshapePlot(data, models = 30, start = 15, end = 500, reverse = TRUE,
    ci = 0.95, autoscale = TRUE, labels = TRUE, \dots) 
gpdtailPlot(fit, optlog = NA, extend = 1.5, labels = TRUE, \dots)
}


\arguments{

    \item{autoscale}{
        whether or not plot should be automatically scaled;
        if not, xlim and ylim graphical parameters may be entered.
        }     
    \item{ci}{
        the probability for asymptotic confidence band; for no 
        confidence band set to zero.
        }
     \item{ci.p}{
        the probability for confidence interval (must be less 
        than 0.999).
        }
    \item{ci.type}{
        the method for calculating a confidence interval: 
        \code{"likelihood"} or \code{"wald"}.
        }
    \item{data}{
        a numeric vector of data.
        }
    \item{doplot}{
        a logical. Should the results be plotted?
        }
    \item{extend}{
        optional argument for plots 1 and 2 expressing how far x-axis 
        should extend as a multiple of the largest data value. This 
        argument must take values greater than 1 and is useful for
        showing estimated quantiles beyond data.
        }
    \item{fit}{
        [print][plot][summary] - \cr
        print method, a fitted object of class \code{"gpd"}.
        }
    \item{information}{
        whether standard errors should be calculated with
        \code{"observed"} or \code{"expected"} information. This only applies
        to the maximum likelihood method; for the probability-weighted moments
        method \code{"expected"} information is used if possible.
        }
    \item{labels}{
        optional argument for plots 1 and 2  specifying whether or not 
        axes should be labelled.
        } 
    \item{like.num}{
        the number of times to evaluate profile likelihood.
        } 
    \item{model}{
        [gpdsim] - \cr
        a list with components \code{shape}, \code{location} and 
        \code{scale} giving the parameters of the GPD distribution.
        By default the shape parameter has the value 0.25, the
        location is zero and the scale is one.}
    \item{models}{
        the number of consecutive gpd models to be fitted.
        }
    \item{n}{
        [gpdsim] - \cr
        lnumber of generated data points, an integer value.
        }
    \item{nextremes}{
        [gpdFit] - \cr
        the number of upper extremes to be used (either this or 
        \code{threshold} must be given but not both).
        }
    \item{object}{
        [summary] - \cr
        a fitted object of class \code{"gpdFit"}.
        }
    \item{optlog}{
        optional argument for plots 1 and 2 giving a particular choice 
        of logarithmic axes: \code{"x"} x-axis only; \code{"y"} y-axis 
        only; \code{"xy"} both axes; \code{""} neither axis.
        }
    \item{plevels, p, pp}{
        a vector of probability levels, the desired probability for the 
        quantile estimate (e.g. 0.99 for the 99th percentile).
        } 
    \item{reverse}{
        should plot be by increasing threshold (\code{TRUE}) or number 
        of extremes (\code{FALSE}).
        }
    \item{start, end}{
        the lowest and maximum number of exceedances to be considered.
        }
    \item{threshold}{
        a threshold value (either this or \code{nextremes} must be given 
        but not both).
        }
    \item{type}{
        a character string selecting the desired estimation mehtod, either
        \code{"mle"} for the maximum likelihood mehtod or \code{"pwm"} for 
        the probability weighted moment method. By default, the first will 
        be selected. Note, the function \code{gpd} uses \code{"ml"}.    
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
        [gpdFit] - \cr
        the data vector. Note, there are two different names
        for the first argument \code{x} and \code{data} depending 
        which function name is used, either \code{gpdFit} or the 
        EVIS synonyme \code{gpd}.
        \cr
        [print][plot] - \cr
        a fitted object of class \code{"gpdFit"}.
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
  
    \code{gpdquantPlot}
    \cr
    returns invisible a table of results.
  
    \code{gpdshapePlot}
    \cr
    returns invisible a table of results.
  
    \code{gpdtailPlot}
    \cr
    returns invisible a list object containing 
    details of the plot is returned invisibly. This object should be 
    used as the first argument of \code{gpdqPlot} or \code{gpdsfallPlot} 
    to add quantile estimates or expected shortfall estimates to the 
    plot. 
    
}


\details{

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


\references{

Hosking J.R.M., Wallis J.R., (1987);
    \emph{Parameter and quantile estimation for the generalized
        Pareto distribution},   
    Technometrics 29, 339--349.
    
}


\author{
  
    This function is based on Alec Stephenson's R-package \code{evir} 
    ported from the \code{EVIS} library, \emph{Extreme Values in S},
    written by Alexander McNeil. The \code{fExtremes} port and the 
    change and addition of some functions were done by Diethelm Wuertz.
    
}

  
\examples{
## SOURCE("fExtremes.053B-GpdFit")

## Load Data:
   data(danish)
   
## gpdSim - 
   # Simulate GPD Data:
   xmpExtremes("\nStart: Simulate a GPD Distributed Sample > ")
   x = gpdSim(model = list(shape = 0.25, location = 0, scale = 1), n = 1000)
   
## gpdFit -   
   xmpExtremes("\nNext: Fit Simulated Data to GPD using PWM > ")
   fit = gpdFit(x, nextremes = length(x), type = "pwm") 
   print(fit)
   par(mfcol = c(4, 2), cex = 0.7)
   summary(fit)  
   
## gpdFit -
   xmpExtremes("\nNext: Fit Simulated Data to GPD using MLE > ")
   fit = gpdFit(x, nextremes = length(x), type = "mle") 
   print(fit)
   summary(fit) 

## gpdFit - 
   xmpExtremes("\nNext: Fit Danish Fire Data to Excess Losses over 10 > ")
   fit = gpdFit(danish, 10, type = "mle") 
   print(fit)
   par(mfrow = c(2, 2), cex = 0.7)
   summary(fit)  
     
## gpdqPlot - 
   xmpExtremes("\nNext: 99.5th Percentiles for Danish Fire Data > ")
   fit = gpdFit(danish, threshold = 10, type = "mle")
   par(mfrow = c(1, 1))
   tail = gpdtailPlot(fit)
   gpdqPlot(tail, 0.995)
   title(main = "Danish Data: 99.5th Percentile") 
    
## gpdquantPlot -   
   xmpExtremes("\nNext: 99.9th Percentiles for Danish Fire Data > ")
   par(mfrow = c(1, 1))
   gpdquantPlot(danish, p = 0.999)
   title(sub = "Danish Fire: GPD High Quantile") 

## gpdsfallPlot - 
   xmpExtremes("\nNext: Expected Shortfall for Danish Fire Data > ")
   fit = gpdFit(danish, nextremes = floor(length(danish)/10), type = "mle")
   par(mfrow = c(1, 1))
   tp = gpdtailPlot(fit)
   gpdsfallPlot(tp, 0.999)   
   title(main = "Danish Fire: Expected Shortfall")   
   
## gpdriskmeasures -
   xmpExtremes("\nNext: Quantiles and Expected Shortfalls > ")
   # Give estimates of 0.999 and 0.9999 quantiles - Danish Fire Date:   
   fit = gpdFit(danish, threshold = 10, type = "mle") 
   par(mfrow = c(1, 1))
   gpdriskmeasures(fit, c(0.99, 0.995, 0.999, 0.9995, 0.9999))   
  
## gpdshapePlot - 
   xmpExtremes("\nNext: Shape Plot of Heavy-Tailed Simulated Data > ")
   set.seed(4711)
   par(mfrow = c(1, 1))
   gpdshapePlot(gpdSim(n = 1000))
   title(sub = "Simulated GPD", cex.sub = 0.7)   

## gpdshapePlot - 
   xmpExtremes("\nNext: Shape Plot of Heavy-Tailed Danish Fire Data > ")
   par(mfrow = c(1, 1))
   gpdshapePlot(danish)
   title(sub = "Danish Fire", cex.sub = 0.7)

## gpdtailPlot - 
   xmpExtremes("\nNext: Plot Tail Estimate of Danish Fire Data >")
   fit = gpdFit(danish, threshold = 10, type = "mle")
   par(mfrow = c(1, 1))
   gpdtailPlot(fit, main = "Danish Fire: GPD Tail Estimate", col = "steelblue4")  
}


\keyword{models}

