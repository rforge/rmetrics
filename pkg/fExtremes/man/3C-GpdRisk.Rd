\name{gpdRisk}

\alias{gpdRisk}


\alias{gpdQPlot}
\alias{gpdQuantPlot}
\alias{gpdSfallPlot}
\alias{gpdShapePlot}
\alias{gpdTailPlot}

\alias{gpdRiskMeasures}

\alias{tailPlot}
\alias{tailSlider}
\alias{tailRisk}


\title{GPD Distributions for Extreme Value Theory}


\description{

    A collection and description to functions to compute
    tail risk under the GPD approach.
    \cr

    The GPD modelling functions are:
     
    \tabular{ll}{
    \code{gpdQPlot} \tab estimation of high quantiles, \cr
    \code{gpdQuantPlot} \tab variation of high quantiles with threshold, \cr
    \code{gpdRiskMeasures} \tab prescribed quantiles and expected shortfalls, \cr
    \code{gpdSfallPlot} \tab expected shortfall with confidence intervals, \cr
    \code{gpdShapePlot} \tab variation of shape with threshold, \cr
    \code{gpdTailPlot} \tab plot of the tail, \cr
    \code{tailPlot} \tab , \cr 
    \code{tailSlider} \tab , \cr
    \code{tailRisk} \tab . }
     
}


\usage{

gpdQPlot(x, p = 0.99, ci = 0.95, type = c("likelihood", "wald"), 
    like.num = 50)
gpdQuantPlot(x, p = 0.99, ci = 0.95, models = 30, start = 15, end = 500,  
    doplot = TRUE, plottype = c("normal", "reverse"), labels = TRUE, \dots)
gpdSfallPlot(x, p = 0.99, ci = 0.95, like.num = 50)
gpdShapePlot(x, ci = 0.95, models = 30, start = 15, end = 500, 
    doplot = TRUE, plottype = c("normal", "reverse"), labels = TRUE, \dots)
gpdTailPlot(object, plottype = c("xy", "x", "y", ""), extend = 1.5, 
    labels = TRUE, \dots)

gpdRiskMeasures(object, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))

tailPlot(object, plottype = c("xy", "x", "y", ""), doplot = TRUE, 
    extend = 1.5, labels = TRUE, ...)
tailSlider(x)
tailRisk(object, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999), ...)
}


\arguments{
    
    \item{ci}{
        the probability for asymptotic confidence band; for no 
        confidence band set to zero.
        }
    \item{description}{
        a character string which allows for a brief description.
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
        [gpdSim] - \cr
        a list with components \code{shape}, \code{location} and 
        \code{scale} giving the parameters of the GPD distribution.
        By default the shape parameter has the value 0.25, the
        location is zero and the scale is one.}
    \item{models}{
        the number of consecutive gpd models to be fitted.
        }
    \item{n}{
        [rgpd][gpdSim\ - \cr
        the number of observations to be generated.
        } 
    \item{object}{
        [summary] - \cr
        a fitted object of class \code{"gpdFit"}.
        }
    \item{p}{
        a vector of probability levels, the desired probability for the 
        quantile estimate (e.g. 0.99 for the 99th percentile).
        } 
    \item{reverse}{
        should plot be by increasing threshold (\code{TRUE}) or number 
        of extremes (\code{FALSE}).
        }
    \item{seed}{
        [gpdSim] - \cr
        an integer value to set the seed for the random number generator.
        }
    \item{prob}{
        a numeric value.
        }
    \item{plottype}{
        a character string.
        }
    \item{start, end}{
        the lowest and maximum number of exceedances to be considered.
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
\dontrun{
## SOURCE("fExtremes.053A-GpdModelling")

## *gpd  -
   par(mfrow = c(2, 2))
   r = rgpd(n = 1000, xi = 1/4)
   plot(r, type = "l", main = "GPD Series")
   
## Plot empirical density and compare with true density:
## Omit values greater than 500 from plot
   hist(r, n = 50, probability = TRUE, xlab = "r", 
     xlim = c(-5, 5), ylim = c(0, 1.1), main = "Density")
   x = seq(-5, 5, by = 0.01)
   lines(x, dgpd(x, xi = 1/4), col = "steelblue3")
   
## Plot df and compare with true df:
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
     cex = 0.5, ylab = "p", xlab = "q", main = "Probability")
   q = seq(-5, 5, by = 0.1)
   lines(q, pgpd(q, xi = 1/4), col = "steelblue3")
   
## Compute quantiles, a test:
   qgpd(pgpd(seq(-1, 5, 0.25), xi = 1/4 ), xi = 1/4) 
   
## Load Data:
   data(danish)
   
## gpdSim - 
   # Simulate GPD Data:
   x = gpdSim(model = list(shape = 0.25, location = 0, scale = 1), n = 1000)
   
## gpdFit -   
   fit = gpdFit(x, nextremes = length(x), type = "pwm") 
   print(fit)
   par(mfcol = c(4, 2), cex = 0.7)
   summary(fit)  
   
## gpdFit -
   fit = gpdFit(x, nextremes = length(x), type = "mle") 
   print(fit)
   summary(fit) 

## gpdFit - 
   fit = gpdFit(danish, 10, type = "mle") 
   print(fit)
   par(mfrow = c(2, 2), cex = 0.7)
   summary(fit)  
     
## gpdqPlot - 
   fit = gpdFit(danish, threshold = 10, type = "mle")
   par(mfrow = c(1, 1))
   tail = gpdtailPlot(fit)
   gpdqPlot(tail, 0.995)
   title(main = "Danish Data: 99.5th Percentile") 
    
## gpdquantPlot -   
   par(mfrow = c(1, 1))
   gpdquantPlot(danish, p = 0.999)
   title(sub = "Danish Fire: GPD High Quantile") 

## gpdsfallPlot - 
   fit = gpdFit(danish, nextremes = floor(length(danish)/10), type = "mle")
   par(mfrow = c(1, 1))
   tp = gpdtailPlot(fit)
   gpdsfallPlot(tp, 0.999)   
   title(main = "Danish Fire: Expected Shortfall")   
   
## gpdriskmeasures -
   # Give estimates of 0.999 and 0.9999 quantiles - Danish Fire Date:   
   fit = gpdFit(danish, threshold = 10, type = "mle") 
   par(mfrow = c(1, 1))
   gpdriskmeasures(fit, c(0.99, 0.995, 0.999, 0.9995, 0.9999))   
  
## gpdshapePlot - 
   set.seed(4711)
   par(mfrow = c(1, 1))
   gpdshapePlot(gpdSim(n = 1000))
   title(sub = "Simulated GPD", cex.sub = 0.7)   

## gpdshapePlot - 
   par(mfrow = c(1, 1))
   gpdshapePlot(danish)
   title(sub = "Danish Fire", cex.sub = 0.7)

## gpdtailPlot - 
   fit = gpdFit(danish, threshold = 10, type = "mle")
   par(mfrow = c(1, 1))
   gpdtailPlot(fit, main = "Danish Fire: GPD Tail Estimate", col = "steelblue4")  
}
}


\keyword{distribution}

