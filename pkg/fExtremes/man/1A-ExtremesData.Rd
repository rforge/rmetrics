\name{ExtremesData}

\alias{ExtremesData}

\alias{emdPlot}
\alias{qqPlot}
%\alias{qqbayesPlot}
\alias{qPlot}
\alias{mePlot}
\alias{mrlPlot}
\alias{mxfPlot}
\alias{msratioPlot}
\alias{recordsPlot}
\alias{ssrecordsPlot}
\alias{sllnPlot}
\alias{lilPlot}
\alias{xacfPlot}


\title{Explorative Data Analysis}


\description{

    A collection and description of functions for 
    explorative data analysis. The tools include 
    plot functions for emprical distributions, quantile 
    plots, graphs exploring the properties of exceedences 
    over a threshold, plots for mean/sum ratio and for the development 
    of records.
    \cr
    
    The plot functions are:
    
    \tabular{ll}{
    \code{emdPlot} \tab Plot of empirical distribution function, \cr
    \code{fBasics::qqnormPlot} \tab Normal quantile-quantile plot, \cr
    \code{qPlot} \tab Exponential/Pareto quantile plot, \cr
    \code{mePlot} \tab Plot of mean excesses over a threshold, \cr
    \code{mrlPlot} \tab another variant, mean residual life plot, \cr
    \code{mxfPlot} \tab another variant, with confidence intervals, \cr
    \code{msratioPlot} \tab Plot of the ratio of maximum and sum, \cr   
    \code{recordsPlot} \tab Record development compared with iid data, \cr
    \code{ssrecordsPlot} \tab another variant, investigates subsamples, \cr
    \code{sllnPlot} \tab verifies Kolmogorov's strong law of large numbers, \cr
    \code{lilPlot} \tab verifies Hartman-Wintner's law of the iterated logarithm, \cr
    \code{xacfPlot} \tab ACF of exceedences over a threshold. }  
    
}


\usage{
emdPlot(x, doplot = TRUE, plottype = c("xy", "x", "y", ""), labels = TRUE, \dots)

qPlot(x, xi = 0, trim = NULL, threshold = NULL, doplot = TRUE, labels = TRUE, \dots)

mePlot(x, doplot = TRUE, labels = TRUE, \dots)
mrlPlot(x, ci = 0.95, umin = mean(x), umax = max(x), nint = 100, doplot = TRUE, 
     plottype = c("autoscale", ""), labels = TRUE, \dots)  
mxfPlot(x, u = quantile(x, 0.05), doplot = TRUE, labels = TRUE, \dots)  
   
msratioPlot(x, p = 1:4, doplot = TRUE, labels = TRUE, \dots) 
   
recordsPlot(x, ci = 0.95, doplot = TRUE, labels = TRUE, \dots)
ssrecordsPlot(x, subsamples = 10, doplot = TRUE, plottype = c("lin", "log"),
    labels = TRUE, \dots)
    
sllnPlot(x, doplot = TRUE, labels = TRUE, \dots)
lilPlot(x, doplot = TRUE, labels = TRUE, \dots)

xacfPlot(x, u = quantile(x, 0.95), lag.max = 15, doplot = TRUE, labels = TRUE, \dots)

}


\arguments{

    \item{ci}{
        [recordsPlot] - \cr
        a confidence level. By default 0.95, i.e. 95\%.
        }
    \item{doplot}{
        a logical value. Should the results be plotted? By 
        default \code{TRUE}.
        }
    \item{labels}{
        a logical value. Whether or not x- and y-axes should be automatically 
        labelled and a default main title should be added to the plot.
        By default \code{TRUE}.
        }
    \item{lag.max}{
        [xacfPlot] - \cr
        maximum number of lags at which to calculate the autocorrelation 
        functions. The default value is 15.
        }
    \item{nint}{
        [mrlPlot] - \cr
        the number of intervals, see \code{umin} and \code{umax}. The 
        default value is 100.
        }
    \item{p}{
        [msratioPlot] - \cr
        the power exponents, a numeric vector. By default a sequence from  
        1 to 4 in unit integer steps.
        }
    \item{plottype}{
        [emdPlot] - \cr
        which axes should be on a log scale: \code{"x"} x-axis only; 
        \code{"y"} y-axis only; \code{"xy"} both axes; \code{""} 
        neither axis.
        \cr
        [msratioPlot] - \cr
        a logical, if set to \code{"autoscale"}, then the scale of the 
        plots are automatically determined, any other string allows user
        specified scale information through the \code{\dots} argument.
        \cr
        [ssrecordsPlot] - \cr
        one from two options can be select either \code{"lin"}
        or \code{"log"}. The default creates a linear plot.
        } 
    \item{subsamples}{
        [ssrecordsPlot] - \cr
        the number of subsamples, by default 10, an integer value.
        }
    \item{threshold, trim}{
        [qPlot][xacfPlot] - \cr
        a numeric value at which data are to be left-truncated, value 
        at which data are to be right-truncated or the thresold value, 
        by default 95\%.
        }
    \item{u}{
        a numeric value at which level the data are to be truncated. By 
        default the threshold value which belongs to the 95\% quantile,
        \code{u=quantile(x,0.95)}.       
        }
    \item{umin, umax}{
        [mrlPlot] - \cr
        range of threshold values. If \code{umin} and/or \code{umax} are 
        not available, then by default they are set to the following 
        values: \code{umin=mean(x)} and \code{umax=max(x)}.
        }
    \item{which}{
        plot selection, which graph should be displayed? If \code{"which"}
        is a character string named "ask" the user is interactively asked 
        which to plot, if a logical vector of length \code{N}, those plots 
        which are set \code{TRUE} are displayed, if a character string
        named \code{"all"} all plots are displayed.
        }
    \item{x, y}{
        numeric data vectors or in the case of x an object to be plotted.  
        }
    \item{xi}{
        the shape parameter of the generalized Pareto distribution.
        }
    \item{\dots}{
        additional arguments passed to the FUN or plot function.
        }
        
}


\details{
  
    \bold{Empirical Distribution Function:}
    \cr\cr
    The function \code{emdPlot} is a simple explanatory function. A 
    straight line on the double log scale indicates Pareto tail behaviour.
    \cr
    
    
    \bold{Quantile--Quantile Plot:}
    \cr\cr      
    The function \code{qqPlot} produces a normal QQ-plot. Note, that 
    \code{qqPlot} is not a synonym function call to the \R-base function 
    \code{qqplot} which produces a quantile-quantile plot of two datasets.      
    To help with assessing the relevance of sampling variability on just
    "how close" to the normal the data appears, \code{qqbayesPlot} (part
    of \code{fBasics}) adds 
    approximate posterior 95% intervals for the uncertain quantile 
    function at each point.
    \code{qPlot} creates a QQ-plot for threshold data. If \code{xi} is 
    zero the reference distribution is the exponential; if \code{xi} is 
    non-zero the reference distribution is the generalized Pareto with 
    that value of \code{xi}. In the case of the exponential, the plot is 
    interpreted as follows: Concave departures from a straight line are a 
    sign of heavy-tailed behaviour, convex departures show thin-tailed 
    behaviour. 
    \cr
    
        
    \bold{Mean Excess Function Plot:}
    \cr\cr
    Three variants to plot the mean excess function are available: 
    A sample mean excess plot over increasing thresholds, and two mean 
    excess function plots with confidence intervals for discrimination 
    in the tails of a distribution.
    In general, an upward trend in a mean excess function plot shows 
    heavy-tailed behaviour. In particular, a straight line with positive 
    gradient above some threshold is a sign of Pareto behaviour in tail. 
    A downward trend shows thin-tailed behaviour whereas a line with 
    zero gradient shows an exponential tail. Here are some hints:
    Because upper plotting points are the average of a handful of extreme 
    excesses, these may be omitted for a prettier plot. 
    For \code{mrlPlot} and \code{mxfPlot} the upper tail is investigated; 
    for the lower tail reverse the sign of the \code{data} vector.
    \cr
    
    
    \bold{Plot of the Maximum/Sum Ratio:}
    \cr\cr
    The ratio of maximum and sum is a simple tool for detecting heavy 
    tails of a distribution and for giving a rough estimate of
    the order of its finite moments. Sharp increases in the curves
    of a \code{msratioPlot} are a sign for heavy tail behaviour.
    \cr
    
    
    \bold{Plot of the Development of Records:}
    \cr\cr
    These are functions that investigate the development of records in 
    a dataset and calculate the expected behaviour for iid data.
    \code{recordPlot} counts records and reports the observations 
    at which they occur. In addition subsamples can be investigated
    with the help of the function \code{ssrecords}.
    \cr
    
    
    \bold{ACF Plot of Exceedences over a Thresold:}
    \cr\cr
    This function plots the autocorrelation functions of heights and 
    distances of exceedences over a threshold.
    \cr
}


\value{
  
    The functions return a plot.

}


\note{

    The plots are labeled by default with a x-label, a y-label and
    a main title. If the argument \code{label} is set to \code{FALSE}
    neither a x-label, a y-label nor a main title will be added to 
    graph. To add user defined label strings "\dots" just use the 
    function \code{title(xlab="\dots", ylab="\dots", main="\dots")}.
    
}


\author{

    Some of the functions were implemented from Alec Stephenson's 
    R-package \code{evir} ported from Alexander McNeil's S library 
    \code{EVIS}, \emph{Extreme Values in S}, some from Alec Stephenson's 
    R-package \code{ismev} based on Stuart Coles code from his book, 
    \emph{Introduction to Statistical Modeling of Extreme Values} and 
    some were written by Diethelm Wuertz.
    
}


\references{

Coles S. (2001);
    \emph{Introduction to Statistical Modelling of Extreme Values},
    Springer.
    
Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, Springer.  
    
}


\examples{ 
## emdPlot -
   # Danish fire insurance data show Pareto tail behaviour:
   par(mfrow = c(2, 2))
   data(danish)
   emdPlot(danish, plottype = "xy", labels = FALSE)
   title(xlab = "x", ylab = "1-F(x)", main = "Danish Fire")   
   # BMW Stocks:
   data(bmw)
   emdPlot(bmw, plottype = "xy", labels = FALSE)
   title(xlab = "x", ylab = "1-F(x)", main = "BMW Stocks")  
   # Simulated Student-t:
   emdPlot(rt(5000, 4), plottype = "xy") 
 
## fBasics::qqnormPlot -
   # QQ-Plot of Simulated Normal rvs:
   par(mfrow = c(2, 2))
   set.seed(4711)
   qqnormPlot(rnorm(5000))
   text(-3.5, 3, pos = 4, "Simulated Normal rvs")
   # QQ-Plot of simulated Student-t rvs:
   qqnormPlot(rt(5000, 4))
   text(-3.5, 11.0, pos = 4, "Simulated Student-t rvs")
   # QQ-Plot of BMW share residuals:
   data(bmw)
   qqnormPlot(bmw)
   text(-3.5, 0.09, pos = 4, "BMW log returns")     
   
## qPlot -
   # QQ-Plot of heavy-tailed Danish fire insurance data:
   data(danish)
   qPlot(danish) 
 
## mePlot -
   # Sample mean excess plot of heavy-tailed Danish fire 
   # insurance data 
   par(mfrow = c(3, 2))
   data(danish)
   mePlot(danish, labels = FALSE)
   title(xlab = "u", ylab = "e", main = "mePlot - Danish Fire Data")
   
## mrlPlot -
   # Sample mean residual live plot of heavy-tailed Danish Fire 
   # insurance data 
   mrlPlot(danish, labels = FALSE)
   title(xlab = "u", ylab = "e", main = "mrlPlot - Danish Fire Data")
 
## mxfPlot -
   # Plot the mean excess functions for randomly distributed 
   # residuals  
   par(mfrow = c(2, 2))
   n = 10000    
   set.seed(4711)
   xlab = "Threshold: u"; ylab = "Mean Excess: e"
   mxfPlot(rnorm(n), tail = 0.5, labels = FALSE)
   title(xlab = xlab, ylab = ylab, main = "mxf Plot - Normal DF")
   set.seed(7138)
   mxfPlot(rexp(n, 2), tail = 0.5, labels = FALSE)
   title(xlab = xlab, ylab = ylab, main = "mxfPlot - Exponential DF")
   abline(1/2, 0)
   set.seed(6952)
   mxfPlot(rlnorm(n, 0, 2), tail = 0.5, xlim = c(0,90), 
     ylim = c(0, 120), labels = FALSE)
   title(xlab = xlab, ylab = ylab, main = "mxfPlot - Lognormal DF")
   set.seed(8835)
   mxfPlot(rgpd(n, 1/2), tail = 0.10, xlim = c(0,200), 
     ylim=c(0,200), labels = FALSE)
   title(xlab = xlab, ylab = ylab, main = "mxfPlot - Pareto")
   abline(0, 1)  
 
## msratioPlot -
   # Examples for Ratio of Maximum and Sum Plots:
   par(mfrow = c(3, 2))
   data(bmw)
   xlab = "n"; ylab = "R(n)"
   msratioPlot (rnorm(8000), labels = FALSE)
   title(xlab = xlab, ylab = ylab, main = "Standard Normal")
   msratioPlot (rexp(8000), labels = FALSE)
   title(xlab = xlab, ylab = ylab, main = "Exponential")
   msratioPlot (rt(8000, 4), labels = FALSE)
   title(xlab = xlab, ylab = ylab, main = "Student-t")
   msratioPlot (rcauchy(8000), labels = FALSE)
   title(xlab = xlab, ylab = ylab, main = "Cauchy")
   msratioPlot (bmw, labels = FALSE)
   title(xlab = xlab, ylab = ylab, main = "BMW Returns")
  
## recordsPlot -
   # Record fire insurance losses in Denmark
   par(mfrow = c(2, 2))
   data(danish)
   recordsPlot(danish)
   text(1, 7.9, pos = 4, "Danish Fire")
   # BMW Stocks
   data(bmw)
   recordsPlot(bmw)
   text(1, 12.8, pos = 4, "BMW Shares")
      
## ssrecordsPlot -
   # Record fire insurance losses in Denmark
   ssrecordsPlot(danish)
   text(1, 9.2, pos = 4, "Danish Fire")
   # BMW Stocks
   ssrecordsPlot(bmw)
   text(1, 10.5, pos = 4, "BMW Shares")  
 
## xacfPlot -
   # Plot ACF of Heights/Distances of Eceedences over threshold:
   par(mfrow = c(2, 2))
   data(bmw)
   xacfPlot(bmw)
}

\keyword{hplot}

