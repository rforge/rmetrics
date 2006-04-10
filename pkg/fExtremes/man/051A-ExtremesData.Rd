\name{ExtremesData}

\alias{ExtremesData}

\alias{emdPlot}
\alias{qqPlot}
\alias{qqbayesPlot}
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

\alias{interactivePlot}
\alias{gridVector}

\alias{blockMaxima}
\alias{findThreshold}
\alias{pointProcess}
\alias{deCluster}


\title{Explorative Data Analysis}


\description{

    A collection and description of functions for explorative 
    data analysis including data preprocessing of extreme values. 
    The tools include plot functions for emprical distributions, 
    quantile plots, graphs exploring the properties of exceedences 
    over a threshold, plots for mean/sum ratio and for the development 
    of records. The data preprocessing includes tools to 
    separate data beyond a threshold value, to compute blockwise 
    data like block maxima, and to decluster point process data.
    \cr
    
    The plot functions are:
    
    \tabular{ll}{
    \code{emdPlot} \tab Plot of empirical distribution function, \cr
    \code{qqPlot} \tab Normal quantile-quantile plot, \cr
    \code{qqbayesPlot} \tab Normal QQ-Plot with 95 percent intervals, \cr
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
    
    The functions for data preprocessing are:
    
    \tabular{ll}{
    \code{blockMaxima} \tab Block Maxima from a vector or a time series, \cr 
    \code{findThreshold} \tab Upper threshold for a given number of extremes, \cr  
    \code{pointProcess} \tab Peaks over Threshold from a vector or a time series, \cr 
    \code{deCluster} \tab Declusters clustered point process data. }
    
    Utility functions are:
    
    \tabular{ll}{
    \code{interactivePlot} \tab a framework for interactive plot displays, \cr
    \code{gridVector} \tab creates from two vectors x and y all grid points. }
    
}


\usage{
emdPlot(x, doplot = TRUE, plottype = c("xy", "x", "y", ""), labels = TRUE, \dots)

qqPlot(x, doplot = TRUE, labels = TRUE, \dots) 
qqbayesPlot(x, doplot = TRUE, labels = TRUE, \dots)
qPlot(x, xi = 0, trim = NULL, threshold = NULL, doplot = TRUE, labels = TRUE, \dots)

mePlot(x, doplot = TRUE, labels = TRUE, \dots)
mrlPlot(x, ci = 0.95, umin = NA, umax = NA, nint = 100, doplot = TRUE, 
     plottype = c("autoscale", ""), labels = TRUE, \dots)  
mxfPlot(x, u = quantile(x, 0.05), doplot = TRUE, labels = TRUE, \dots)  
   
msratioPlot(x, p = 1:4, doplot = TRUE, plottype = c("autoscale", ""), 
    labels = TRUE, \dots) 
   
recordsPlot(x, conf = 0.95, doplot = TRUE, labels = TRUE, \dots)
ssrecordsPlot(x, subsamples = 10, doplot = TRUE, plottype = c("lin", "log"),
    labels = TRUE, \dots)
    
sllnPlot(x, doplot = TRUE, labels = TRUE, ...)
lilPlot(x, doplot = TRUE, labels = TRUE, ...)

xacfPlot(x, threshold = 0.95, lag.max = 15, doplot = TRUE, labels = TRUE, \dots)

blockMaxima(x, block = c("monthly", "quarterly"), doplot = FALSE)
findThreshold(x, n = floor(0.05*length(as.vector(x))))
pointProcess(x, u = quantile(x, 0.95), column = 1)
deCluster(x, run = 20)

interactivePlot(x, choices = paste("Plot", 1:9), 
    plotFUN = paste("plot.", 1:9, sep = ""), which = "all", \dots)
gridVector(x, y)
}


\arguments{

    \item{block}{
        [blockMaxima] - \cr
        the block size. A numeric value is interpreted as the number  
        of data values in each successive block. All the data is used,
        so the last block may not contain \code{block} observations. 
        If the \code{data} has a  \code{times} attribute containing (in
        an object of class \code{"POSIXct"}, or an object that can be
        converted to that class, see \code{\link{as.POSIXct}}) the
        times/dates of each observation, then \code{block} may instead
        take the character values \code{"month"}, \code{"quarter"},
        \code{"semester"} or \code{"year"}. By default monthly blocks
        from daily data are assumed.
        }
    \item{choices}{
        [interactivePlot] - \cr
        a vector of character strings for the
        choice menu. By Default \code{"Plot 1"} ... \code{"Plot 9"}
        allowing for 9 plots at maximum.
        }
    \item{column}{
        an integer value or column name string which allows to extract
        the specified column from a multivariate time Series.
        }
    \item{conf}{
        [recordsPlot] - \cr
        a confidence level. By default 0.95, i.e. 95\%.
        }
    \item{doplot}{
        a logical. Should the results be plotted? By default \code{TRUE}.
        }
    \item{labels}{
        a logical. Whether or not x- and y-axes should be automatically 
        labelled and a default main title should be added to the plot.
        By default \code{TRUE}.
        }
    \item{lag.max}{
        [xacfPlot] - \cr
        maximum number of lags at which to calculate the autocorrelation 
        functions. The default value is 15.
        }
    \item{n}{
        [findThreshold] - \cr
        a numeric value or vector giving number of extremes above 
        the threshold. By default, \code{n} is 
        set to an integer representing 5\% of the data from the 
        whole data set \code{x}.
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
    \item{plotFUN}{
        [interactivePlot] - \cr
        a vector of character strings naming the
        plot functions. By Default \code{"plot.1"} ... \code{"plot.9"}
        allowing for 9 plots at maximum.
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
    \item{run}{
        [deCluster] - \cr
        parameter to be used in the runs method; any two consecutive 
        threshold exceedances separated by more than this number of 
        observations/days are considered to belong to different clusters.
        }  
    \item{subsamples}{
        [ssrecordsPlot] - \cr
        the number of subsamples, by default 10, an integer value.
        }
    \item{tail}{
        [mxfPlot] - \cr
        the threshold determined from the relative number of data points 
        defining the tail, a numeric value; by default 0.05 which says 
        that 5\% of the data make the tail.
        }
    \item{threshold, trim}{
        [qPlot][xacfPlot] - \cr
        a numeric value at which data are to be left-truncated, value 
        at which data are to be right-truncated or the thresold value, 
        by default 95\%.
        }
    \item{u}{
        a numeric value at which data are to be truncated. By default
        the threshold value which belongs to the 95\% quantile.
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
        \cr
        [finThreshold][blocks][blockMaxima][deCluster] - \cr
        a numeric data vector from which \code{findThreshold} and 
        \code{blockMaxima} determine the threshold values and block 
        maxima values. 
        For the function \code{deCluster} the argument
        \code{x} represents a numeric vector of threshold exceedances
        with a \code{times} attribute which should be a numeric
        vector containing either the indices or the times/dates
        of each exceedance (if times/dates, the attribute should
        be an object of class \code{"POSIXct"} or an object that
        can be converted to that class; see \code{\link{as.POSIXct}}).
        [gridVector] - \cr
        two numeric vector which span the two dimensional grid. 
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
    "how close" to the normal the data appears, \code{qqbayesPlot} adds 
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
    
    
    \bold{Finding Thresholds:} 
    \cr\cr
    The function \code{findThreshold} finds a threshold so that a given 
    number of extremes lie above. When the data are tied a threshold is 
    found so that at least the specified number of extremes lie above.
    \cr
    
  
    \bold{Computing Block Maxima:} 
    \cr\cr  
    The function \code{blockMaxima} calculates block maxima from a vector 
    or a time series, whereas the function
    \code{blocks} is more general and allows for the calculation of
    an arbitrary function \code{FUN} on blocks.
    \cr
    
        
    \bold{De-Clustering Point Processes:} 
    \cr\cr
    The function \code{deCluster} declusters clustered point process 
    data so that Poisson assumption is more tenable over a high threshold.

}


\value{
  
    \code{blockMaxima} 
    \cr
    returns a timeSeries object or a numeric vector of block 
    maxima data.
    
    \code{findThreshold} 
    \cr
    returns a numeric value or vector of suitable thresholds. 
    
    \code{pointProcess} 
    \cr
    returns a timeSeries object or a numeric vector of peaks over
    a threshold.
    
    \code{deCluster} 
    \cr
    returns a timeSeries object or a numeric vector for the 
    declustered point process. 

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
## SOURCE("fExtremes.051A-ExtremesData")
\dontrun{
## emdPlot -
   xmpExtremes("\nStart: Empirical Distribution Function >")
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
 
## qqPlot -
   xmpExtremes("\nNext: Quantile-Quantile Plot >")
   # QQ-Plot of Simulated Normal rvs:
   par(mfrow = c(2, 2))
   set.seed(4711)
   qqPlot(rnorm(5000))
   text(-3.5, 3, pos = 4, "Simulated Normal rvs")
   # QQ-Plot of simulated Student-t rvs:
   qqPlot(rt(5000, 4))
   text(-3.5, 11.0, pos = 4, "Simulated Student-t rvs")
   # QQ-Plot of BMW share residuals:
   data(bmw)
   qqPlot(bmw)
   text(-3.5, 0.09, pos = 4, "BMW log returns")     
   
## qPlot -
   xmpExtremes("\nNext: QQ-Plot of Heavy Tails >")
   # QQ-Plot of heavy-tailed Danish fire insurance data:
   data(danish)
   qPlot(danish) 
 
## mePlot -
   xmpExtremes("\nNext: Mean Excess Plot >")
   # Sample mean excess plot of heavy-tailed Danish fire 
   # insurance data 
   par(mfrow = c(3, 2))
   data(danish)
   mePlot(danish, labels = FALSE)
   title(xlab = "u", ylab = "e", main = "mePlot - Danish Fire Data")
   
## mrlPlot -
   xmpExtremes("\nNext: mean Residual Live Plot >")
   # Sample mean residual live plot of heavy-tailed Danish Fire 
   # insurance data 
   mrlPlot(danish, labels = FALSE)
   title(xlab = "u", ylab = "e", main = "mrlPlot - Danish Fire Data")
 
## mxfPlot -
   xmpExtremes("\nNext: Mean Excess Function Plot >")
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
   xmpExtremes("\nNext: Maximum/Sum Ratio Plot >")
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
   xmpExtremes("\nNext: Records Plot >")
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
   xmpExtremes("\nNext: Subsample Record Plot >")
   # Record fire insurance losses in Denmark
   ssrecordsPlot(danish)
   text(1, 9.2, pos = 4, "Danish Fire")
   # BMW Stocks
   ssrecordsPlot(bmw)
   text(1, 10.5, pos = 4, "BMW Shares")  
 
## xacfPlot -
   xmpExtremes("\nNext: ACF Plot of Exceedences >")
   # Plot ACF of Heights/Distances of Eceedences over threshold:
   par(mfrow = c(2, 2))
   data(bmw)
   xacfPlot(bmw)
   
## findThreshold -
   xmpExtremes("\nStart: Find Thresold >")
   # Find threshold giving (at least) fifty exceedances 
   # for Danish Fire data
   data(danish)
   findThreshold(danish, n = c(10, 50, 100))    
   
## blockMaxima -
   xmpExtremes("\nNext: Compute Block Maxima >")
   # Block Maxima (Minima) for the right and left tails 
   # of the BMW log returns:
   par(mfrow = c(2, 1))
   blockMaxima( as.timeSeries(data(bmwRet)), block = 65)
   blockMaxima(-as.timeSeries(data(bmwRet)), block = 65)     
 
## deCluster -
   xmpExtremes("\nNext: De-Cluster Exceedences >")
   # Decluster the 200 exceedances of a particular  
   # threshold in the negative BMW log-return data
   PP = pointProcess(-as.timeSeries(data(bmwRet)), n = 300)
   deCluster(x = PP, run = 15, doplot = TRUE) 
}
}

\keyword{hplot}

