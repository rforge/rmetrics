\name{ReturnSeriesBasics}

\alias{ReturnSeriesBasics}

\alias{basicStats}

\alias{seriesPlot}
\alias{cumulatedPlot}
\alias{histPlot}
\alias{densityPlot}

\alias{qqnormPlot}
\alias{qqnigPlot}

\alias{boxPlot}
\alias{boxPercentilePlot}

\alias{returnSeriesGUI}


\title{Return Series Basics}


\description{
    
    A collection and description of functions which
    allow to investigate and display the basics of 
    financial return sderies.
    \cr

    List of Functions:
    
    \tabular{ll}{
    \code{basicStats} \tab Computes an overview of basic statistical values, \cr
    \code{seriesPlot} \tab Returns a tailored return series plot, \cr
    \code{cumulatedPlot} \tab Returns a tailored cumulatede return plot, \cr
    \code{histPlot} \tab Returns a tailored histogram plot, \cr
    \code{densityPlot} \tab Returns a tailored kernel density estimate plot, \cr
    \code{qqnormPlot} \tab Returns a tailored Normal quantile-quantile plot, \cr
    \code{qqnigPlot} \tab Returns a tailored NIG quantile-quantile plot, \cr
    \code{boxPlot} \tab Returns a side-by-side standard box plot, \cr
    \code{boxPercentilePlot} \tab Returns a side-by-side box-percentile plot, \cr
    \code{returnSeriesGUI} \tab Opens a GUI for return series plots.}
    
}
    

\usage{ 
basicStats(x, ci = 0.95)

seriesPlot(x, labels = TRUE, type = "l", col = "steelblue", 
    ylab = "Returns", rug = TRUE, \dots) 
cumulatedPlot(x, index = 100, labels = TRUE, type = "l", col = "steelblue", 
    ylab = "Index", rug = TRUE, \dots)
histPlot(x, labels = TRUE, col = "steelblue", add.fit = TRUE, rug = TRUE, 
    skipZeros = TRUE, \dots) 
densityPlot(x, labels = TRUE, col = "steelblue", add.fit = TRUE, 
    rug = TRUE, skipZeros = TRUE, \dots) 

qqnormPlot(x, labels = TRUE, col = "steelblue", rug = TRUE, 
    scale = TRUE, \dots) 
qqnigPlot(x, labels = TRUE, col = "steelblue", rug = TRUE, \dots)

boxPlot(x, col = "steelblue", \dots)
boxPercentilePlot(x, col = "steelblue", \dots) 

returnSeriesGUI(x)
}


\arguments{
  
    \item{add.fit}{
        [*Plot] - \cr
        a logical, should a fit added to the Plot?
        }
    \item{ci}{
        [basicsStats] - \cr
        confidence interval, a numeric value, by default 0.95, 
        i.e. 95 percent.
        }
    \item{col, ylab}{
        [*Plot] - \cr
        plot parameter, color, main title, and y label to be used. Only
        active when \code{labels=TRUE}.
        } 
    \item{index}{
        [cumulatedPlot] - \cr
        a numeric value, by default 100. The function cumulates 
        column by colum the returns and multiplies the result with 
        the index value: \code{index*exp(colCumsums(x))}. 
        }
    \item{labels}{
        a logical, should the plot be tailored?
        }
    \item{rug}{
        a logical value by default TRUE. Should a rug representation 
        of the data added to the plot?
        }
    \item{scale}{
        a logical value by default TRUE. Should the time series be 
        scale for further investigation?
        }
    \item{skipZeros}{
        a logical, should zeros be skipped in the return Series?
        }
    \item{type}{
        what type of plot should be drawn. For ossible types consult
        the \code{plot} help page.
        }
    \item{x}{
        an object of class \code{"timeSeries"} or any other object which
        can be transformed by the function \code{as.timeSeries} into an
        object of class \code{timeSeries}.
        }
    \item{\dots}{
        optional arguments to be passed.
        }
    
}


\value{
       
    \code{basicsStats}
    \cr
    returns data frame with the following entries and row names:
    nobs, NAs, Minimum, Maximum , 1. Quartile, 3. Quartile,
    Mean, Median, Sum, SE Mean, LCL Mean, UCL Mean, Variance,
    Stdev, Skewness, Kurtosis.
    
    \code{*Plot}
    \cr
    For the \code{*Plot} functions, beside the plot no values are 
    returned.\cr
    
    \code{returnSeriesGUI}
    \cr
    For the \code{returnSeriesGUI} function, beside the graphical
    user interface no values are returned.\cr
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## basicStats -
   # Simulated Monthly Return Data:
   tS = timeSeries(matrix(rnorm(12)), timeCalendar())
   # ... must be univariate:
   basicStats(tS)
}


\keyword{programming}

