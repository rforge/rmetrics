\name{ReturnSeriesBasics}

\alias{ReturnSeriesBasics}

\alias{seriesPlot}
\alias{histPlot}
\alias{densityPlot}

\alias{qqnormPlot}
\alias{qqnigPlot}

\alias{basicStats}
\alias{stdev}

\title{Return Series Basics}


\description{
    
    A collection and description of functions which
    allow to investigate and display the basics of 
    financial return sderies.
    \cr

    Tailored plot functions:
    
    \tabular{ll}{
    \code{seriesPlot} \tab Returns a tailored time series plot, \cr
    \code{histPlot} \tab Returns a tailored histogram plot, \cr
    \code{densityPlot} \tab Returns a tailored kernel density estimate plot, \cr
    \code{qqnormPlot} \tab Returns a tailored Normal quantile-quantile plot, \cr
    \code{qqnigPlot} \tab Returns a tailored NIG quantile-quantile plot.}
    
    Basic Statistic functions are:
    
    \tabular{ll}{
    \code{basicStats} \tab computes an overview of basic statistical values. }
    
    For SPLUS Compatibility:
    
    \tabular{ll}{
    \code{stdev} \tab Returns the standard deviation of a vector or matrix. }
    
}
    

\usage{ 
seriesPlot(x, type = "l", col = "steelblue", main = x@units, rug = TRUE, \dots) 
histPlot(x, labels = TRUE, main = x@units, add.fit = TRUE, \dots) 
densityPlot(x, col = "steelblue", main = x@units, add.fit = TRUE, \dots)

qqnormPlot(x, labels = TRUE, col = "steelblue", main = x@units, \dots) 
qqnigPlot(x, labels = TRUE, col = "steelblue", main = x@units, \dots)

stdev(x, na.rm = FALSE)

basicStats(x, ci = 0.95)
}


\arguments{
  
    \item{add.fit}{
        [*Plot] - \cr
        a logical, should a fit added to the Plot?
        }
    \item{ci}{
        confidence interval, a numeric value, by default 0.95, 
        i.e. 95 percent.
        }
    \item{col, main}{
        [*Plot] - \cr
        plot parameters, color and main title.
        } 
    \item{labels}{
        a logical, should the plot be tailored?
        }
        \item{na.rm}{
        a logical. Should missing values be removed?
        }
   \item{rug}{
        a logical value by default TRUE. Should a rug representation 
        of the data added to the plot?
        }
   \item{type}{
        what type of plot should be drawn. For ossible types consult
        the \code{plot} help page.
        }
   \item{x}{
        an object of class \code{"timeSeries"} or a numeric vector.
        }
    \item{\dots}{
        optional arguments to be passed.
        }
    
}


\value{
    
    \code{*Plot}
    \cr
    For the \code{*Plot} functions. beside the plot, no other values are 
    returned.\cr
    
    \code{basicsStats}
    \cr
    returns data frame with the following entries and row names:
    nobs, NAs, Minimum, Maximum , 1. Quartile, 3. Quartile,
    Mean, Median, Sum, SE Mean, LCL Mean, UCL Mean, Variance,
    Stdev, Skewness, Kurtosis.
    
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

