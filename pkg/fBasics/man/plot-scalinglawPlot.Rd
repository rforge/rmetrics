\name{ScalingLawPlot}

\alias{scalinglawPlot}


\alias{scalinglawPlot}


\title{Scaling Law Bahaviour}


\description{

    Evaluates the scaling exponent of a financial 
    return series and plots the scaling law.
    
}


\usage{
scalinglawPlot(x, span = ceiling(log(length(x)/252)/log(2)), doplot = TRUE, 
    labels = TRUE, trace = TRUE, \dots)
}


\arguments{

    \item{doplot}{
        a logical value. Should a plot be displayed?
        }
    \item{labels}{
        a logical value. Whether or not x- and y-axes should be automatically 
        labeled and a default main title should be added to the plot.
        By default \code{TRUE}.
        }
    \item{span}{
        an integer value, determines for the \code{qqgaussPlot} the 
        plot range, by default 5, and for the \code{scalingPlot} a
        reasonable number of of points for the scaling range, by
        default daily data with 252 business days per year are
        assumed.
        }
    \item{trace}{
        a logical value. Should the computation be traced?
        }
    \item{x}{
        an uni- or multivariate return series of class \code{timeSeries} 
        or any other object which can be transformed by the function
        \code{as.timeSeries()} into an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments to be passed.
        }

}


\value{
    
    returns a list with the following components: \code{Intercept},
    \code{Exponent} the scaling exponent, and \code{InverseExponent}
    its inverse value.
    
}

\details{

    
    \bold{Scaling Behavior:}
    \cr\cr
    The function \code{scalingPlot} plots the scaling law of financial 
    time series under aggregation and returns an estimate for the scaling 
    exponent. The scaling behavior is a very striking effect of the 
    foreign exchange market and also other markets expressing a regular
    structure for the volatility. Considering the average absolute
    return over individual data periods one finds a scaling power law
    which relates the mean volatility over given time intervals
    to the size of these intervals. The power law is in many cases 
    valid over several orders of magnitude in time. Its exponent  
    usually deviates significantly from a Gaussian random walk model 
    which implies 1/2. 
  
}


\references{

Taylor S.J. (1986); 
    \emph{Modeling Financial Time Series},
    John Wiley and Sons, Chichester.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{   
## data - 
   SPI <- LPP2005REC[, "SPI"]
   plot(SPI, type = "l", col = "steelblue", main = "SP500")
   abline(h = 0, col = "grey")

## teffectPlot -
   # Scaling Law Effect:
   scalinglawPlot(SPI)
}


\keyword{hplot}

