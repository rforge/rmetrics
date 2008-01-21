\name{starsPlot}


\alias{starsPlot}

\alias{assetsStarsPlot}  
\alias{assetsMomentsPlot} 
\alias{assetsBasicStatsPlot} 
\alias{assetsBoxStatsPlot} 
\alias{assetsNIGFitPlot}    


\title{Assets Stars Plots}


\description{
  
    A collection and description of functions which 
    display several different kind of views on
    multivariate data sets of assets.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    
    % STARS PLOTS:
    \code{assetsStarsPlot} \tab Draws segment or star diagrams of data sets, \cr
    \code{assetsBasicStatsPlot} \tab Displays a segment plot of box plot statistics, \cr
    \code{assetsMomentsPlot} \tab Displays a segment plot of distribution moments, \cr
    \code{assetsBoxStatsPlot} \tab Displays a segment plot of box plot statistics, \cr
    \code{assetsNIGFitPlot} \tab Displays a segment plot NIG parameter estimates. }
}


\usage{
assetsStarsPlot(x, method = c("segments", "stars"), 
    keyOffset = c(0, 0), \dots)
assetsBoxStatsPlot(x, par = TRUE, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "Box Plot Statistics", descriptionPosition = c(3, 3.50))
assetsBasicStatsPlot(x, par = TRUE, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "Basic Returns Statistics", descriptionPosition = c(3, 3.50))
assetsMomentsPlot(x, par = TRUE, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "Moments Statistics", descriptionPosition = c(3, 3.50))
assetsNIGFitPlot(x, par = TRUE, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "NIG  Parameters", descriptionPosition = c(3, 3.50))
}


\arguments{
  
    \item{description}{
        a destription string.
        }
    \item{descriptionPosition}{
        the position of the description string.
        }
    \item{method}{
        a character string from to select the plot method. Eiter a 
        \code{"star"} or a \code{"segment"} plot.
        }
    \item{keyOffset}{
        to set the object of the legend.
        }
    \item{main}{
        to set the main title.
        }
    \item{mar}{
        to set the number of lines of margin to be specified on the four 
        sides of the plot. The default is \code{c(5,4,4,2)+0.1}.
        }
    \item{oma}{
        to set the size of the outer margins in lines of text.
        }
    \item{par}{
        a logical flag. Should be internal \code{par()} setting be used?
        }
    \item{title}{
        a character string, the plot title.
        }
    \item{titlePosition}{
        the position of the title string.
        }
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\seealso{

    \code{MultivariateDistribution}.

}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP2005REC -
   x = as.timeSeries(data(LPP2005REC))
   head(x)

## assetsBoxStatsPlot -
   assetsBoxStatsPlot(x, title = "", description = "")

## assetsBasicStatsPlot -
   assetsBasicStatsPlot(x, title = "", description = "")

## assetsMomentsPlot -
   assetsMomentsPlot(x, title = "", description = "")

## assetsNIGFitPlot -
   assetsNIGFitPlot(x, title = "", description = "")
}


\keyword{models}

