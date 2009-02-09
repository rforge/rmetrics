\name{starsPlot}


\alias{starsPlot}

\alias{assetsStarsPlot}  
\alias{assetsBasicStatsPlot}
\alias{assetsMomentsPlot}  
\alias{assetsBoxStatsPlot} 
\alias{assetsNIGFitPlot}    


\title{Stars Plots of Assets.}


\description{
  
    Displays star plots to compare assets sets.

}


\usage{
assetsStarsPlot(x, method = c("segments", "stars"), locOffset = c(0, 0),
    keyOffset = c(0, 0), \dots)
    
assetsBoxStatsPlot(x, par = TRUE, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "Box Plot Statistics", descriptionPosition = c(3, 3.50), \dots)
assetsBasicStatsPlot(x, par = TRUE, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "Basic Returns Statistics", descriptionPosition = c(3, 3.50), \dots)
assetsMomentsPlot(x, par = TRUE, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "Moments Statistics", descriptionPosition = c(3, 3.50), \dots)
assetsNIGFitPlot(x, par = TRUE, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "NIG  Parameters", descriptionPosition = c(3, 3.50), \dots)
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
        a numeric vector of lenght two, specifying an offset in the
        legend with respect to \code{x} and
        \code{y} direction.
        }
    \item{locOffset}{
        a numeric vector of lenght two, specifying an offset in the
        location of the stars/circles with respect to \code{x} and
        \code{y} direction.
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


\details{


    \code{assetsStarsPlot}\cr
    draws segment or star diagrams of data sets,
    
    \code{assetsBasicStatsPlot}\cr
    displays a segment plot of box plot statistics,
    
    \code{assetsMomentsPlot}\cr
    displays a segment plot of distribution moments,
    
    \code{assetsBoxStatsPlot}\cr
    displays a segment plot of box plot statistics,
    
    \code{assetsNIGFitPlot}\cr
    displays a segment plot NIG parameter estimates.  
    
}
\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## LPP2005REC -
   LPP = as.timeSeries(data(LPP2005REC))

## assetsBasicStatsPlot -
   # par(mfrow = c(1, 1))
   assetsBasicStatsPlot(LPP, 
     title = "", description = "")
   
## assetsMomentsPlot -
   assetsMomentsPlot(LPP, 
     title = "", description = "")
   
## assetsBoxStatsPlot -
   assetsBoxStatsPlot(LPP, 
     title = "", description = "")

## assetsNIGFitPlot -
   assetsNIGFitPlot(LPP[, 7:9], 
     title = "", description = "")
}


\keyword{models}

