\name{plot-pairs}


\alias{pairsPlot}

\alias{assetsPairsPlot}
\alias{assetsCorgramPlot}
\alias{assetsCorTestPlot}
\alias{assetsCorImagePlot}   


\title{Assets Pairs Plot}


\description{
  
    Display several aspects of correlation bettween pairs of
    assets.   
    
}


\usage{
assetsPairsPlot(x, labels = TRUE, \dots)
assetsCorgramPlot(x, labels = TRUE, 
    method = c("pie", "shade"), \dots)
assetsCorTestPlot(x, labels = TRUE, \dots)
assetsCorImagePlot(x, labels = TRUE, show = c("cor", "test"), 
    use = c("pearson", "kendall", "spearman"), abbreviate = 3, \dots)
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{labels}{
        a logical flag, if \code{TRUE} then default labels will be used,
        otherwise the plots will be displayed without labels and the user
        can add his own labels.
        }
    \item{method}{
        a character string, the type of graph used in the lower panel.
        }
    \item{show}{
        a character string, what should be pressented, correlations or
        results from correlation tests?
        }
    \item{use}{
        a character string indicating which correlation coefficient or 
        covariance is to be computed. One of \code{"pearson"}, the default, 
        \code{"kendall"}, or \code{"spearman"}.
        }
    \item{abbreviate}{
        allows to abbreviate strings to at least \code{abbreviate} 
        characters, such that they remain unique, if they were. 
        }
    \item{\dots}{
        optional arguments to be passed.
        }   
        
}


\details{


    \code{assetsPairsPlot}\cr
    displays pairs of scatterplots of individual assets,
    
    \code{assetsCorgramPlot}\cr
    displays correlations between assets, 
    
    \code{assetsCorTestPlot}\cr
    displays and tests pairwise correlations,
    
    \code{assetsCorImagePlot}\cr
    displays an image plot of a correlations.
    
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
   # Load Swiss Pension Fund Data:
   LPP <- LPP2005REC[, 1:6]
   head(LPP)   
   
## assetsPairsPlot -
   # Create Pairs Plot:  
   assetsPairsPlot(LPP)
  
## assetsCorgramPlot - 
   # Create Corellogram Plot:  
   assetsCorgramPlot(LPP, method = "pie")
   assetsCorgramPlot(LPP, method = "shade")
   
## assetsCorTestPlot - 
   # Create Correlation Test Plot: 
   assetsCorTestPlot(LPP)
   
## assetsCorImagePlot -
   # Create Correlation Image Plot: 
   assetsCorImagePlot(LPP)    
}


\keyword{models}

