\name{pairsPlot}


\alias{pirsPlot}

\alias{assetsCorgramPlot}
\alias{assetsPairsPlot}
\alias{assetsCorTestPlot}
\alias{assetsCorImagePlot}   


\title{Assets Pairs Plot}


\description{
  
    A collection and description of functions which 
    display several different kind of views on
    multivariate data sets of assets.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    
    % PAIRS PLOTS:
    \code{assetsPairsPlot} \tab Displays pairs of scatterplots of individual assets, \cr
    \code{assetsCorgramPlot} \tab Displays correlations between assets, \cr 
    \code{assetsCorTestPlot} \tab Displays and tests pairwise correlations, \cr
    \code{assetsCorImagePlot} \tab Displays an image plot of a correlations.}
    
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
  
    \item{abbreviate}{
        allows to abbreviate strings to at least \code{abbreviate} 
        characters, such that they remain unique, if they were. 
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
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{use}{
        a character string indicating which correlation coefficient or 
        covariance is to be computed. One of \code{"pearson"}, the default, 
        \code{"kendall"}, or \code{"spearman"}.
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
   LPP = as.timeSeries(data(LPP2005REC))
   head(LPP)
   
## assetsPairsPlot - 
   assetsPairsPlot(LPP)
  
## assetsCorgramPlot -  
   assetsCorgramPlot(LPP, method = "pie")
   assetsCorgramPlot(LPP, method = "shade")
   
## assetsCorTestPlot -
   assetsCorTestPlot(LPP)
   
## assetsCorImagePlot -
   assetsCorImagePlot(LPP)    
}


\keyword{models}

