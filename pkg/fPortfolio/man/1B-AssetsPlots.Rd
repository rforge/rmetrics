\name{AssetsPlots}

\alias{AssetsPlots}

\alias{assetsSeriesPlot}
\alias{assetsHistPlot}
\alias{assetsDensityPlot}
\alias{assetsQQNormPlot}

\alias{assetsPairsPlot}
\alias{assetsCorTestPlot}


\title{Plots of Multivariate Asset Sets}


\description{
  
    A collection and description of functions which 
    display several different kind of views on
    multivariate data sets of assets.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsSeriesPlot} \tab Displays time series of individual assets, \cr
    \code{assetsHistPlot} \tab Displays histograms of individual assets, \cr
    \code{assetsDensityPlot} \tab Displays density plots of individual assets, \cr
    \code{assetsQQNormPlot} \tab Displays normal qq-plots of individual assets, \cr
    \code{assetsPairsPlot} \tab Displays pairs of scatterplots of individual assets, \cr
    \code{assetsCorTestPlot} \tab S3 Displays and tests pairwise correlations of assets. }
    
}
    

\usage{
assetsSeriesPlot(x, which = 1:dim(x)[2], \dots)
assetsHistPlot(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
    which = 1:dim(x)[2], xlim = NULL, \dots) 
assetsDensityPlot(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
    which = 1:dim(x)[2], \dots)
assetsQQNormPlot(x, which = 1:dim(x)[2], \dots)

assetsPairsPlot(x, labels = TRUE)
assetsCorTestPlot(x, scale = 1, labels = TRUE, \dots)
}


\arguments{
  
    \item{labels}{
        a logical flag, if \code{TRUE} then default labels will be used,
        otherwise the plots will be displayed without labels and the user
        can add his own labels.
        }
    \item{method}{
        a character string, defining the method to be used for calculating
        the covariance.
        }
    \item{scale}{
        which of the plots should be displayed? \code{which} can 
        be either a character string, \code{"all"} (displays all plots) 
        or \code{"ask"} (interactively asks which one to display), or a 
        vector of logical values, for those elements which are set 
        \code{TRUE} the correponding plot will be displayed.
        }
    \item{which}{
        an integer value, defining which plot in a series of plots 
        should be displayed.
        }
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{xlim}{
        a numeric vector with two elements defining the range of the plot.
        If the value of \code{cxlim=NULL}, the default setting, then the
        range will be automatically selected. 
        } 
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


%\value{}


%\details{
%
%    \bold{Displaying Asset Return Plots:}
%    \cr\cr
%    xxx 
%    \cr
%    
%    
%    \bold{Displaying Bivariate Assets Returns Plots:}
%    \cr\cr   
%    The xxx
%
%}



%\references{}



\seealso{

    \code{MultivariateDistribution}.

}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## berndtInvest -
   data(berndtInvest)
   # Select "CONTIL" "DATGEN" "TANDY" and "DEC" Stocks:
   select = c("CONTIL", "DATGEN", "TANDY", "DEC")
   # Convert into a timeSeries object:
   berndtAssets.tS = as.timeSeries(berndtInvest)[, select]
   head(berndtAssets.tS)
   
## assetsSeriesPlot - 
   # Display time series of individual assets
   par(mfrow = c(2, 2), cex = 0.7)
   assetsSeriesPlot(berndtAssets.tS)
       
## assetsHistPlot -       
   # Display histograms of individual assets 
   assetsHistPlot(berndtAssets.tS)
   
## assetsQQNormPlot -       
   # Display normal qq-plots of individual assets
   assetsQQNormPlot(berndtAssets.tS)

## assetsPairsPlot -        
   # Display pairs of scatterplots of individual assets
   assetsPairsPlot(berndtAssets.tS)
   
## assetsCorTestPlot -      
   # Display and tests pairwise correlations of assets
   assetsCorTestPlot(berndtAssets.tS)   
}


\keyword{models}
