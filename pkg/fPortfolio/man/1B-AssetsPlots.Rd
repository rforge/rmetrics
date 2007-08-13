\name{AssetsPlots}

\alias{AssetsPlots}

\alias{assetsPlot}
\alias{assetsSeriesPlot}
\alias{assetsHistPlot}
\alias{assetsDensityPlot}
\alias{assetsQQNormPlot}

\alias{assetsBoxPlot}
\alias{assetsBoxPercentilePlot}

\alias{assetsPairsPlot}
\alias{assetsCorTestPlot}
\alias{assetsCorgramPlot}

\alias{assetsCorEigenPlot}
\alias{assetsTreePlot}
\alias{assetsDendrogramPlot}


\title{Plots of Multivariate Asset Sets}


\description{
  
    A collection and description of functions which 
    display several different kind of views on
    multivariate data sets of assets.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsPlot} \tab Plots an overview of single assets, \cr
    \code{assetsSeriesPlot} \tab Displays time series of individual assets, \cr
    \code{assetsHistPlot} \tab Displays histograms of individual assets, \cr
    \code{assetsDensityPlot} \tab Displays density plots of individual assets, \cr
    \code{assetsQQNormPlot} \tab Displays normal qq-plots of individual assets, \cr
    \code{assetsBoxPlot} \tab Producess standard box plots, \cr
    \code{assetsBoxPercentilePlot} \tab roducess side-by-side box-percentile plots, \cr 
    \code{assetsPairsPlot} \tab Displays pairs of scatterplots of individual assets, \cr
    \code{assetsCorTestPlot} \tab Displays and tests pairwise correlations, \cr
    \code{assetsCorgramPlot} \tab Displays correlations between assets, \cr
    \code{assetsCorEigenPlot} \tab Displays ratio of the largest two eigenvalues, \cr
    \code{assetsTreePlot} \tab Displays minimum spanning tree of assets, \cr
    \code{assetsDendrogramPlot} \tab Displays hierarchical clustering dendrogram. }
    
}
    

\usage{
assetsPlot(x, title = NULL, \dots)

assetsSeriesPlot(x, which = 1:dim(x)[2], \dots)
assetsHistPlot(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
    which = 1:dim(x)[2], xlim = NULL, skipZeros = FALSE, \dots) 
assetsDensityPlot(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
    which = 1:dim(x)[2], \dots)
assetsQQNormPlot(x, which = 1:dim(x)[2], \dots)

assetsBoxPlot(x, col = "bisque", \dots)
assetsBoxPercentilePlot(x, col = "bisque", \dots) 

assetsPairsPlot(x, labels = TRUE, \dots)
assetsCorTestPlot(x, labels = TRUE, \dots)
assetsCorgramPlot(x, labels = TRUE, method = c("pie", "shade"), \dots)

assetsCorEigenPlot(x, method = c("pearson", "kendall", "spearman"), \dots)
assetsTreePlot(x, method = "euclidian", seed = NULL)
assetsDendrogramPlot(x, method = c(dist = "euclidian", clust = "complete"))
}


\arguments{
  
    \item{col}{
        a character string, defining the color to fill the boxes.
        }
    \item{labels}{
        a logical flag, if \code{TRUE} then default labels will be used,
        otherwise the plots will be displayed without labels and the user
        can add his own labels.
        }
    \item{method}{
        [assetsHistPlot, assetsDensityPlot] - \cr
        a character string, mean-covariance estimator used for the 
        robust gaussian distribution fit.\cr
        [assetsCorgramPlot] - \cr
        a character string, the type of graph used in the lower panel.\cr
        [assetsCorEigenPlot] - \cr
        a character string, the method used to compute the correlation
        matrixs, ee function \code{cor}.\cr
        [assetsTreePlot] - \cr
        a character string, the method used to compute the distance
        matrix, see function \code{dist}.\cr
        [assetsDendrogramPlot] - \cr
        a character vector with two elements, the method used to compute 
        the distance matrix, see function \code{dist}, and the method used 
        for the agglomeration algorithm, see function \code{hclust}.\cr
        }
    \item{seed}{
        [assetsTreePlot] - \cr
        an integer value setting the seed in the computation of the 
        sample ranks.
        }
    \item{skipZeros}{
        [assetsHistPlot] - \cr
        a logical,  should zeros be skipped in the histogram plot of the
        return series ?
        }
    \item{title}{
        a character string, the plot title.
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
