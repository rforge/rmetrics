\name{AssetsPlots}

\alias{AssetsPlots}

\alias{assetsReturnPlot}
\alias{assetsCumulatedPlot}
\alias{assetsSeriesPlot}
\alias{assetsHistPlot}
\alias{assetsQQNormPlot}

\alias{assetsRiskReturnPlot}
\alias{assetsNIGShapeTrianglePlot}

\alias{assetsBoxPlot}
\alias{assetsBoxPercentilePlot}

\alias{assetsCorgramPlot}
\alias{assetsPairsPlot}
\alias{assetsCorTestPlot}

\alias{assetsCorEigenPlot}
\alias{assetsTreePlot}
\alias{assetsDendrogramPlot}

\alias{assetsStarsPlot}  
\alias{assetsMomentsPlot} 
\alias{assetsBasicStatsPlot} 
\alias{assetsStatsBoxPlot} 
\alias{assetsNIGFitPlot}    



\title{Plots of Multivariate Asset Sets}


\description{
  
    A collection and description of functions which 
    display several different kind of views on
    multivariate data sets of assets.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsReturnPlot} \tab Displays return series of individual assets, \cr
    \code{assetsCumulatedPlot} \tab Displays cumulated returns of individual assets, \cr
    \code{assetsSeriesPlot} \tab Displays time series of individual assets, \cr
    \code{assetsHistPlot} \tab Displays histograms of individual assets, \cr
    \code{assetsQQNormPlot} \tab Displays normal qq-plots of individual assets, \cr
    \code{assetsRiskReturnPlot} \tab Displays risk-return giagram of assets, \cr
    \code{assetsNIGShapeTrianglePlot} \tab Displays NIG Shape Triangle, \cr
    \code{assetsBoxPlot} \tab Producess standard box plots, \cr
    \code{assetsBoxPercentilePlot} \tab roducess side-by-side box-percentile plots, \cr 
    \code{assetsCorgramPlot} \tab Displays correlations between assets, \cr 
    \code{assetsPairsPlot} \tab Displays pairs of scatterplots of individual assets, \cr
    \code{assetsCorTestPlot} \tab Displays and tests pairwise correlations, \cr
    \code{assetsCorEigenPlot} \tab Displays ratio of the largest two eigenvalues, \cr
    \code{assetsTreePlot} \tab Displays minimum spanning tree of assets, \cr
    \code{assetsDendrogramPlot} \tab Displays hierarchical clustering dendrogram, \cr 
    \code{assetsStarsPlot} \tab Draws segment or star diagrams of data sets, \cr
    \code{assetsBasicStatsPlot} \tab Displays a segment plot of box plot statistics, \cr
    \code{assetsMomentsPlot} \tab Displays a segment plot of distribution moments, \cr
    \code{assetsStatsBoxPlot} \tab Displays a segment plot of box plot statistics, \cr
    \code{assetsNIGFitPlot} \tab Displays a segment plot NIG parameter estimates. }
}


\usage{
assetsReturnPlot(x, col = "steelblue", \dots)
assetsCumulatedPlot(x, col = "steelblue", \dots)
assetsSeriesPlot(x, col = "steelblue", ...)
assetsHistPlot(x, col = "steelblue", skipZeros = FALSE, \dots)
assetsQQNormPlot(x, col = "steelblue", skipZeros = FALSE, \dots)

assetsRiskReturnPlot(x, col = "steelblue", percentage = FALSE, scale = 252, \dots)
assetsNIGShapeTrianglePlot(x, col = "steelblue", \dots)

assetsBoxPlot(x, col = "bisque", \dots)
assetsBoxPercentilePlot(x, col = "bisque", \dots) 

assetsCorgramPlot(x, labels = TRUE, method = c("pie", "shade", "hist"), \dots)
assetsPairsPlot(x, labels = TRUE, \dots)
assetsCorTestPlot(x, labels = TRUE, \dots)

assetsCorEigenPlot(x, method = c("pearson", "kendall", "spearman"), \dots)
assetsTreePlot(x, method = "euclidian", seed = NULL)
assetsDendrogramPlot(x, method = c(dist = "euclidian", clust = "complete"))

assetsStarsPlot(x, method = c("segments", "stars"), keyOffset = c(0, 0),\dots)
assetsStatsBoxPlot(x, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "Box Plot Statistics", descriptionPosition = c(3, 3.50))
assetsBasicStatsPlot(x, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "Basic Returns Statistics", descriptionPosition = c(3, 3.50))
assetsMomentsPlot(x, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "Moments Statistics", descriptionPosition = c(3, 3.50))
assetsNIGFitPlot(x, oma = c(0,0,0,0), mar = c(4, 4, 4, 4), 
    keyOffset = c(-0.65, -0.50), main = "Assets Statistics", 
    title = "Assets", titlePosition = c(3, 3.65), 
    description = "NIG  Parameters", descriptionPosition = c(3, 3.50))
}


\arguments{
  
    \item{col}{
        a character string, defining the color to fill the boxes.
        }
    \item{description}{
        ...
        }
    \item{descriptionPosition}{
        ... 
        }
    \item{keyOffset}{
        ...
        }
    \item{labels}{
        a logical flag, if \code{TRUE} then default labels will be used,
        otherwise the plots will be displayed without labels and the user
        can add his own labels.
        }
    \item{main}{
        ...
        }
    \item{mar}{
        ...
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
    \item{oma}{
        ...
        }
    \item{percentage}{
        ...
        }
    \item{scale}{
        ...
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
    \item{titlePosition}{
        ... 
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
