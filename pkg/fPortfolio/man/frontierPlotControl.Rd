\name{frontierPlotControl}


\alias{frontierPlotControl}


\title{Frontier Plot Control List}


\description{

    Allows to modify plot settings for the frontier plot.

}


\usage{
frontierPlotControl(   
    # Colors:
        sharpeRatio.col   = "blue",
        minvariance.col   = "red",
        tangency.col      = "steelblue",
        cml.col           = "green",
        equalWeights.col  = "blue",
        singleAsset.col   = "topo.colors",
        twoAssets.col     = "grey",
        monteCarlo.col    = "black",
    # Point Sizes:
        minvariance.cex   = 1.25,
        tangency.cex      = 1.25,
        cml.cex           = 1.25,
        equalWeights.cex  = 1.25,
        singleAsset.cex   = 1.25,
        twoAssets.cex     = 0.01,
        monteCarlo.cex    = 0.01,
        sharpeRatio.cex   = 0.1, 
    # Limits:
        xlim              = NULL,
        ylim              = NULL,
    # MC Steps:
        mcSteps           = 5000,
    # Pie Settings:
        pieR              = NULL, 
        piePos            = NULL, 
        pieOffset         = NULL)
}


\arguments{

    \item{sharpeRatio.col}{Color setting.}
    \item{minvariance.col}{Color setting.}
    \item{tangency.col}{Color setting.}
    \item{cml.col}{Color setting.}
    \item{equalWeights.col}{Color setting.}
    \item{singleAsset.col}{Color setting.}
    \item{twoAssets.col}{Color setting.}
    \item{monteCarlo.col}{Color setting.}
    
    \item{minvariance.cex}{Font point size setting.}
    \item{tangency.cex}{Font point size setting.}
    \item{cml.cex}{Font point size setting.}
    \item{equalWeights.cex}{Font point size setting.}
    \item{singleAsset.cex}{Font point size setting.}
    \item{twoAssets.cex}{Font point size setting.}
    \item{monteCarlo.cex}{Font point size setting.}
    \item{sharpeRatio.cex}{Font point size setting.} 

    \item{xlim}{x-axis limit setting.}
    \item{ylim}{y-axis limit setting.}
    
    \item{mcSteps}{Numer of Monte Carlo steps.}

    \item{pieR}{Pie radius setting.}
    \item{piePos}{Pie position coordinates setting.}
    \item{pieOffset}{Pie offset coordinates setting.}
    
}


%\details{}



\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## --
}


\keyword{models}

