\name{risk-surfaceRisk}


\alias{markowitzHull}
\alias{feasibleGrid}
\alias{bestDiversification}
\alias{riskSurface}
\alias{surfacePlot}


\title{Surface Risk Analytics}


\description{
  
    Functions for surface risk analytics.
    
}


\usage{
markowitzHull(data, nFrontierPoints=50)
feasibleGrid(hull, trace=FALSE)
bestDiversification(grid, FUN="var", trace=FALSE)
riskSurface(diversification, FUN=NULL, \dots)

surfacePlot(surface, type=c("image", "filled.contour"), nlevels=11, 
    palette=topo.colors, addContour=TRUE, addGrid=TRUE, addHull=TRUE, 
    addAssets=TRUE, \dots)
}


\arguments{

    \item{data}{data}
    \item{hull}{hull}
    \item{surface}{surface}
    \item{diversification}{diversification}
    \item{FUN}{FUN}
    
    \item{grid}{grid}
    \item{nFrontierPoints}{nFrontierPoints}
    \item{trace}{trace}
    \item{type}{type}
    \item{nlevels}{nlevels}
    \item{palette}{palette}
    \item{addContour}{addContour}
    \item{addGrid}{addGrid}
    \item{addHull}{addHull}
    \item{addAssets}{addAssets}
    \item{\dots}{optional arguments}

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}


