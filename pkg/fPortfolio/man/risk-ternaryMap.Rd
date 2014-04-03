\name{risk-ternaryMap}


\alias{ternaryMap}
\alias{ternaryFrontier}
\alias{riskMap}
\alias{maxddMap}
\alias{ternaryWeights}
\alias{ternaryCoord}
\alias{ternaryPoints}


\title{Creates and Plots a Ternary Map}


\description{
  
    Functions for craeting and plotting ternary maps.
    
}


\usage{
ternaryMap(data, FUN=NULL, ..., 
  locator=FALSE, N=41, palette=topo.colors, nlevels=11) 
ternaryFrontier(data, locator=FALSE)  

riskMap(data, weights)
maxddMap(data, weights)

ternaryWeights(n=21)
ternaryCoord(weights)
ternaryPoints(weights, \dots)
}



\arguments{

    \item{data}{data}
    \item{weights}{weights}
    
    \item{FUN, locator, N, palette, nlevels}{ternaryMap}
    \item{n}{n}
    \item{\dots}{optional arguments}

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}

