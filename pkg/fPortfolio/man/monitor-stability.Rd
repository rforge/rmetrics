\name{monitor-stability}


\alias{stabilityAnalytics}
\alias{turnsAnalytics}
\alias{drawdownsAnalytics}
\alias{garchAnalytics}
\alias{riskmetricsAnalytics}
\alias{bcpAnalytics}
\alias{pcoutAnalytics}
\alias{addRainbow}
\alias{waveletSpectrum}
\alias{parAnalytics}


\title{Monitoring Stability}


\description{
  
    Functions for time series aggregation, converting a time series
    from a daily to a monthly or weekly base.
    
}


\usage{
stabilityAnalytics(index, method=c("turns", "drawdowns", "garch", 
    "riskmetrics", "bcp", "pcout"), \dots)
    
turnsAnalytics(index, spar=0.5, main=NULL, 
    trace=TRUE, doplot=TRUE, at=pretty(index), format="\%m/\%y")
drawdownsAnalytics(index, spar=0.5, main=NULL, 
    trace=TRUE, doplot=TRUE, at=pretty(index), format="\%m/\%y")
garchAnalytics(index, spar = 0.5, main=NULL, 
    trace=TRUE, doplot=TRUE, at=pretty(index), format="\%m/\%y")    
riskmetricsAnalytics(index, spar=0.5, lambda=0.9, main=NULL, 
    trace=TRUE, doplot=TRUE, at=pretty(index), format="\%m/\%y")
bcpAnalytics(index, spar=0.5, FUN=returns, method=c("prob", "mean", "var"),
    main=NULL, trace=TRUE, doplot=TRUE, at=pretty(index), format="\%m/\%y")    
pcoutAnalytics(index, spar=0.5, main=NULL, trace=TRUE, doplot=TRUE, 
    at=pretty(index), format="\%m/\%y", strong=TRUE, k=2, cs=0.25, outbound=0.25)     
    
addRainbow(analytics, palette=rainbow, a=0.3, b=0.8, K=100)    

waveletSpectrum(index, spar=0.5, main=NULL, trace=TRUE, doplot=TRUE, 
  at=pretty(index), format="\%m/\%y")
  
parAnalytics()
}


\arguments{

    \item{index}{an object of class 'timeSeries'}
    \item{method}{name of selected analytics}
    \item{analytics}{analytics object}
    \item{\dots}{optional arguments}
    
    \item{spar}{0.5}
    \item{main}{""}
    \item{trace}{TRUE}
    \item{doplot}{TRUE}
    \item{at}{pretty()}
    \item{format}{"\%m/\%y"}
    
    \item{lambda}{riskmetricsAnalytics}
    \item{bcp}{bcpAnalytics}
    \item{FUN,strong,k,cs,outbound}{pcoutAnalytics}
    \item{palette,a,b,K}{addRainbow}

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}

