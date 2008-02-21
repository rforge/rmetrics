\name{fBasics-package}


\alias{fBasics}


\docType{package}


\title{Portfolio Modelling, Optimization and Backtesting}


\description{

    The Rmetrics "fbasics" package is a collection of functions to 
    explore and to investigate basic properties of financial returns 
    and related quantities.
    
    The covered fields include techniques of explorative data analysis
    and the investigation of distributional properties, including
    parameter estimation and hypothesis testing.
    
    For \emph{Explorative Data Anlysis} several plot functions are available
    to explore the time series themselves, to show their distributional 
    properties, and to encover correlations and dependencies.
    
    Functions to compute \emph{Distributional Properties} of financial 
    returns and derivated and related series are enclosed. The in detail 
    considered distributions include the normal, the student-t, the
    stable, the family of generalised hyperbolic, and max drawdown
    distributions. Moment and log-likelihood estimators allow to
    estimate the distributional parameters. Functions to compute moments 
    and modes ar also avialable.
    
    The functions from \emph{Hypothesis Testing} one sample and two sample
    tests. Most of the one sample tests deal with testing normality. The
    two sample tests allow to compare two series to find out if the series
    are correlated or their distributions have the same distributional
    parameters.
    
}


\details{

    \tabular{ll}{
        Package:    \tab fBasics\cr
        Type:       \tab Package\cr
        Version:    \tab 270.73\cr
        Date:       \tab 2008\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2008 Diethelm Wuertz and Rmetrics Foundation\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }
    
}


\section{Overview:}{

    The following chapters give a brief introduction how to optimize
    and analyze portfolios.
    
    1. Explorative Data Analysis\cr
    2. Distributional Properties\cr
    3. Hypthesis Testing
    
}

   
\section{1. Explorative Data Analysis}{

    Explorative data analysis of financial return series and related 
    series 
    
    
    Exploratory Data Analysis is an approach for data analysis that 
    employs a variety of techniques most of graphical nature to
    maximize insight into a data set, to uncover underlying structures, 
    and to detect outliers and anomalies. For this several functions
    are implemented to plot the series, to plot the distribution from
    different views, and to visualize correlations and dependencies.

    \tabular{ll}{
    \bold{Time Series Plots:} \tab \cr
    \code{seriesPlot} \tab Returns a tailored return series plot, \cr
    \code{cumulatedPlot} \tab returns a cumulated series given the returns, \cr
    \code{returnPlot} \tab returns returns given the cumulated series, \cr
    \code{drawdownPlot} \tab returns drawdowns given the return series, \cr
    \tab \cr
    \bold{Density Plots:} \tab \cr
    \code{histPlot} \tab Returns a tailored histogram plot, \cr
    \code{densityPlot} \tab returns a tailored kernel density estimate plot, \cr
    \code{logDensityPlot} \tab returns a tailored log kernel density estimate plot, \cr
    \code{qqPlot} \tab returns a quantile-quantile plot, \cr
    \code{scalinglawPlot} \tab returns a scaling law plot, \cr
    \tab \cr    
    \bold{Box Plots:} \tab \cr
    \code{boxPlot} \tab Returns a side-by-side standard box plot, \cr
    \code{boxPercentilePlot} \tab Returns a side-by-side box-percentile plot, \cr
    \tab \cr
    \bold{CorrelationPlots:} \tab \cr
    \code{acfPlot} \tab autocorrelation function plot, \cr
    \code{pacfPlot} \tab partial autocorrelation function plot, \cr
    \code{lacfPlot} \tab lagged autocorrelation function plot, \cr
    \code{teffectPlot} \tab Taylor effect plot. }
        
}


\section{2. Distributional Properties}{
    
    The distributional properties can be investigate for several types
    of distribution functions which are important in the investigation
    of financial returns and related time series.
    
    %\emph{Normal Student-t Distribution}
    
    %The normal distribution, definitely the most important distribution
    %in finance, is part of R's base package. We have added the function
    %\code{[dpqr]normal which has added an additional skewness parameter.
    %In the case of the Student t distribution we have added a version 
    %named \code{[dpqr]student} which is standardized, and which allows
    %for an additional location, scale, and skewness parameter in constrast
    %to the function \code{[dpqr]t} from R's base package.
    
    
    \emph{Stable and Skew-Stable Distribution}
    
    
    \tabular{ll}{
    \code{[dpqr]stable} \tab the stable and skew-stable distribution, \cr
    \code{stableMode} \tab the stable and skew-stable mode, \cr
    \code{stableSlider} \tab interactive stable distribution display. }

    
    \emph{The family of Generalized Hyperbolic Distributions}
    
    \tabular{ll}{
    \bold{GH:} \tab \cr
    \code{[dpqr]gh} \tab GH, Generlized hyperbolic distribution, \cr
    \code{ghFit} \tab GH parameter estimation, \cr
    \code{ghMode} \tab mode of the GH distribution, \cr
    \tab \cr
    \bold{HYP:} \tab \cr
    \code{[dpqr]hyp} \tab HYP, hyperbolic distribution, \cr
    \code{hypFit} \tab HYP parameter estimation, \cr
    \code{hypMode} \tab mode of the hyperbolic distribution, \cr
    \code{hypSlider} \tab HYP distribution Slider, \cr
    \tab \cr
    \bold{NIG:} \tab \cr
    \code{[dpqr]nig} \tab NIG, hyperbolic distribution, \cr
    \code{nigFit} \tab NIG parameter estimation, \cr
    \code{nigMode} \tab mode of the NIG distribution, \cr
    \code{nigSlider} \tab NIG distribution Slider, \cr
    \tab \cr
    \bold{GHT:} \tab \cr
    \code{[dpqr]ght} \tab GHT, generalized hyperbolic Student-t, \cr
    \code{ghtFit} \tab GHT parameter estimation, \cr
    \code{ghtMode} \tab mode of the GHT distribution, \cr
    \code{ghtSlider} \tab GHT distribution Slider.}



        
    \emph{The max-Drawdown Distribution}

    The functions compute compute drawdown statistics. 
    Included are density, distribution function,  
    and random generation for the maximum drawdown 
    distributions. In addition the expectation of 
    drawdowns for Brownian motion can be computed.
    
    \tabular{ll}{
    \code{[dpr]maxdd} \tab the max drawdown distribution, \cr
    \code{maxddStats} \tab the expectation of drawdowns. }
    
}


\section{3. Hypthesis Testing}{

    \emph{One Sample Normality Tests}
   
        normality Tests
    
    \emph{Two Sample Tests}
    
        correlationTest
        
        locationTest
        
        varianceTest
        
        scaleTest ?
    
}
   

\note{

    With Rmetrics version 2.7.0 ...
    
}
    

\keyword{models}

