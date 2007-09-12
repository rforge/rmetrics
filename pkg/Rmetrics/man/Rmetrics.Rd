\name{Rmetrics}

\alias{Rmetrics}


\title{Rmetrics Software Collection}


\description{
    
    A collection and description of functions for
    teaching computational finance and financial 
    engineering.
    \cr
    
}


\details{

    Some of the Rmetrics packages have become so huge that it is
    for many Rmetrics programmers a problem to find out where
    to search for a desired function. So w started to split the 
    biggest packages in smaller ones.\cr
    
    So far Rmetrics (<= R-2.5.1) has packaged functions according 
    to a quite simple subject classification scheme. We have 
    extended it a litte bit, and now this scheme comes with the 
    following topics:

    A) DATA SETS AND UTILIY FUNCTIONS\cr
    B) CHRONOLOGICAL OBJECTS, FINANCIAL CENTERS AND TIME SERIES\cr
    C) ANALYSIS OF FINANCIAL RETURNS AND VOLATILITY\cr
    D) TECHNICAL ANALYSIS, DECISION MAKING AND PREDICTION\cr
    E) EXTREME VALUE THEORY, COPULAE AND RISK MANAGEMENT\cr
    F) VALUATION OF FINANCIAL INSTRUMENTS\cr
    G) ASSET MANAGEMENT, PORTFOLIO ANALYSIS AND OPTIMIZATION\cr

    We think, this scheme starting with R-2.6.0, will help programmers 
    and developers to search faster around for functions in the Rmetrics 
    packages. 
    
    
    Changes to previous Rmetrics Packages:
    
    Concerning A)\cr
        We splitted the package \code{fEcofin} in two packages:\cr
    \code{fEcofin}
        now holds all economic and financial data sets, and\cr
    \code{fUtilities}
        now holds all kinds of utility functions and general tools 
        which are needed for Rmetrics (most as internal functions).

    Concerning B)\cr
        We splitted the package \code{fCalendar} now into three 
        packages:\cr
    \code{fCalendar} 
        now holds \code{timeDate} class, zone and DST 
        information, and everything what has to do with Holidays and 
        Holiday Calendars,\cr
    \code{fSeries} 
        now holds everything about \code{timeSeries} 
        class,\cr
    \code{fImport} 
       now holds download functions for several web data
       bases, like Yahoo, Economagic, Federal Reserve, and others.

    Concerning C)\cr
    \code{fBasics} 
        still holds everything about financial returns,
        stable and hyperbolic distributions, distributional fits,
        stylized facts, and hypothesis tests.
        The content of the former \code{fSeries} package has moved 
        to four new packages\cr
    \code{fArma} 
        holds linear and long range time series models,
        fractional ARMA and fractional Brownian Nois/Motion,
        mainly wrapper functions allowing for timeSeries objects
        and providing easy to use,\cr
    \code{fGarch} 
        deals with volatility modeling using ARCH, GARCH,
        APARCH and related heteroskedastic models, also providing
        an interface to OXGarch (Windows only)\cr
    \code{fNonlinear} 
        is thought mainly for nonlinearity models, 
        chaos and nonlinearity tests, e.g. like BDS,\cr
    \code{fUnitRoots} deals with time series trends and unit
        root testing, implementing MacKinnon's pValues and
        interfacing Pfaff's urca package.

    Concerning D)\cr
        The previous fMultivar package has been splitted now into
        four packages:\cr
    \code{fMultivar} 
        now contains bivariate and multivariate return
        distributions and tools like bivariate binning and gridding,
        interpolation,\cr
    \code{fTrading} 
        deals with technical analysis, benchmark analysis
        and rolling analysis,\cr
    \code{fRegression} 
        holds convenient wrappers allowing for
        timeSeries objects to be modeled by regression methods
        for building trading systems, for decision making and
        prediction)

    Concering E)\cr
        Nothing has changed  so far.\cr
    \code{fExtremes} 
        deals with GEV and GPD modeling, the extremal
        index, and Risk estimation,\cr
    \code{fCopulae} 
        has functions for bivariate (only) copulae
        including elliptical, archimedean, extreme value, mixed,
        and empirical copulae, tail dependency estimators.

    Concerning F)\cr
        The huge fOptions package has been splitted into three
        new packages:\cr
    \code{fOptions} 
        holds the option basics, like Black-Scholes,
        Heston-Nandi, Binomial Trees, American Option Approximations,
        low discrepancy sequences and Monte Carlo valuation of
        options,\cr
    \code{fExoticOptions} 
        has pricing formulas for dozens of exotic
        European options\cr
    \code{fAsianOptions} 
        is made for Exponential Brownian Motion for
        (exact) pricing formulas of Asian Options including
        moment matching methods, Gram-Charlier Series Expansion,
        PDE pricing, Laplace Inversion approach, Spectral
        Expansion approach, and symmetry relations.\cr  
    \code{Bonds} 
        has been started up currently with functions for
        yield curve modeling.

    Concerning G)\cr
        The package fPortfolio has been splitted into new 
        packages:\cr
    \code{fAssets} 
        deals with many aspects of asset selection and
        asset analysis, including robust statistics, clustering,
        correlation analysis, lower partial moments, etc.\cr
    \code{fPortfolio} 
        is made for design and optimization of
        portfolios using either the standard Markowitz or the
        Conditional Value-at-Risk approach, included are
        functions for rolling efficient portfolios and frontiers.

}


\keyword{programming}

