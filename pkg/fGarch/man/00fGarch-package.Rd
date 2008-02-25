\name{fGarch-package}

\alias{fGarch-package}
\alias{fGarch}

\docType{package}

\title{GARCH Modelling Package}


\description{

  Package of econometric functions for modelling GARCH processes.
}


\details{

  \tabular{ll}{
    Package:    \tab fGarch \cr
    Type:       \tab Package \cr
    Version:    \tab 270.73 \cr
    Date:       \tab 2008 \cr
    License:    \tab GPL (>= 2) \cr
    Copyright:  \tab (c) 1999-2008 Diethelm Wuertz and Rmetrics
    Foundation \cr
    URL:        \tab \url{http://www.rmetrics.org}
  }
  
  
  GARCH, Generalized Autoregressive Conditional Heteroskedastic, models 
  have become important in the analysis of time series data, particularly 
  in financial applications when the goal is to analyze and forecast 
  volatility. 
  
  For this purpose, the family of GARCH functions offers functions for 
  simulating, estimating and forecasting various univariate GARCH-type 
  time series models in the conditional variance and an ARMA specification 
  in the conditional mean. The function \code{garchFit} is a numerical
  implementation of the maximum log-likelihood approach under different 
  assumptions, Normal, Student-t, GED errors or their skewed versions. 
  The parameter estimates are checked by several diagnostic analysis tools 
  including graphical features and hypothesis tests. Functions to compute 
  n-step ahead forecasts of both the conditional mean and variance are also
  available.

  The number of GARCH models is immense, but the most influential models 
  were the first. Beside the standard ARCH model introduced by Engle [1982] 
  and the GARCH model introduced by Bollerslev [1986], the function
  \code{garchFit} also includes the more general class of asymmetric power
  ARCH models, named APARCH, introduced by Ding, Granger and Engle [1993]. 
  The APARCH models include as special cases the TS-GARCH model of 
  Taylor [1986] and Schwert [1989], the GJR-GARCH model of Glosten, 
  Jaganathan, and Runkle [1993], the T-ARCH model of Zakoian [1993], the 
  N-ARCH model of Higgins and Bera [1992], and the Log-ARCH model of
  Geweke [1986] and Pentula [1986].
  
  There exist a collection of review articles by Bollerslev, Chou and 
  Kroner [1992], Bera and Higgins [1993], Bollerslev, Engle and
  Nelson [1994], Engle [2001], Engle and Patton [2001], and Li, Ling 
  and McAleer [2002] which give a good overview of the scope of the 
  research.\cr

}


\section{Time Series Simulation}{

  contains functions to simulate artificial GARCH and APARCH
  time series processes.
  
  Functions:
  
  \tabular{ll}{
    \code{garchSpec} \tab Specifies an univariate GARCH time series
    model, \cr 
    \code{garchSim} \tab Simulates a GARCH/APARCH process. }
  
}


\section{Parameter Estimation}{

  contains functions to fit the parameters of GARCH and APARCH 
  time series processes.
  
  Functions:
  
  \tabular{ll}{
    \code{garchFit} \tab Fits the parameters of a GARCH process, \cr 
    \code{residuals} \tab Extracts residuals from a fitted 'fGARCH'
    object, \cr 
    \code{fitted} \tab Extracts fitted values from a fitted 'fGARCH'
    object, \cr 
    \code{volatility} \tab Extracts conditional volatility from a fitted
    'fGARCH' object, \cr 
    \code{coef} \tab Extracts coefficients from a fitted 'fGARCH'
    object, \cr 
    \code{formula} \tab Extracts formula expression from a fitted
    'fGARCH' object. } 
}


\section{Forecasting}{

  contains functions to forcecast mean and variance of GARCH and
  APARCH processes.
  
  Functions:
  
  \tabular{ll}{
    \code{predict} \tab Forecasts from an object of class 'fGARCH'.
  }
  
}


\section{Standardized Distribution Functions}{

  contains functions to model standardized distribution functions.
  
  Functions:
  
  \tabular{ll}{
    \code{[dpqr]norm} \tab Normal distribution function, \cr 
    \code{[dpqr]snorm} \tab Skew Normal distribution function, \cr 
    \code{[s]normFit} \tab Fits parameters of [skew] Normal
    distribution, \cr 
    \code{[dpqr]ged} \tab Generalized Error distribution function, \cr 
    \code{[dpqr]sged} \tab Skew Generalized Error  distribution
    function, \cr 
    \code{[s]gedFit} \tab Fits parameters of [skew] Generalized Error
    distribution, \cr 
    \code{[dpqr]std} \tab standardized Student-t distribution function,
    \cr 
    \code{[dpqr]sstd} \tab Skew standardized Student-t distribution
    function, \cr 
    \code{[s]stdFit} \tab Fits parameters of [skew] Student-t
    distribution, \cr 
    \code{absMoments} \tab Computes absolute Moments of these
    distribution.
  } 
  
}


\section{OX Interface}{

  contains a Windows interface to OX.
  
  The function \code{garchOxFit} interfaces a subset of the functionality 
  of the G@ARCH 4.0 Package written in Ox. 
  G@RCH 4.0 is one of the most sophisticated packages for modelling 
  univariate GARCH processes including GARCH, EGARCH, GJR, APARCH, 
  IGARCH, FIGARCH, FIEGARCH, FIAPARCH and HYGARCH models. Parameters
  can be estimated by approximate (Quasi-) maximum likelihood methods
  under four assumptions: normal, Student-t, GED or skewed Student-t 
  errors.
  
}


\author{

  Diethelm Wuertz and Rmetrics Core Team.
  
}


\keyword{package}

