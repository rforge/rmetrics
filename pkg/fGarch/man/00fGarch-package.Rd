\name{fGarch-package}


\alias{fGarch-package}
\alias{fGarch}


\docType{package}


\title{Modelling Heterskedasticity in Financial Time Series}


\description{

  The Rmetrics "fGarch" package is a collection of functions to 
  analyze and model heteroskedastic behavior in financial time
  series models.
  .
}


\details{

    \tabular{ll}{
        Package:    \tab fGarch \cr
        Type:       \tab Package\cr
        Version:    \tab R 3.0.1\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Assiciation\cr
        URL:        \tab \url{https://www.rmetrics.org}
  }
  
}


\section{1 Introduction}{

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


\section{2 Time Series Simulation}{

    contains functions to simulate artificial GARCH and APARCH
    time series processes.
    
    \preformatted{
    garchSpec       specifies an univariate GARCH time series model 
    garchSim        simulates a GARCH/APARCH process
    }
  
}


\section{3 Parameter Estimation}{

    contains functions to fit the parameters of GARCH and APARCH 
    time series processes.
    
    \preformatted{
    garchFit} \tab Fits the parameters of a GARCH process
    }
    
    
    \emph{Extractor Functions:}
    
    \preformatted{
    residuals       extracts residuals from a fitted 'fGARCH' object
    fitted          extracts fitted values from a fitted 'fGARCH' object
    volatility      extracts conditional volatility from a fitted 'fGARCH' object
    coef            extracts coefficients from a fitted 'fGARCH' object
    formula         extracts formula expression from a fitted 'fGARCH' object
    }
    
}


\section{4 Forecasting}{

    contains functions to forcecast mean and variance of GARCH and
    APARCH processes.
  
    \preformatted{
    predict         forecasts from an object of class 'fGARCH'
    }
  
}


\section{5 Standardized Distribution Functions}{

    This section contains functions to model standardized distribution 
    functions.
    
    
    \emph{Skew Normal Distribution:}
    
    \preformatted{
    [dpqr]norm      Normal distribution function
    [dpqr]snorm     Skew Normal distribution function
    [s]normFit      fits parameters of [skew] Normal distribution
    }
    
    
    \emph{Skew Generalized Error Distribution:}
    
    \preformatted{
    [dpqr]ged       Generalized Error distribution function
    [dpqr]sged      Skew Generalized Error  distribution function
    [s]gedFit       fits parameters of [skew] Generalized Error distribution
    }
    
    
    \emph{Skew Standardized Student-t Distribution:}
    
    \preformatted{
    [dpqr]std       Standardized Student-t distribution function
    [dpqr]sstd      Skew standardized Student-t distribution function
    [s]stdFit       fits parameters of [skew] Student-t distribution
    }
    
    \emph{Abdolute Moments:}
    \preformatted{
    absMoments      computes absolute Moments of these distribution
    } 
  
}


\section{About Rmetrics:}{

    The \code{fImport} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


%  Diethelm Wuertz and Yohan Chalabi, with contributions from 
%  Michal Miklovac, Pierre Chausse, and Chris Boudt.


\keyword{package}


