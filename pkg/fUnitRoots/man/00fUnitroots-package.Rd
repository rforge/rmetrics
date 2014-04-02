\name{fUnitRoots-package}


\alias{fUnitRoots-package}
\alias{fUnitRoots}


\docType{package}


\title{Modelling Trends and Unit Roots}


\description{

    The Rmetrics "fUnitRoots" package is a collection of functions 
    to model trends and to analyze unit roots. 
    
}


\details{

    \preformatted{
        Package:    \tab fUnitRoots\cr
        Type:       \tab Package\cr
        Version:    \tab R 3.0.1\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Association\cr
        Repository: \tab R-FORGE\cr
        URL:        \tab \url{https://www.rmetrics.org}
    }
    
}


\section{1 Introduction}{

    The 'fUnitroots' provides four addons for analyzing trends and
    unit roots in financial time series: (i) functions for the density
    and probability of the augmented Dickey-Fuller Test, (ii) functions 
    for the density and probability of MacKinnon's unit root test 
    statistics, (iii) reimplementations for the ADF and MacKinnon
    Test, and (iv) an 'urca' Unit Root Test Interface for Pfaff's
    unit root test suite.
}


\section{2 Dickey-Fuller p Values}{

    The section provides functions to compute the distribution and 
    quantile functions for the ADF unit root test statistics. 
    
    \preformatted{
    padf            returns the cumulative probability for the ADF test
    qadf            returns the quantiles for the ADF test
    adfTable        tables p values for ADF test
    }
    
}


\section{3 Mc Kinnon p Values}{

    The section provides functions to compute the distribution and 
    quantile functions for MacKinnon's unit root test statistics. 
    
    \preformatted}{
    punitroot       returns the cumulative probability
    qunitroot       returns the quantiles of the unit root test statistics
    unitrootTable   tables p values from MacKinnon's response surface
    }
    
}


\section{4 Unit Root Time Series Tests}{
    
    This section provides two functions for unit root testing of financial
    time series, the ADF tests based on Banerjee's et al. tables and the
    unit root tests based on J.G. McKinnons' tables:
    
    \preformatted{}
    adfTest         augmented Dickey--Fuller test for unit roots
    unitrootTest    the same based on McKinnons's test statistics
    }

}


\section{5 "urca"" Unit Root Test Interface}{

    This is an interface to the unitroot tests suite implemented by 
    Bernhard Pfaff available through the \R package "\pkg{urca}" 
    
    \preformatted{
    urdfTest        Augmented Dickey--Fuller test for unit roots
    urersTest       Elliott--Rothenberg--Stock test for unit roots
    urkpssTest      KPSS unit root test for stationarity
    urppTest        Phillips--Perron test for unit roots
    urspTest        Schmidt--Phillips test for unit roots
    urzaTest        Zivot--Andrews test for unit roots
    }
  
}


\section{About Rmetrics:}{

    The \code{fImport} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}