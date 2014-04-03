\name{fArma-package}


\alias{fArma-package}
\alias{fArma}


\docType{package}


\title{Modelling ARMA Time Series Processes}


\description{

    The Rmetrics "fArma" package is a collection of functions to 
    analyze and model ARMA time series processes which special emphesis
    in Finance.
    
}


\details{

    \preformatted{
        Package:    \tab fArma\cr
        Type:       \tab Package\cr
        Version:    \tab R 3.0.1\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Assiciation\cr
        URL:        \tab \url{https://www.rmetrics.org}
    }
    
}


\section{1 Introduction}{

    The 'fARMA' package is a collection of functions to analyze, to
    simulate, to fit parameteres, and to forecast ARMA model and long
    range dependency of fincnaial time series models. 

}


\section{2 ARMA Modeling Interface}{

    The section provides a collection simple to use functions to 
    model univariate autoregressive moving average time series 
    processes, including time series simulation, parameter estimation, 
    diagnostic analysis of the fit, and predictions of future values.

    \preformatted{
    armaSim         simulates an artificial ARMA time series process
    armaFit         fits the parameters of an ARMA time series process
    }
    
    
    \emph{Extractor Functions:}
    
    \preformatted{
    fitted          returns fitted values
    coef            returns coefficients
    residuals       returns residuals
    }
    
    \emph{Forecasting Function:}
    
    \preformatted{
    predict         forecasts and optionally plots ARMA processes
    }
    
    
    \emph{Generic print, plot and summary functions:}
    
    \preformatted{
    print           print method
    plot            plot method
    summary         summary method
    }
  
}


\section{3 Statistics of the True ARMA Process}{

    Here we provide two functions to compute the statistics of a true 
    ARMA time series process.
    
    \preformatted{
    armaRoots       roots of the characteristic ARMA polynomial
    armaTrueacf     true autocorrelation function of an ARMA process
    }

}


\section{4 Long Range Dependence Modelling}{

    This is a collection and description of functions to investigate 
    the long range dependence or long memory behavior of an univariate 
    time series process. Included are functions to simulate fractional 
    Gaussian noise and fractional ARMA processes, and functions to 
    estimate the Hurst exponent by several different methods.
    
    \emph{Functions to simulate long memory time series processes:}
    
    \preformatted{
    fnmSim          simulates fractional Brownian motion
    - mvn           from the numerical approximation of the stochastic integral
    - chol          from the Choleski's decomposition of the covariance matrix
    - lev           using the method of Levinson
    - circ          using the method of Wood and Chan
    - wave          using the wavelet synthesis
    }
    
    \preformatted{
    fgnSim          simulates fractional Gaussian noise
    - beran         using the method of Beran
    - durbin        using the method Durbin and Levinson
    - paxson        using the method of Paxson
    }
    
    \preformatted{
    farimaSim       simulates FARIMA time series processes
    }
        
    
    \emph{Functions to estimate the Hurst exponent:}
    
    \preformatted{
    aggvarFit       aggregated variance method
    diffvarFit      differenced aggregated variance method
    absvalFit       aggregated absolute value (moment) method
    higuchiFit      Higuchi's or fractal dimension method
    pengFit         Peng's or variance of residuals method
    rsFit           R/S Rescaled Range Statistic method
    perFit          periodogram method
    boxperFit       boxed (modified) periodogram method
    whittleFit      Whittle estimator
    hurstSlider     interactive Display of Hurst Estimates
    }
    
    
    \emph{Function for the wavelet estimator:}
    
    \preformatted{
    waveletFit      wavelet estimator
    }
    
}


\section{5 LRD True Statistics}{

    This section provides two sets of functions functions to investigate 
    the true statistics of the long range dependence or long memory behavior 
    of univariate FGN or FARIMA time series processes. 
    \cr
    
    
    \emph{FGN Models:}
    \preformatted{
    fgnTrueacf            returns true FGN covariances
    fgnTruefft            returns true FGN fast Fourier transform
    fgnStatsSlider        returns a plot of true FGN Statistics
    }
    
    
    \emph{FARIMA Models:}
    \preformatted{
    farimaTrueacf         returns true FARIMA covariances
    farimaTruefft         returns true FARIMA fast Fourier transform
    farimaStatsSlider     returns a plot of true FARIMA Statistics
    }
    
}


\section{About Rmetrics:}{

    The \code{fArma} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}


