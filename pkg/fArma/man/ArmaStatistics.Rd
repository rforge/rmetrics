\name{ArmaStatistics}

\alias{ArmaStatistics}

\alias{armaRoots}
\alias{armaTrueacf}


\title{Statistics of the True ARMA Process}


\description{

    A collection and description of functions 
    to compute statistics of a true ARMA time
    series process.
    \cr
        
    The functions are:
        
    \tabular{ll}{
    \code{armaRoots} \tab Roots of the characteristic ARMA polynomial, \cr
    \code{armaTrueacf} \tab True autocorrelation function of an ARMA process.}  
    
}
    

\usage{
armaRoots(coefficients, n.plot = 400, digits = 4, \dots) 
armaTrueacf(model, lag.max = 20, type = c("correlation", "partial", "both"), 
    doplot = TRUE) 
}


\arguments{
 
    \item{coefficients}{    
        [armaRoots] - \cr
        a numeric vector with the coefficients of the characterisitic 
        polynomial.
        } 
    \item{digits}{  
        [armaRoots] - \cr
        output precision, an integer value.
        }
    \item{doplot}{
        [armaRoots] - \cr
        a logical. Should a plot be displayed?
        }   
    \item{lag.max}{
        [armaTrueacf] - \cr
        maximum number of lags at which to calculate the acf or 
        pacf, an integer value by default 20.
        }
    \item{model}{
        [armaTrueacf] - \cr
        a specification of the ARMA model with two elements:
        \code{model$ar} is the vector of the AR coefficients, and 
        \code{model$ma} is the vector of the MA coefficients.
        }
    \item{n}{
        [armaSim] - \cr
        an integer value setting the length of the series to be simulated 
        (optional if \code{innov} is provided). The default value is 100.
        }
    \item{n.plot}{  
        [armaRoots] - \cr
        the number of data points to plot the unit circle;
        an integer value.
        }
    \item{type}{
        [armaTrueacf] - \cr
        a character string, "correlation" to compute the true 
        autocorrelation function, "partial" to compute the true
        partial autocorrelation function, or "both" if both
        functions are desired. The start of one of the strings 
        will suffice.
        } 
    \item{\dots}{
        additional arguments to be passed.
        }
        
}


\value{

    \code{armaRoots} 
    \cr
    returns a three column data frame with the real, the imaginary part 
    and the radius of the roots. The number of rows corresponds
    to the coefficients.
    
    \code{armaTrueacf}
    \cr
    returns a two column data frame with the lag and the correlation 
    function. 
    
}
               

\references{

Brockwell, P.J. and Davis, R.A. (1996); 
    \emph{Introduction to Time Series and Forecasting}, 
    Second Edition, Springer, New York. 
    
Durbin, J. and Koopman, S.J. (2001); 
    \emph{Time Series Analysis by State Space Methods},  
    Oxford University Press.
    
Gardner, G, Harvey, A.C., Phillips, G.D.A. (1980); 
    \emph{Algorithm AS154. An algorithm for exact maximum likelihood 
    estimation of autoregressive-moving average models by means of 
    Kalman filtering},
    Applied Statistics, 29, 311--322.
    
Hannan E.J. and Rissanen J. (1982);
    \emph{Recursive Estimation of Mixed Autoregressive-Moving 
    Average Order.}
    Biometrika 69, 81--94. 
    
Harvey, A.C. (1993); 
    \emph{Time Series Models},
    2nd Edition, Harvester Wheatsheaf, Sections 3.3 and 4.4.

Jones, R.H. (1980); 
    \emph{Maximum likelihood fitting of ARMA models to time
    series with missing observations},
    Technometrics, 20, 389--395.

Percival, D.P. and Walden, A.T. (1998); 
    \emph{Spectral Analysis for Physical Applications.} 
    Cambridge University Press.

Whittle, P. (1963); 
    \emph{On the fitting of multivariate autoregressions
    and the approximate canonical factorization of a spectral 
    matrix.}
    Biometrika 40, 129--134.
    
Haslett J. and Raftery A.E. (1989);
    \emph{Space-time Modelling with Long-memory Dependence: Assessing 
    Ireland's Wind Power Resource (with Discussion)},
    Applied Statistics 38, 1--50.
    
}


\author{

    M. Plummer and B.D. Ripley for \code{ar} functions and code,\cr
    B.D. Ripley for \code{arima} and \code{ARMAacf} functions and code, \cr
    C. Fraley and F. Leisch for \code{fracdiff} functions and code, and \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## armaRoots -
   # Calculate and plot the roots of an ARMA process:
   par(mfrow = c(2, 2), cex = 0.7)
   coefficients = c(-0.5, 0.9, -0.1, -0.5)
   armaRoots(coefficients)
   
## armaTrueacf -
   model = list(ar = c(0.3, +0.3), ma = 0.1)
   armaTrueacf(model)
   model = list(ar = c(0.3, -0.3), ma = 0.1)
   armaTrueacf(model)
}


\keyword{models}

