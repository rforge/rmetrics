\name{garchSim}

\alias{garchSim}

\title{Univariate GARCH/APARCH Time Series Simulation}


\description{

    Simulates a univariate GARCH/APARCH time series model.

}


\usage{
garchSim(spec = garchSpec(), n = 100, n.start = 100, extended = FALSE) 
}


\arguments{

    \item{extended}{
        logical parameter if the output series should be a 3 columns
        \code{timeSeries} object with \code{garch}, \code{sigma} and
        \code{eps} data (\code{extended = TRUE}) or a univariate GARCH\\APARCH time
        series (\code{extended = FALSE}
        }
    \item{spec}{
        a specification object of class \code{"fGARCHSPEC"} as
        returned by the function \code{garchSpec}. The model parameters
        are taken from the \code{@model} slot, a list with the 
        following entries:\cr
        \code{omega} - the constant coefficient of the variance equation,
        by default 1e-6; \cr
        \code{alpha} - the value or vector of autoregressive coefficients, 
            by default 0.1, specifying a model of order 1; \cr
        \code{beta} - the value or vector of variance coefficients,
            by default 0.8, specifying a model of order 1; 
        \cr
        The optional values for the linear part are: \cr
        \code{mu} - the mean value, by default 0; \cr
        \code{ar} - the autoregressive ARMA coefficients, by default 0; \cr
        \code{ma} - the moving average ARMA coefficients, by default 0. 
        \cr
        The optional parameters for the conditional distributions are:\cr
        \code{skew} - the skewness parameter (also named xi), by default
            0.9, effective only for the \code{"dsnorm"}, the \code{"dsged"},
            and the \code{"dsstd"} skewed conditional distributions; \cr
        \code{shape} = the shape parameter (also named nu), by default 2 
            for the \code{"dged"} and \code{"dsged"}, and by default 4
            for the \code{"dstd"} and \code{"dsstd"} conditional
            distributions.\cr
        See also below for further details.
        }   
    \item{n}{
        length of output series, an integer value. An integer value,
        by default \code{n=100}.
        }
    \item{n.start}{
        length of "burn-in" period, by default 100.
        }

}


\details{

    The function \code{garchSim} simulates an univariate GARCH or 
    APARCH time series process as specified by the argument \code{model}. 
    
    The \code{model} is an object of class \code{"fGARCHSPEC"} as 
    returned by the function \code{garchSpec}. The returned model
    specification comes comes with a slot \code{@model} which is
    a list of just the numeric parameter entries. These are recognized 
    and extracted for use by the function \code{garchSim}.
    
    By default the series will be returned as an object of class
    \code{"ts"} or as a \code{"numeric"} vector. Having time/date
    positions, e.g. from an empirical process the numeric vector
    can be easily transformed into other time series objects like 
    \code{"timeSeries"} or \code{"zoo"}. So one can estimate the
    parameters of a GARCH process from empirical data using the
    function \code{garchFit} and simulate than statistically 
    equivalent GARCH processes with the same set of model parameters
    using the function \code{garchSim}.
    
    The third entry in the argument \code{returnClass="mts"} allows
    to return a trivariate time series, where the first column contains
    the simulated \code{"garch"} process, the second column the conditional
    standard deviations \code{"h"}, and the last column the innovations
    named \code{"eps"}.
    
    Note, the default model specifies Bollerslev's GARCH(1,1) model
    with normal distributed innovations.

}


\value{

    The function \code{garchSim} returns an objects of class
    \code{"timeSeries"} attributed by a list with entry
    \code{$garchSpec} giving the GARCH specification structure as
    returned by the function \code{garchSpec} and with information on 
    conditional standard deviations and innovations. See details above.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSpec -
   spec = garchSpec()
   spec

## garchSim -
   # Simulate a "timeSeries" object:
   x = garchSim(spec, n = 50)
   class(x)
   print(x) 
   
## More simulations ...

   # Default GARCH(1,1) - uses default parameter settings
   spec = garchSpec(model = list())
   garchSim(spec, n = 10)
   
   # ARCH(2) - use default omega and specify alpha, set beta=0!
   spec = garchSpec(model = list(alpha = c(0.2, 0.4), beta = 0))
   garchSim(spec, n = 10)
   
   # AR(1)-ARCH(2) - use default mu, omega
   spec = garchSpec(model = list(ar = 0.5, alpha = c(0.3, 0.4), beta = 0))
   garchSim(spec, n = 10)
   
   # AR([1,5])-GARCH(1,1) - use default garch values and subset ar[.]
   spec = garchSpec(model = list(mu = 0.001, ar = c(0.5,0,0,0,0.1)))
   garchSim(spec, n = 10)
   
   # ARMA(1,2)-GARCH(1,1) - use default garch values
   spec = garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3)))  
   garchSim(spec, n = 10)
   
   # GARCH(1,1) - use default omega and specify alpha/beta
   spec = garchSpec(model = list(alpha = 0.2, beta = 0.7))
   garchSim(spec, n = 10)
   
   # GARCH(1,1) - specify omega/alpha/beta
   spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8))
   garchSim(spec, n = 10)
   
   # GARCH(1,2) - use default omega and specify alpha[1]/beta[2]
   spec = garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4)))
   garchSim(spec, n = 10)
   
   # GARCH(2,1) - use default omega and specify alpha[2]/beta[1]
   spec = garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08))
   garchSim(spec, n = 10)
   
   # snorm-ARCH(1) - use defaults with skew Normal
   spec = garchSpec(model = list(beta = 0, skew = 0.8), cond.dist = "snorm")
   garchSim(spec, n = 10)
   
   # sged-GARCH(1,1) - using defaults with skew GED
   model = garchSpec(model = list(skew = 0.93, shape = 3), cond.dist = "sged")
   garchSim(model, n = 10)
   
   # Taylor Schwert GARCH(1,1) - this belongs to the family of APARCH Models
   spec = garchSpec(model = list(delta = 1))
   garchSim(spec, n = 10)
   
   # AR(1)-t-APARCH(2, 1) - a little bit more complex specification ...
   spec = garchSpec(model = list(mu = 1.0e-4, ar = 0.5, omega = 1.0e-6, 
       alpha = c(0.10, 0.05), gamma = c(0, 0), beta = 0.8, delta = 1.8, 
       shape = 4, skew = 0.85), cond.dist = "sstd")
   garchSim(spec, n = 10)
}


\keyword{models}

