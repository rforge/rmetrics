\name{garchSim}

\alias{garchSim}

\title{Univariate GARCH/APARCH Time Series Simulation}


\description{

    Simulates an univariate GARCH/APARCH time series model.

}


\usage{
garchSim(model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), 
    n = 100, n.start = 100, presample = NULL, cond.dist = c("rnorm", 
    "rged", "rstd", "rsnorm", "rsged", "rsstd"), rseed = NULL,
    returnClass = c("ts", "numeric"))
}


\arguments{

    \item{cond.dist}{
        a character string naming the desired conditional distribution.
        Valid values are \code{"dnorm"}, \code{"dged"}, \code{"dstd"}, 
        \code{"dsnorm"}, \code{"dsged"}, \code{"dsstd"}. The default value 
        is the normal distribution.
        }
    \item{model}{
        a list of GARCH model parameters: \cr
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
        \cr
        Note, the default model specifies Bollerslev's GARCH(1,1) model
        with normal distributed innovations.
        }   
    \item{n}{
        length of output series, an integer value. An integer value,
        by default \code{n=100}.
        }
    \item{n.start}{
        length of "burn-in" period, by default 100.
        }
    \item{presample}{
        a numeric three column matrix with start values for the series, 
        for the innovations, and for the conditional variances. For an 
        ARMA(m,n)-GARCH(p,q) process the number of rows must be at least 
        max(m,n,p,q), longer presamples are cutted.
        }
    \item{returnClass}{
        a character string denoting the return class of the simulated
        time series, either an object of class \code{"ts"} or a 
        \code{"numeric"} vector.
        }
    \item{rseed}{
        single integer argument, the seed for the intitialization of
        the random number generator for the innovations.
        }

}


\details{

    The function \code{garchSim} simulates a GARCH or APARCH time
    series process as specified by the argument \code{model}.
    
    The \code{model} argument can be set up from scratch or obtained
    from the \code{@model} as returned by the function \code{garchSpec}.
    
}


\value{

    The function \code{garchSim} returns an objects of class \code{ts} 
    attributed with a list with entry \code{$garchSpec} giving the GARCH 
    specification structure as returned by the function \code{garchSpec}. 
    
    Optionally, the function can return a numeric vector when setting
    the argument \code{returnClass="numeric"}. In this case the specification 
    structure will not be attributed to the returned numeric vector.
    
    Alternatively, one can create a \code{ts} object and convert it with
    the function \code{asVector} into a numeric vector. In this case the
    parameters of the \code{ts} object will be attributed to the returned
    numeric vector.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSpec -
   spec = garchSpec()
   spec

## garchSim -
   # Simulate a "ts" object:
   x = garchSim(model = spec@model, n = 10)
   class(x)
   print(x) 
   
## garchSim - 
   # Simulate a numeric vector:
   y = garchSim(model = spec@model, n = 10, returnClass = "numeric")
   class(y)
   print(y)
    
## asVector - 
   # Simulate a numeric vector:
   z = asVector(x)
   class(z)
   print(z)
   
\dontrun{
## More Simulations ...
    
    # ARCH(1) - default omega and alpha
    spec = garchSpec(model = list())
    garchSim(n = 10, model = spec)
    
    # ARCH(1) - default omega
    spec = garchSpec(model = list(alpha = 0.1))
    garchSim(n = 10, model = spec)
    
    # ARCH(1)
    spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1))
    garchSim(n = 10, model = spec)
    
    # AR(1) - ARCH(1)
    spec = garchSpec(model = list(ar = 0.5)) 
    garchSim(n = 10, model = spec)
    
    # AR([1,5]) - ARCH(1)
    spec = garchSpec(model = list(ar = c(0.5, 0, 0, 0 ,0.1)))
    garchSim(n = 10, model = spec)
    
    # ARMA(1,2) - ARCH(1)
    spec = garchSpec(model = list(ar = 0.5, ma = c(0.3,-0.3)))
    garchSim(n = 10, model = spec)
    
    # Skew-Normal - ARCH(2)
    spec = garchSpec(model = list(alpha = c(0.12, 0.04), dist = 2/3), 
        cond.dist = "rsnorm")
    garchSim(n = 10, model = spec)
    
    # GARCH(1,1)
    spec = garchSpec()
    garchSim(n = 10, model = spec)
    
    # GARCH(1,1)
    spec = garchSpec(model = list(alpha = 0.1, beta = 0.8))
    garchSim(n = 10, model = spec)
    
    # GARCH(1,1)
    spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8))
    garchSim(n = 10, model = spec)
    
    # GARCH(1,2)
    spec = garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4)))
    garchSim(n = 10, model = spec)
    
    # GARCH(2,1)
    spec = garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08))
    garchSim(n = 10, model = spec)   
    
    # Normal - GARCH(1,1)   
    spec = garchSpec(model = list(), cond.dist = "rnorm")
    garchSim(n = 10, model = spec)
    
    # Skewed Normal - GARCH(1,1)
    spec = garchSpec(model = list(parm = 2), cond.dist = "rsnorm")
    garchSim(n = 10, model = spec)
    
    # GED - GARCH(1,1)
    spec = garchSpec(model = list(parm = 4), cond.dist = "rged")
    garchSim(n = 10, model = spec)
    
    # Skewed GED - GARCH(1,1)
    spec = garchSpec(model = list(parm = c(4, 2)), cond.dist = "rsged")
    garchSim(n = 10, model = spec)
    
    # Normalized Student t - GARCH(1,1)
    spec = garchSpec(model = list(parm = 4), cond.dist = "rstd")
    garchSim(n = 10, model = spec)
    
    # Skewed Normalized Student t - GARCH(1,1)
    spec = garchSpec(model = list(parm = c(4, 2)), cond.dist = "rsstd")
    garchSim(n = 10, model = spec)
             
    # Taylor Schwert Normal GARCH(1,1)
    spec = garchSpec(list(alpha = 0.1, delta = 1))
    garchSim(n = 10, model = spec)
   
}
}


\keyword{models}

