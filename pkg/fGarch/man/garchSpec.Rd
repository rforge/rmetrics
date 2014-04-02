\name{garchSpec}


\alias{garchSpec}


\title{Univariate GARCH Time Series Specification}


\description{

    Specifies an univariate GARCH time series model.

}


\usage{
garchSpec(model = list(), presample = NULL, 
    cond.dist = c("norm", "ged", "std", "snorm", "sged", "sstd"), 
    rseed = NULL)
}


\arguments{

    \item{cond.dist}{
        a character string naming the desired conditional distribution.
        Valid values are \code{"norm"}, \code{"ged"}, \code{"std"}, 
        \code{"snorm"}, \code{"sged"}, \code{"sstd"}. The default value 
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
        The values for the linear part are: \cr
        \code{mu} - the mean value, by default NULL; \cr
        \code{ar} - the autoregressive ARMA coefficients, by default NULL; \cr
        \code{ma} - the moving average ARMA coefficients, by default NULL. 
        \cr
        The parameters for the conditional distributions are:\cr
        \code{skew} - the skewness parameter (also named "xi"), by default
            0.9, effective only for the \code{"dsnorm"}, the \code{"dsged"},
            and the \code{"dsstd"} skewed conditional distributions; \cr
        \code{shape} - the shape parameter (also named "nu"), by default 2 
            for the \code{"dged"} and \code{"dsged"}, and by default 4
            for the \code{"dstd"} and \code{"dsstd"} conditional
            distributions.\cr
        \cr
        Note, the default \code{model=list()} specifies Bollerslev's 
        GARCH(1,1) model with normal conditional distributed innovations.
        }   
    \item{presample}{
        a numeric three column matrix with start values for the series, 
        for the innovations, and for the conditional variances. For an 
        ARMA(m,n)-GARCH(p,q) process the number of rows must be at least 
        max(m,n,p,q)+1, longer presamples are cutted. Note, all presamples
        are initialized by a normal-GARCH(p,q) process.
        }
    \item{rseed}{
        single integer argument, the seed for the intitialization of
        the random number generator for the innovations. Using the
        default value \code{rseed=NULL} then the random number generation
        will be started with \code{set.seed(0)}.
        }
}


\details{

    The function \code{garchSpec} specifies a GARCH or APARCH time
    series process which we can use for simulating artificial GARCH
    and/or APARCH models. This is very useful for testing the
    GARCH parameter estimation results, since your model parameters
    are known and well specified.
    
    For example specifying a subet AR(5[1,5])-GARCH(2,1) model with a 
    standardized Student-t distribution with four degrees of freedom 
    will return the following printed output:
       
        \preformatted{
        garchSpec(model = list(ar = c(0.5,0,0,0,0.1), alpha =  
            c(0.1, 0.1), beta = 0.75, shape = 4), cond.dist = "std")  
        
        Formula: 
         ~ ar(5) + garch(2, 1)  
        Model: 
         ar:    0.5 0 0 0 0.1  
         omega: 1e-06  
         alpha: 0.1 0.1  
         beta:  0.75  
        Distribution:   
         std  
        Distributional Parameter:  
         nu = 4  
        Presample:  
           time          z     h y  
        0     0 -0.3262334 2e-05 0  
        -1   -1  1.3297993 2e-05 0  
        -2   -2  1.2724293 2e-05 0  
        -3   -3  0.4146414 2e-05 0  
        -4   -4 -1.5399500 2e-05 0  
        }
    
    The "Formula" describes the formula expression specifying the
    generating process, "Model" lists the associated model parameters,
    "Distribution" the type of the conditional distribution function
    in use, "Distributional Parmeters" lists the distributional 
    parameter (if any), and the "Presample' shows the presample
    input matrix.
    
    If we have specified \code{presample=NULL} in the argument list, 
    then the presample is generated automatically by default as  
    norm-AR()-GARCH() process.
   
}


\value{

    The returned value is an object of class \code{"fGARCHSPEC"}.
    
}



\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\examples{
## garchSpec -

   # Normal Conditional Distribution:
   spec = garchSpec()
   spec
   
   # Skewed Normal Conditional Distribution:
   spec = garchSpec(model = list(skew = 0.8), cond.dist = "snorm")
   spec
   
   # Skewed GED Conditional Distribution:
   spec = garchSpec(model = list(skew = 0.9, shape = 4.8), cond.dist = "sged")
   spec
   
## More specifications ...

   # Default GARCH(1,1) - uses default parameter settings
   garchSpec(model = list())
   
   # ARCH(2) - use default omega and specify alpha, set beta=0!
   garchSpec(model = list(alpha = c(0.2, 0.4), beta = 0))
   
   # AR(1)-ARCH(2) - use default mu, omega
   garchSpec(model = list(ar = 0.5, alpha = c(0.3, 0.4), beta = 0))
   
   # AR([1,5])-GARCH(1,1) - use default garch values and subset ar[.]
   garchSpec(model = list(mu = 0.001, ar = c(0.5,0,0,0,0.1)))
   
   # ARMA(1,2)-GARCH(1,1) - use default garch values
   garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3)))  
   
   # GARCH(1,1) - use default omega and specify alpha/beta
   garchSpec(model = list(alpha = 0.2, beta = 0.7))
   
   # GARCH(1,1) - specify omega/alpha/beta
   garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8))
   
   # GARCH(1,2) - use default omega and specify alpha[1]/beta[2]
   garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4)))
   
   # GARCH(2,1) - use default omega and specify alpha[2]/beta[1]
   garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08))
   
   # snorm-ARCH(1) - use defaults with skew Normal
   garchSpec(model = list(beta = 0, skew = 0.8), cond.dist = "snorm")
   
   # sged-GARCH(1,1) - using defaults with skew GED
   garchSpec(model = list(skew = 0.93, shape = 3), cond.dist = "sged")
   
   # Taylor Schwert GARCH(1,1) - this belongs to the family of APARCH Models
   garchSpec(model = list(delta = 1))
   
   # AR(1)-t-APARCH(2, 1) - a little bit more complex specification ...
   garchSpec(model = list(mu = 1.0e-4, ar = 0.5, omega = 1.0e-6, 
       alpha = c(0.10, 0.05), gamma = c(0, 0), beta = 0.8, delta = 1.8, 
       shape = 4, skew = 0.85), cond.dist = "sstd")
}


\keyword{models}

