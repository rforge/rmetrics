\name{garchSpec}


\alias{garchSpec}


\title{Univariate GARCH Time Series Specification}


\description{

    Specifies an univariate GARCH time series model.

}


\usage{
garchSpec(model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), 
    presample = NULL, cond.dist = c("rnorm", "rged", "rstd", "rsnorm", 
    "rsged", "rsstd"), rseed = NULL)
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
    \item{presample}{
        a numeric three column matrix with start values for the series, 
        for the innovations, and for the conditional variances. For an 
        ARMA(m,n)-GARCH(p,q) process the number of rows must be at least 
        max(m,n,p,q), longer presamples are cutted.
        }
    \item{rseed}{
        single integer argument, the seed for the intitialization of
        the random number generator for the innovations.
        }
}


\value{

    returns an object of class \code{"fGARCHSPEC"}.
        
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
   spec = garchSpec(model = list(skew = 0.8), cond.dist = "rsnorm")
   spec
   
   # Skewed GED Conditional Distribution:
   spec = garchSpec(model = list(skew = 0.9, shape = 4.8), cond.dist = "rsged")
   spec
   
\dontrun{
## More examples:

   # ARCH(1) - use default omega and default alpha[1]
   garchSpec(model = list())
   
   # ARCH(1) - use default omega and specify alpha
   garchSpec(model = list(alpha = 0.2))
   
   # ARCH(1) - specify omega and alpha
   garchSpec(model = list(omega = 3.0e-6, alpha = 0.3))
   
   # AR(1)-ARCH(1) - use default omega/alpha and specify alpha[1]
   garchSpec(model = list(ar = 0.5))
   
   # AR([1,5])-ARCH(1) - use default omega, specify alpha and subset ar[.]
   garchSpec(model = list(ar = c(0.5,0,0,0,0.1), alpha = 0.25))
   
   # ARMA(1,2)-ARCH(1) - use default omega/alpha and specify ar[1]/ma[2]
   garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3)))
   
   # ARMA(2,2)-ARCH(1) use default omega/alpha and specify ar[2]/ma[2]
   garchSpec(model = list(ar = c(0.5, -0.5), ma = c(0.3,-0.3)))
   
   # ARCH(2) - use default omega and specify alpha[2]
   garchSpec(model = list(alpha = c(0.12, 0.04)))
   
   # GARCH(1,1) - use just defaults
   garchSpec())
   
   # GARCH(1,1) - use default omega and specify alpha/beta
   garchSpec(model = list(alpha = 0.2, beta = 0.7))
   
   # GARCH(1,1) - specify omega/alpha/beta
   garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8))
   
   # GARCH(1,2) - use default omega and specify alpha[1]/beta[2]
   garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4)))
   
   # GARCH(2,1) - use default omega and specify alpha[2]/beta[1]
   garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08))
   
   # rsnorm-ARCH(1) - use defaults with skew Normal
   garchSpec(model = list(dist = 2), cond.dist = "rsnorm")
   
   # rged-ARCH(1) using default omega and alpha[1]
   garchSpec(model = list(dist = 4), cond.dist = "rged")
   
   # rsged-ARCH(1) using default omega and alpha[1]
   garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsged")
   
   # rstd-ARCH(1) using default omega and alpha[1]
   garchSpec(model = list(dist = 4), cond.dist = "rstd")
   
   # rsstd-ARCH(1) using default omega and alpha[1]
   garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsstd")
   
   # TS-GARCH(1,1)
   garchSpec(model = list(delta = 1))
   
   # AR(1)-t-APARCH(2, 1)
   garchSpec(model = list(mu = 1.0e-4, ar = 0.5, omega = 1.0e-6, 
       alpha = c(0.10, 0.05), gamma = c(0, 0), beta = 0.8, delta = 1.8, 
       dist = c(nu = 4, xi = 0.5)), cond.dist = "rsstd")
}       
}


\keyword{models}

