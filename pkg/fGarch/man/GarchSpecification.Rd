\name{garchSpec}


\alias{garchSpec-class}
\alias{garchSpec}

\alias{show.garchSpec}
\alias{show,garchSpec-method}


\title{Univariate GARCH Time Series Specification}


\description{

    Specifies an univariate GARCH time series model.

}


\usage{
garchSpec(model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), 
    presample = NULL, cond.dist = c("rnorm", "rged", "rstd", "rsnorm", 
    "rsged", "rsstd"), rseed = NULL)
    
show.garchSpec(object)
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
    \item{object}{
        an object of class \code{garchSpec} as returned from the function
        \code{garchSpec()}.
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

    \code{garchSpec}\cr
    \cr
    returns a S4 object of class \code{garchSpec} with the following slots:
    
    \item{@call}{
        the call of the \code{garch} function.
        }
    \item{@formula}{
        a list with two formula entries for the mean and variance 
        equation.
        }
    \item{@model}{
        a list with the model parameters.
        }   
    \item{@presample}{
        a numeric matrix with presample values.
        }
    \item{@distribution}{
        a character string with the name of the conditional distribution.
        }
    \item{@rseed}{
        an integer with the random number generator seed.
        }
        
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
}


\keyword{models}

