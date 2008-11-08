\name{regSim}


\alias{regSim}

\alias{LM3}
\alias{LOGIT3}
\alias{GAM3}


\title{Regression Model Simulation}


\description{

    Simulates regression models.

}


\usage{
regSim(model = "LM3", n = 100, ...)

LM3(n = 100, seed = 4711)
LOGIT3(n = 100, seed = 4711)
GAM3(n = 100, seed = 4711)
}


\arguments{


    \item{model}{
        a character string defining the function name from which the
        regression model will be simulated.
        }   
    \item{n}{
        an integer value setting the length, i.e. the number of records
        of the output series, an integer value. By default \code{n=100}.
        }
    \item{seed}{
        an integer value, the recommended way to specify seeds for 
        random number generation.

        }
    \item{\dots}{
        arguments to be passed to the underlying function specified by
        the \code{model} argument.
        }

}


\details{

    The function \code{regSim} allows to simulate from various regression
    models defined by one of the three example functions \code{LM3},
    \code{LOGIT3}, \code{GAM3} or by a user specified function.
    
    The examples are defined in the following way:
    
    \code{# LM3:}\cr
    \code{> y = 0.75 * x1 + 0.25 * x2 - 0.5 * x3 + 0.1 * eps }\cr
    
    \code{# LOGIT3:}\cr
    \code{> y = 1 / (1 + exp(- 0.75 * x1 + 0.25 * x2 - 0.5 * x3 + eps)) }\cr

    \code{# GAM3:}\cr
    \code{> y = scale(scale(sin(2 * pi * x1)) + scale(exp(x2)) + scale(x3)) }\cr
    \code{> y = y + 0.1 * rnorm(n, sd = sd(y))}\cr
    
    \code{"LM3"} models a liner regression model, \code{"LOGIT3"} a generalized
    linear regression model expressed by a logit model, and \code{"GAM"} an
    additive model. \code{x1}, code{x2}, \code{x3}, and \code{eps} are random
    normal deviates of length \code{n}.
    
    The \code{model} function should return an rectangular series defined 
    as an object of class \code{data.frame}, \code{timeSeries} or \code{mts}
    which can be accepted from the parameter estimation
    functions \code{regFit} and \code{gregFit}.
    
}


\value{

    The function \code{garchSim} returns an object of the same class 
    as returned by the underlying function \code{match.fun(model)}.
    These may be objects of class \code{data.frame}, \code{timeSeries} or
    \code{mts}.
    
}


\note{

    This function is still under development. For the future we plan, 
    that the function \code{regSim} will be able to generate general
    regression models.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## LM2 -
   # Data for a user defined linear regression model:
   LM2 = function(n){
     x = rnorm(n)
     y = rnorm(n)
     eps = 0.1 * rnorm(n)
     z = 0.5 + 0.75 * x + 0.25 * y + eps
     data.frame(Z = z, X = x, Y = y)
   }
   for (FUN in c("LM2", "LM3")) {
     cat(FUN, ":\n", sep = "")
     print(regSim(model = FUN, n = 10))
   }
}


\keyword{models}

