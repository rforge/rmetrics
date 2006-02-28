\name{HestonNandiGarchFit}


\alias{HestonNandiGarchFit}

\alias{hngarchSim}
\alias{hngarchFit}

\alias{hngarchStats}
\alias{print.hngarch}
\alias{summary.hngarch}


\title{Heston-Nandi Garch(1,1) Modelling}


\description{

    A collection and description of functions to model 
    the GARCH(1,1) price paths which underly Heston and 
    Nandi's option pricing model.
    \cr
    
    The functions are:

    \tabular{ll}{  
	\code{hngarchSim} \tab Simulates a Heston-Nandi Garch(1,1) process, \cr
	\code{hngarchFit} \tab MLE for a Heston Nandi Garch(1,1) model, \cr
	\code{hngarchStats} \tab True moments of the log-Return distribution, \cr
	\code{print.hngarch} \tab Print method, \cr
	\code{summary.hngarch} \tab Diagnostic summary.}
    
}


\usage{
hngarchSim(model, n, innov, n.start, start.innov, rand.gen, \dots)
hngarchFit(x, model = list(lambda = -0.5, omega = var(x), alpha =
	0.1 * var(x), beta = 0.1, gamma = 0, rf = 0), symmetric = TRUE, 
	trace = FALSE, title = NULL, description = NULL, \dots)
hngarchStats(model)

\method{print}{hngarch}(x, \dots)
\method{summary}{hngarch}(object, \dots)
}


\arguments{

    \item{description}{
    	a brief description of the porject of type character.
    	}
    \item{innov}{
        [hngarchSim] - \cr
        is a univariate time series or vector of innovations to produce 
        the series. If not provided, \code{innov} will be generated using 
        the random number generator specified by \code{rand.gen}. 
        Missing values are not allowed. By default the normal
        random number generator will be used.
        }
    \item{model}{
        a list of GARCH model parameters with the following entries:
        \code{lambda},
        \code{omega}, the constant coefficient of the variance equation, 
        \code{alpha} the autoregressive coefficient,
        \code{beta} the variance coefficient,
        \code{gamma} the asymmetry coefficient,
        and \code{rf}, the risk free rate, numeric values.  
        }
    \item{n}{
        [hngarchSim] - \cr
        is the length of the series to be simulated. The default 
        value is 1000.
        }
    \item{n.start}{
        [hngarchSim] - \cr
        gives the number of start-up values to be discarded.
        The default value is 100.
        } 
    \item{object}{
        [summary] - \cr
        a fitted HN-GARCH(1,1) time series object of class \code{"hngarch"} 
        as returned from the function \code{hngarchFit}.
        }
    \item{rand.gen}{
        [hngarchSim] - \cr
        is the function which is called to generate the innovations. 
        Usually, \code{rand.gen} will be a random number generator. 
        Additional arguments required by the random number generator 
        \code{rand.gen}, usually the location, scale and/or shape 
        parameter of the underlying distribution function, have to be 
        passed through the \code{dots} argument.
        }
    \item{start.innov}{
        [hngarchSim] - \cr
        is a univariate time series or vector of innovations to be used 
        as start up values. Missing values are not allowed.
        }
    \item{symmetric}{
        [hngarchFit] - \cr
        a logical, if TRUE a symmetric model is estimated, otherwise 
        the parameters are estimated for an asymmetric HN Garch(1,1) model.
        }
    \item{title}{
    	a character string which allows for a project title.
    	}
    \item{trace}{
        [hngarchFit] - \cr
        a logical value. Should the optimizarion be traced?
        If \code{trace=FALSE}, no tracing is done of the 
        iteration path.
        }
    \item{x}{
        [hngarchFit] - \cr
        an univariate vector or time series. \cr
        [print] - \cr
        a fitted HN-GARCH(1,1) time series object of class \code{"hngarch"} 
        as returned from the function \code{hngarchFit}.
        }
    \item{\dots}{
        additional arguments to be passed.
        }
    
}


\details{

    \bold{Path Simulation:}
    \cr\cr
    The function \code{hngarchSim} simulates a Heston-Nandi Garch(1,1) 
    process with structure parameters specified through the list 
    \code{model(lambda, omega, alpha, beta, gamma, rf)}.
    \cr

    
    \bold{Parameter Estimation:}
    \cr\cr
    The function \code{hngarchFit} estimates by the maximum log-likelihood
    approach the parameters either for a symmetric or an asymmetric 
    Heston-Nandi Garch(1,1) model from the log returns \code{x} of a 
    financial time series. For optimization R's \code{optim} function is
    used. Additional optimization parameters may be passed throught the 
    \code{\dots} argument.
    \cr
  
    
    \bold{Diagnostic Analysis:}
    \cr\cr
    The function \code{summary.hngarch} performs a diagnostic analysis
    and recalculates conditional variances and innovations from the time 
    series.
    \cr
  
    
    \bold{Calculation of Moments:}
    \cr\cr
    The function \code{hngarchStats} calculates the first four true 
    moments of the unconditional log return distribution for a stationary 
    Heston-Nandi Garch(1,1) process with standard normally distributed 
    innovations. In addition the persistence and the expectation values 
    of sigma to the power 2, 4, 6, and 8 can be accessed.
    
}

\value{
  
    \code{hngarchSim}
    \cr
    returns numeric vector with the simulated time 
    series points  neglecting those from the first \code{start.innov} 
    innovations.  
    \cr
  
    \code{hngarchFit}
    \cr
    returns list with two entries: The estimated model parmeters 
    \code{model}, where \code{model} is a list of the parameters 
    itself, and \code{llh} the value of the log likelihood.
    \cr
    
    \code{hngarchStats}
    \cr
    returns a list with the following components:
    \code{persistence}, the value of the persistence,
    \code{meansigma2}, \code{meansigma4}, \code{meansigma6}, \code{meansigma8}, 
    the expectation value of sigma to the power of 2, 4, 6, and 8,
    \code{mean}, \code{variance}, \code{skewness}, \code{kurtosis},
    the mean, variance, skewness and kurtosis of the log returns.
    \cr

    \code{summary.hngarch}
    \cr
    returns list with the following elements: \code{h}, 
    a numeric vector with the conditional variances, \code{z}, a numeric 
    vector with the innovations.
    
}


\references{

Heston S.L., Nandi S. (1997);
    \emph{A Closed-Form GARCH Option Pricing Model},
    Federal Reserve Bank of Atlanta.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fOptions.C1-HestonNandiGarchFit")

## hngarchSim -
   # Simulate a Heston Nandi Garch(1,1) Process:
   xmpOptions("\nStart: Simulate a HN-GARCH(1,1) Process > ")
   # Symmetric Model - Parameters:
   model = list(lambda = 4, omega = 8e-5, alpha = 6e-5, 
     beta = 0.7, gamma = 0, rf = 0)
   ts = hngarchSim(model = model, n = 500, n.start = 100)
   par(mfrow = c(2, 1), cex = 0.75)
   ts.plot(ts, col = "steelblue4", main = "HN Garch Symmetric Model")
   grid()
   
## hngarchFit - 
   # HN-GARCH log likelihood Parameter Estimation:
   xmpOptions("\nNext: Estimate a HN-GARCH(1,1) Process > ")
   # To speed up, we start with the simulated model ...
   mle = hngarchFit(model = model, x = ts, symmetric = TRUE)
   mle
   
## summary.hngarch - 
   # HN-GARCH Diagnostic Analysis:
   xmpOptions("\nNext: Analyse a HN-GARCH(1,1) Process > ")
   par(mfrow = c(3, 1), cex = 0.75)
   summary(mle)    

## hngarchStats - 
   # HN-GARCH Moments:
   xmpOptions("\nNext: Compute Moments of a HN-GARCH(1,1) Process > ")
   hngarchStats(mle$model)     
}


\keyword{models}

