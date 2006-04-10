\name{GlmGevFit}

\alias{GlmGevFit}

\alias{gevglmFit}

\alias{print.gevglmFit}
\alias{plot.gevglmFit}
\alias{summary.gevglmFit}

\alias{gevglmprofPlot}
\alias{gevglmprofxiPlot}


\title{Modelling the GEV Distribution including GLM}


\description{
  
    A collection and description of functions to model the 
    Generalized Extreme Value, GEV, distribution by maximum 
    likelihood approximation based on R's 'ismev' package. In 
    addition to the function gevFit the parameter estimation 
    allows to include generalized linear modelling, GLM, of 
    each parameter.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{gevglmFit} \tab fits empirical or simulated data to the distribution, \cr 
    \code{print} \tab print method for a fitted GEV object of class ..., \cr
    \code{plot} \tab plot method for a fitted GEV object, \cr 
    \code{summary} \tab summary method for a fitted GEV object, \cr
    \code{gevglmprofPlot} \tab profile log-likelihoods for return levels, \cr 
    \code{gevglmprofxiPlot} \tab profile log-likelihoods for shape parameters. }
    
}


\usage{
gevglmFit(x, y = NULL, gumbel = FALSE, mul = NULL, sigl = NULL, shl = NULL, 
    mulink = identity, siglink = identity, shlink = identity, show = FALSE, 
    method = "Nelder-Mead", maxit = 10000, \dots)

\method{print}{gevglmFit}(x, \dots)
\method{plot}{gevglmFit}(x, which = "ask", \dots)
\method{summary}{gevglmFit}(object, doplot = TRUE, which = "all", \dots)

gevglmprofPlot(object, m, xlow, xup, conf = 0.95, nint = 100)
gevglmprofxiPlot(object, xlow, xup, conf = 0.95, nint = 100)
}


\arguments{
  
    \item{conf}{
        [gevglmprof*Plot] - \cr
        the confidence coefficient of the plotted profile confidence 
        interval.
        }
    \item{doplot}{
        a logical. Should the results be plotted?
        } 
    \item{gumbel}{
        [gevglmFit] - \cr
        a logical, should a Gumbel fit be created? In this case the shape
        parameter equals zero, and the arguments \code{shl} and and 
        \code{shlink} are omitted.
        }
    \item{m}{
        [gevglmprofPlot] - \cr
        the return level; i.e. the profile likelihood is for the value 
        that is exceeded with probability 1/\code{m}.
        }
    \item{maxit}{
        [gevglmFit] - \cr
        the maximum number of iterations.
        }
    \item{method}{
        [gevglmFit] - \cr
        the optimization method (see \code{\link{optim}} for details).
        }
    \item{mul, sigl, shl}{
        [gevglmFit] - \cr
        numeric vectors of integers, giving the columns
        of \code{ydat} that contain covariates for generalized linear
        modelling of the location, scale and shape parameters repectively
        (or \code{NULL} (the default) if the corresponding parameter is
        stationary).
        }
    \item{mulink, siglink, shlink}{
        [gevglmFit] - \cr
        inverse link functions for generalized linear modelling of the 
        location, scale and shape parameters repectively.
        }
    \item{nint}{
        [gevglmprof*Plot] - \cr
        the number of points at which the profile likelihood is evaluated.
        } 
    \item{object}{
        [summary][grlevelPlot] - \cr
        a fitted object of class \code{"gevglmFit"}.
        }
    \item{show}{
        [gevglmFit] - \cr
        a logical; if \code{TRUE} (the default), print details of
        the fit.
        }
    \item{x}{
        [gevglmFit] - \cr
        a numeric vector of data to be fitted.
        \cr
        [print][plot] - \cr
        a fitted object of class \code{"gevglmFit"}.
        }
    \item{xlow, xup}{
        [gevglmprof*Plot] - \cr
        the least and greatest value at which to evaluate the profile 
        likelihood.
        }
    \item{y}{
        [gevglmFit] - \cr
        a matrix of covariates for generalized linear modelling
        of the parameters (or \code{NULL} (the default) for stationary
        fitting). The number of rows should be the same as the length
        of \code{xdat}.
        }
    \item{which}{
        [plot][summary] - \cr
        a vector of logicals, one for each plot, denoting which plot 
        should be displayed.
        }
    \item{\dots}{
        [gevglmFit] - \cr
        control parameters optionally passed to the 
        optimization function. Parameters for the optimization
        function are passed to components of the \code{control} argument of
        \code{optim}. 
        \cr
        [plot][summary] - \cr
        arguments passed to the plot function.
        }     
        
}


\value{
  
    \code{gevglmFit}
    \cr
    returns a list containing the following components.
    A subset of these components are printed after the fit. If \code{show} 
    is \code{TRUE}, then assuming that successful convergence is
    indicated, the components \code{nllh}, \code{mle} and \code{se}
    are always printed.
  
    \item{trans}{
        an logical indicator for a non-stationary fit.
        }
    \item{model}{
        a list with components \code{mul}, \code{sigl}
        and \code{shl}.
        }
    \item{link}{
        a character vector giving inverse link functions.
        }
    \item{conv}{
        the convergence code, taken from the list returned by
        \code{\link{optim}}. A zero indicates successful convergence.
        }
    \item{nllh}{
        the negative logarithm of the likelihood evaluated at
        the maximum likelihood estimates.
        }
    \item{data}{
        the data that has been fitted. For non-stationary
        models, the data is standardized.
        }
    \item{mle}{
        a vector containing the maximum likelihood estimates.
        }
    \item{cov}{
        the covariance matrix.
        }
    \item{se}{
        a vector containing the standard errors.
        }
    \item{vals}{
        a matrix with three columns containing the maximum
        likelihood estimates of the location, scale and shape parameters
        at each data point.
        }  
    
    For stationary models four plots are produced; a probability
    plot, a quantile plot, a return level plot and a histogram
    of data with fitted density. For non-stationary models two plots 
    are produced; a residual probability plot and a residual quantile 
    plot.
    \cr
  
    \code{gevglmprofPlot}\cr
    \code{gevglmprofxiPlot}\cr
    a plot of the profile likelihood is produced, with a horizontal line 
    representing a profile confidence interval with confidence coefficient 
    \code{conf}.
  
}


\details{

    \bold{Simulation:}
    \cr\cr
    To simulate a GEV series use the function \code{gevSim}. 
    \cr
    
    \bold{Parameter Estimation:}
    \cr\cr
    \code{gevglmFit} fits by the Maximum-likelihood approach the generalized 
    extreme value distribution, including generalized linear modelling 
    of each parameter. 
    \cr
  
    \bold{Methods:}
    \cr\cr
    \code{print.gevglm}, \code{plot.gevglm} and \code{summary.gevglm} 
    are print, plot, and summary methods for a fitted object of class 
    \code{gevglmFit}.
    \cr
  
    \bold{Profile Likelihood:}
    \cr\cr
    \code{gevglmprofPlot} and \code{gevglmprofxiPlot} produce profile 
    log-likelihoods for shape parameters and \code{m} year/block return 
    levels for stationary GEV models using the output of the function 
    \code{gevFit}.
    \cr

    \bold{Nonstationary Models:}
    \cr\cr
    For non-stationary fitting it is recommended that the covariates
    within the generalized linear models are (at least approximately)
    centered and scaled (i.e.\ the columns of \code{ydat} should be
    approximately centered and scaled).
    
}


\author{
  
    Alec Stephenson for the code implemented from \R's ismev package, \cr
    Stuart Scott for the original code, and
    Diethelm Wuertz for this \R-port.
    
}


\references{

Coles S. (2001);
    \emph{Introduction to Statistical Modelling of Extreme Values},
    Springer.  
    
}

  
\examples{
## SOURCE("fExtremes.054A-ExtremesGlm")
\dontrun{
## Simulated GEV Data:
   xmpExtremes("\nStart: Simulte GEV Sample >")
   x = gevSim(model = list(shape = 0.25, location = 0, scale = 1), n = 1000)
   par(mfrow = c(2, 2))
   plot(x, main = "Simulated GEV Data")
   # Fit GEV Data:
   fit = gevglmFit(x) 
   print(fit)
   # Summarize Results:
   summary(fit, which = c(TRUE, TRUE, TRUE, FALSE))
   
## Simulated GEV Data:
   xmpExtremes("\nNext: Estimate Parameters >")
   ##> x = gevSim(model = list(shape = 0, location = 0, scale = 1), n = 1000)
   ##> par(mfrow = c(2, 2))
   ##> plot(x, main = "Simulated Gumbel Data")
   # Fit GEV Data:
   ##> fit = gevglmFit(x, gumbel = TRUE) 
   ##> print(fit)
   # Summarize Results:
   ##> summary(fit, which = c(TRUE, TRUE, TRUE, FALSE))
   
## Portpirie Data:
   xmpExtremes("\nNext: Fit Portpirie Data >")
   par(mfrow = c(2, 1))
   data(portpirie)
   fit = gevglmFit(portpirie[, 2])
   gevglmprofPlot(fit, m = 10, 4.1, 5)
   title(main = "Portpirie")
   gevglmprofxiPlot(fit, -0.3, 0.3)
   title(main = "Portpirie")
   
## Interactive Plot:
   ##> par(mfrow = c(2, 2))
   ##> plot(fit)
}
}


\keyword{models}

