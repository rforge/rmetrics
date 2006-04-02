\name{GlmPPFit}

\alias{GlmPPFit}

\alias{ppFit}

\alias{print.ppFit}
\alias{plot.ppFit}
\alias{summary.ppFit}

\title{Modelling Point Processes}


\description{

    A collection and description of functions to model 
    point processes, PP, over a threshold, based on \R's 
    'ismev' package. The parameter estimation allows to 
    include generalized linear modelling, GLM, of each 
    parameter.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{potSim} \tab generates data from a point process, \cr
    \code{potFit} \tab fits empirical or simulated data to a point process, \cr 
    \code{print} \tab print method for a fitted POT object of class ..., \cr
    \code{plot} \tab plot method for a fitted GEV object, \cr 
    \code{summary} \tab summary method for a fitted GEV object, \cr     
    \code{gevrlevelPlot} \tab k-block return level with confidence intervals. }
    
}


\usage{
ppFit(x, threshold, npy = 365, y = NULL, mul = NULL, sigl = NULL, 
    shl = NULL, mulink = identity, siglink = identity, shlink =
    identity, method = "Nelder-Mead", maxit = 10000, \dots)

\method{print}{ppFit}(x, \dots)
\method{plot}{ppFit}(x, which = "ask", \dots)
\method{summary}{ppFit}(object, doplot = TRUE, which = "all", \dots)
}


\arguments{

    \item{doplot}{
        a logical. Should the results be plotted?
        }
    \item{maxit}{
        [ppFit] - \cr
        the maximum number of iterations.
        }
    \item{method}{
        [ppFit] - \cr
        The optimization method (see \code{\link{optim}} for details).
        }
    \item{mul, sigl, shl}{
        [ppFit] - \cr
        numeric vectors of integers, giving the columns
        of \code{ydat} that contain covariates for generalized linear
        modelling of the location, scale and shape parameters repectively
        (or \code{NULL} (the default) if the corresponding parameter is
        stationary).
        }
    \item{mulink, siglink, shlink}{
        [ppFit] - \cr
        inverse link functions for generalized
        linear modelling of the location, scale and shape parameters
        repectively.
        }
    \item{npy}{
        [ppFit] - \cr
        the number of observations per year/block.
        }
    \item{object}{
        [summary] - \cr
        a fitted object of class \code{"ppFit"}.
        }
    \item{threshold}{
        [ppFit] - \cr
        the threshold; a single number or a numeric
        vector of the same length as \code{x}.
        }
    \item{which}{
        [print][plot][summary] - \cr
        a logical for each plot, denoting which plots should be created.
        }
    \item{x}{
        [ppFit] - \cr
        a numeric vector of data to be fitted.
        \cr
        [print][plot] -
        a fitted object of class \code{"ppFit"}.
        }
    \item{y}{
        [ppFit] - \cr
        a matrix of covariates for generalized linear modelling
        of the parameters (or \code{NULL} (the default) for stationary
        fitting). The number of rows should be the same as the length
        of \code{x}.
        }
    \item{\dots}{
        [ppFit] - \cr
        control parameters and plot parameters optionally passed to the 
        optimization and/or plot function. Parameters for the optimization
        function are passed to components of the \code{control} argument of
        \code{optim}.
        }     
        
}


\value{

    A list containing the following components. A subset of these
    components are printed after the fit. If \code{show} is
    \code{TRUE}, then assuming that successful convergence is
    indicated, the components \code{nexc}, \code{nllh}, \code{mle}
    and \code{se} are always printed.
  
    \item{trans}{
        An logical indicator for a non-stationary fit.
        }
    \item{model}{
        A list with components \code{mul}, \code{sigl}
        and \code{shl}.
        }
    \item{link}{
        A character vector giving inverse link functions.
        }
    \item{threshold}{
        The threshold, or vector of thresholds.
        }
    \item{npy}{
        The number of observations per year/block.
        }
    \item{nexc}{
        The number of data points above the threshold.
        }
    \item{data}{
        The data that lie above the threshold. For
        non-stationary models, the data is standardized.
        }
    \item{conv}{
        The convergence code, taken from the list returned by
        \code{\link{optim}}. A zero indicates successful convergence.
        }
    \item{nllh}{
        The negative logarithm of the likelihood evaluated at
        the maximum likelihood estimates.
        }
    \item{vals}{
        A matrix with four columns containing the maximum
        likelihood estimates of the location, scale and shape
        parameters, and the threshold, at each data point.
        }
    \item{gpd}{
        A matrix with three rows containing the maximum
        likelihood estimates of corresponding GPD location, scale
        and shape parameters at each data point.
        }
    \item{mle}{
        A vector containing the maximum likelihood estimates.
        }
    \item{cov}{
        The covariance matrix.
        }
    \item{se}{
        A vector containing the standard errors.
        }
    
    For stationary models two plots are produced; a probability plot 
    and a quantile plot. For non-stationary models two plots are produced; 
    a residual probability plot and a residual quantile plot.
    
}


\details{
  
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
\dontrun{
## SOURCE("fExtremes.054C-PPFit")

## Use Rain Data:
   data(rain)
   
## Fit Point Process Model:
   xmpExtremes("Start: Parameter Fit for Point Process > ")
   fit = ppFit(x = rain[1:200], threshold = 10)
   print(fit) 
   
## Summarize Results:
   xmpExtremes("Next: Diagnostic Analysis > ")
   par(mfrow = c(2, 2), cex = 0.75)
   summary(fit)
   xmpExtremes("Next: Interactive Plot > ")
   
## Interactive Plot:
   ##> par(mfrow = c(2, 2), cex = 0.75)
   ##> plot(fit)
}
}


\keyword{models}

