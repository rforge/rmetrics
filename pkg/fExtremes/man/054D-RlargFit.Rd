\name{RlargFit}

\alias{RlargFit}

\alias{rlargFit}

\alias{print.rlargFit}
\alias{plot.rlargFit}
\alias{summary.rlargFit}

\title{Modelling the Order Statistic Model}


\description{
  
    A collection and description of functions to model 
    the Order Statistic Model by maximum likelihood 
    approximation based on \R's 'ismev' package. The 
    parameter estimation allows to include generalized 
    linear modelling of each parameter.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{gpdglmFit} \tab fits empirical or simulated data to the distribution, \cr 
    \code{print} \tab print method for a fitted GPD object of class ..., \cr
    \code{plot} \tab plot method for a fitted GPD object, \cr 
    \code{summary} \tab summary method for a fitted GPD object, \cr
    \code{gevglmprofPlot} \tab profile log-likelihoods for return levels, \cr 
    \code{gevglmprofxiPlot} \tab profile log-likelihoods for shape parameters. }
    
}


\usage{
rlargFit(x, r = dim(x)[2], y = NULL, mul = NULL, sigl = NULL,
    shl = NULL, mulink = identity, siglink = identity, shlink = identity,
    method = "Nelder-Mead", maxit = 10000, \dots)

\method{print}{rlargFit}(x, \dots)
\method{plot}{rlargFit}(x, which = "all", \dots)
\method{summary}{rlargFit}(object, doplot = TRUE, which = "all", \dots)
}


\arguments{

    \item{doplot}{
        a logical. Should the results be plotted?
        }
    \item{maxit}{
        [rlargFit] - \cr
        the maximum number of iterations.
        }
    \item{method}{
        [rlargFit] - \cr
        the optimization method (see \code{\link{optim}} for details).
        }
    \item{mul, sigl, shl}{
        [rlargFit] - \cr
        numeric vectors of integers, giving the columns
        of \code{ydat} that contain covariates for generalized linear
        modelling of the location, scale and shape parameters repectively
        (or \code{NULL} (the default) if the corresponding parameter is
        stationary).
        }
    \item{mulink, siglink, shlink}{
        [rlargFit] - \cr
        inverse link functions for generalized linear modelling of the 
        location, scale and shape parameters repectively.
        }
    \item{object}{
        [summary] - \cr
        a fitted object of class \code{"rlargFit"}.
        }
    \item{r}{
        [rlargFit] - \cr
        the largest \code{r} order statistics are used for the fitted model.
        }
    \item{x}{
        [rlargFit] - \cr
        a numeric matrix of data to be fitted. Each row should be a vector 
        of decreasing order, containing the largest order statistics for 
        each year (or time period). The first column therefore contains annual 
        (or period) maxima.
        Only the first \code{r} columns are used for the fitted model. By 
        default, all columns are used.
        If one year (or time period) contains fewer order statistics than 
        another, missing values can be appended to the end of the 
        corresponding row.
        \cr
        [print][plot] - \cr
        a fitted object of class \code{"rlargFit"}.
        }
    \item{y}{
        [rlargFit] - \cr
        A matrix of covariates for generalized linear modelling of the 
        parameters (or \code{NULL} (the default) for stationary fitting). 
        The number of rows should be the same as the number of rows of 
        \code{x}.
        }
    \item{which}{
        [print][plot][summary] - \cr
        a logical for each plot, denoting which plots should be created.
        }
    \item{\dots}{
        [rlargFit][plot] - \cr
        control parameters and plot parameters optionally passed to the 
        optimization and/or plot function. Parameters for the optimization
        function are passed to components of the \code{control} argument of
        \code{optim}.
        }   
    
}


\details{
  
    For non-stationary fitting it is recommended that the covariates
    within the generalized linear models are (at least approximately)
    centered and scaled (i.e.\ the columns of \code{ydat} should be
    approximately centered and scaled).
    
}


\value{
  
    A list containing the following components. A subset of these
    components are printed after the fit. If \code{show} is
    \code{TRUE}, then assuming that successful convergence is
    indicated, the components \code{nllh}, \code{mle} and \code{se}
    are always printed.
  
    \item{trans}{
        An logical indicator for a non-stationary fit.
        }
    \item{model}{
        A list with components \code{mul}, \code{sigl} and \code{shl}.
        }
    \item{link}{
        A character vector giving inverse link functions.
        }
    \item{conv}{
        The convergence code, taken from the list returned by
        \code{\link{optim}}. A zero indicates successful convergence.
        }
    \item{nllh}{
        The negative logarithm of the likelihood evaluated at
        the maximum likelihood estimates.
        }
    \item{data}{
        The data that has been fitted. For non-stationary
        models, the data is standardized.
        }
    \item{mle}{
        A vector containing the maximum likelihood estimates.
        }
    \item{cov}{
        The covariance matrix.
        }
    \item{se}{
        A vector containing the standard errors.}
    \item{vals}{
        A matrix with three columns containing the maximum
        likelihood estimates of the location, scale and shape parameters
        at each data point.
        }
    \item{r}{
        The number of order statistics used.
        }
    
    For stationary models four plots are initially produced;
    a probability plot, a quantile plot, a return level plot
    and a histogram of data with fitted density.
    Then probability and quantile plots are produced for the
    largest \code{n} order statistics. For non-stationary models 
    residual probability plots and residual quantile plots are 
    produced for the largest \code{n} order statistics.
    
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
## SOURCE("fExtremes.54D-RlargFit")

## Use Venice Data:
   data(venice)
   
## Fit for the order statistic model:
   xmpExtremes("Start: Parameter Fit for Order Statistics Model > ")
   fit = rlargFit(venice[, 2:4], r = 3)
   fit
   
## Summarize Results:
   xmpExtremes("Next: Diagnostic Analysis > ")
   summary(fit) 
}


\keyword{models}

