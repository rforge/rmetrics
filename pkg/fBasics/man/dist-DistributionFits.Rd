\name{DistributionFits}

\alias{DistributionFits}

\alias{fDISTFIT}
\alias{fDISTFIT-class}

\alias{show,fDISTFIT-method}

\alias{nFit}
\alias{tFit}

\alias{stableFit}

\alias{print.fDISTFIT}

\title{Parameter Fit of a Distribution}


\description{

    A collection and description of moment and maximum 
    likelihood estimators to fit the parameters of a 
    distribution. 
    \cr

    The functions are:
    
    \tabular{ll}{
    \code{nFit} \tab MLE parameter fit for a normal distribution, \cr
    \code{tFit} \tab MLE parameter fit for a Student t-distribution, \cr
    \code{stableFit} \tab MLE and Quantile Method stable parameter fit. }

}


\usage{
nFit(x, doplot = TRUE, span = "auto", title = NULL, description = NULL, \dots)

tFit(x, df = 4, doplot = TRUE, span = "auto", trace = FALSE, title = NULL, 
    description = NULL, \dots)
    
stableFit(x, alpha = 1.75, beta = 0, gamma = 1, delta = 0, 
    type = c("q", "mle"), doplot = TRUE, algorithm = c("nlminb", "nlm"),
    trace = FALSE, title = NULL, description = NULL) 
    
\S4method{show}{fDISTFIT}(object)
}


\arguments{
  
    \item{algorithm}{
        [stableFit] - \cr
        a character string, which optimization algorithm should be
        used for max log-lilelihood estimation.
        }
    \item{alpha, beta, gamma, delta}{
        [stable] - \cr
        The parameters are \code{alpha}, \code{beta}, \code{gamma}, 
        and \code{delta}:\cr
        value of the index parameter \code{alpha} with \code{alpha = (0,2]};
        skewness parameter \code{beta}, in the range [-1, 1];
        scale parameter \code{gamma}; and
        shift parameter \code{delta}.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{df}{
        the number of degrees of freedom for the Student distribution, 
        \code{df > 2}, maybe non-integer. By default a value of 4 is
        assumed.
        }
    \item{object}{
        [show] - \cr
        an S4 class object as returned from the fitting functions.
        }
    \item{doplot}{
        a logical flag. Should a plot be displayed?
        }
    \item{span}{
        x-coordinates for the plot, by default 100 values 
        automatically selected and ranging between the 0.001, 
        and 0.999 quantiles. Alternatively, you can specify
        the range by an expression like \code{span=seq(min, max,
        times = n)}, where, \code{min} and \code{max} are the 
        left and right endpoints of the range, and \code{n} gives 
        the number of the intermediate points.
        }
    \item{title}{
        a character string which allows for a project title.
        }
    \item{trace}{
        a logical flag. Should the parameter estimation process be
        traced?
        }
    \item{type}{
        a character string which allows to select the method for
        parameter estimation: \code{"mle"}, the maximum log likelihood
        approach, or \code{"qm"}, McCulloch's quantile method.
        }
    \item{x}{
        a numeric vector. 
        }
    \item{\dots}{
        parameters to be parsed.
        }

}


\value{
  
    The functions \code{tFit}, \code{hypFit} and \code{nigFit} return 
    a list with the following components:
  
    \item{estimate}{
        the point at which the maximum value of the log liklihood 
        function is obtained.
        }
    \item{minimum}{
        the value of the estimated maximum, i.e. the value of the
        log liklihood function.
        }
    \item{code}{
        an integer indicating why the optimization process terminated.\cr
        1: relative gradient is close to zero, current iterate is probably 
            solution; \cr
        2: successive iterates within tolerance, current iterate is probably 
            solution; \cr
        3: last global step failed to locate a point lower than \code{estimate}. 
            Either \code{estimate} is an approximate local minimum of the 
            function or \code{steptol} is too small; \cr
        4: iteration limit exceeded; \cr
        5: maximum step size \code{stepmax} exceeded five consecutive times. 
            Either the function is unbounded below, becomes asymptotic to a 
            finite value from above in some direction or \code{stepmax} 
            is too small.
            }
    \item{gradient}{
        the gradient at the estimated maximum.
        }
    \item{steps}{
        number of function calls.
        }
        
    Remark: The parameter estimation for the stable distribution via the 
    maximum Log-Likelihood approach may take a quite long time.
              
}


\details{
    
    \bold{Stable Parameter Estimation:}
    
    Estimation techniques based on the quantiles of an empirical sample 
    were first suggested by Fama and Roll [1971]. However their technique 
    was limited to symmetric distributions and suffered from a small 
    asymptotic bias. McCulloch [1986] developed a technique that uses 
    five quantiles from a sample to estimate \code{alpha} and \code{beta}
    without asymptotic bias. Unfortunately, the estimators provided by
    McCulloch have restriction \code{alpha>0.6}.
       
}


\examples{    
## nFit -
   # Simulate random normal variates N(0.5, 2.0):
   set.seed(1953)
   s = rnorm(n = 1000, 0.5, 2) 

## nigFit -  
   # Fit Parameters:
   nFit(s, doplot = TRUE) 
}


\keyword{distribution}

