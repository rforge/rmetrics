\name{DistributionFits}

\alias{DistributionFits}

\alias{fDISTFIT}
\alias{fDISTFIT-class}

\alias{show.fDISTFIT}
\alias{show,fDISTFIT-method}

\alias{nFit}
\alias{tFit}

\alias{stableFit}

\alias{ghFit}
\alias{hypFit}
\alias{nigFit}
\alias{print.fDISTFIT}

\title{Parameter Fit of a Distribution}


\description{

    A collection and description of moment and maximum 
    likelihood estimators to fit the parameters of a 
    distribution. Included are estimators for the Student-t, 
    for the stable, for the generalized hyperbolic hyperbolic, 
    for the normal inverse Gaussian, and for empirical 
    distributions. 
    \cr

    The functions are:
    
    \tabular{ll}{
    \code{nFit} \tab MLE parameter fit for a normal distribution, \cr
    \code{tFit} \tab MLE parameter fit for a Student t-distribution, \cr
    \code{stableFit} \tab MLE and Quantile Method stable parameter fit, \cr
    \code{ghFit} \tab MLE parameter fit for a generalized hyperbolic distribution, \cr
    \code{hypFit} \tab MLE parameter fit for a hyperbolic distribution, \cr
    \code{nigFit} \tab MLE parameter fit for a normal inverse Gaussian distribution. }

}


\usage{
nFit(x, doplot = TRUE, span = "auto", title = NULL, description = NULL, \dots)
tFit(x, df = 4, doplot = TRUE, span = "auto", trace = FALSE, title = NULL, 
    description = NULL, \dots)
    
stableFit(x, alpha = 1.75, beta = 0, gamma = 1, delta = 0, 
    type = c("q", "mle"), doplot = TRUE, trace = FALSE, title = NULL, 
    description = NULL)
    
ghFit(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1, doplot = TRUE, 
    span = "auto", trace = FALSE, title = NULL, description = NULL, \dots) 
hypFit(x, alpha = 1, beta = 0, delta = 1, mu = 0, doplot = TRUE, 
    span = "auto", trace = FALSE, title = NULL, description = NULL, \dots)
nigFit(x, alpha = 1, beta = 0, delta = 1, mu = 0, doplot = TRUE, 
    span = "auto", trace = FALSE, title = NULL, description = NULL, \dots)
   
show.fDISTFIT(object)
}


\arguments{
  
    \item{alpha, beta, gamma, delta, mu, lambda}{
        [stable] - \cr
        The parameters are \code{alpha}, \code{beta}, \code{gamma}, 
        and \code{delta}:\cr
        value of the index parameter \code{alpha} with \code{alpha = (0,2]};
        skewness parameter \code{beta}, in the range [-1, 1];
        scale parameter \code{gamma}; and
        shift parameter \code{delta}.
        \cr
        [hyp] - \cr
        The parameters are \code{alpha}, \code{beta}, \code{delta}, 
        \code{mu}, and and \code{lambda}:\cr
        shape parameter \code{alpha};
        skewness parameter \code{beta}, \code{abs(beta)} is in the 
        range (0, alpha);
        scale parameter \code{delta}, \code{delta} must be zero or 
        positive; 
        location parameter \code{mu}, by default 0;
        and lambda parameter \code{lambda}, by default 1.
        These is the meaning of the parameters in the first 
        parameterization \code{pm=1} which is the default 
        parameterization selection.
        In the second parameterization, \code{pm=2} \code{alpha}
        and \code{beta} take the meaning of the shape parameters
        (usually named) \code{zeta} and \code{rho}.
        In the third parameterization, \code{pm=3} \code{alpha}
        and \code{beta} take the meaning of the shape parameters
        (usually named) \code{xi} and \code{chi}.
        In the fourth parameterization, \code{pm=4} \code{alpha}
        and \code{beta} take the meaning of the shape parameters
        (usually named) \code{a.bar} and \code{b.bar}.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{df}{
        [tFit] - \cr
        the number of degrees of freedom for the Student distribution, 
        \code{df > 2}, maybe non-integer. By default a value of 4 is
        assumed.
        }
    \item{object}{
        [show] - \cr
        an S4 class object as returned from the fitting functions.
        }
    \item{doplot}{
        [tFit][hypFit][nigFit] - \cr
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
        [*Fit] - \cr
        a logical flag. Should the parameter estimation process be
        traced?
        }
    \item{type}{
        a character string which allows to select the method for
        parameter estimation: \code{"mle"}, the maximum log likelihood
        approach, or \code{"qm"}, McCulloch's quantile method.
        }
    \item{x}{
        [*Fit] - \cr
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

    \bold{Maximum Likelihood Estimation:}
    \cr
    The function \code{\link{nlm}} is used to minimize the "negative" 
    maximum log-likelihood function. \code{nlm} carries out a minimization 
    using a Newton-type algorithm.
    \cr\cr
       
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.2D-DistributionFits")
   
## Plot:
   par(ask = FALSE)
   
## nigFit -
   # Simulate random variates HYP(1.5, 0.3, 0.5, -1.0):
   set.seed(1953)
   s = rnig(n = 1000, alpha = 1.5, beta = 0.3, delta = 0.5, mu = -1.0) 

## nigFit -  
   # Fit Parameters:
   # Note, this may take some time.
   # Starting vector (1, 0, 1, mean(s)):
   nigFit(s, alpha = 1, beta = 0, delta = 1, mu = mean(s), doplot = TRUE) 
}


\keyword{distribution}

