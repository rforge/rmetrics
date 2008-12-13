\name{hypFit}

\alias{hypFit}

\title{Fit of a Hyperbolic Distribution}


\description{

    Estimates the parameters of a hyperbolic distribution.

}


\usage{    
hypFit(x, alpha = 1, beta = 0, delta = 1, mu = 0, 
    scale = TRUE, doplot = TRUE, span = "auto", trace = TRUE, 
    title = NULL, description = NULL, \dots) 
}


\arguments{
  
    \item{alpha, beta, delta, mu}{
        \code{alpha} is a shape parameter by default 1,
        \code{beta} is a skewness parameter by default 0, 
            note \code{abs(beta)} is in the range (0, alpha),
        \code{delta} is a scale parameter by default 1, 
            note, \code{delta} must be zero or positive, and
        \code{mu} is a location parameter, by default 0.
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
    \item{doplot}{
        a logical flag. Should a plot be displayed?
        }
    \item{scale}{
        a logical flag, by default \code{TRUE}. Should the time series
        be scaled by its standard deviation to achieve a more stable
        optimization?
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

    The function \code{\link{nlm}} is used to minimize the "negative" 
    maximum log-likelihood function. \code{nlm} carries out a minimization 
    using a Newton-type algorithm.
       
}


\examples{    
## rhyp -
   # Simulate random variates HYP(1.5, 0.3, 0.5, -1.0):
   set.seed(1953)
   s = rhyp(n = 1000, alpha = 1.5, beta = 0.3, delta = 0.5, mu = -1.0) 

## hypFit -  
   # Fit Parameters, this may take some time ...
   # Starting vector (1, 0, 1, mean(s)):
   hypFit(s, alpha = 1, beta = 0, delta = 1, mu = mean(s), doplot = TRUE) 
}


\keyword{distribution}

