\name{sged}

\alias{sged}

\alias{dged}
\alias{pged}
\alias{qged}
\alias{rged}

\alias{dsged}
\alias{psged}
\alias{qsged}
\alias{rsged}

\alias{gedFit}
\alias{sgedFit}

\alias{sgedSlider}


\title{Skew GED Distribution and Parameter Estimation}


\description{
    
    \description{
    
    Functions to compute density, distribution function, 
    quantile function and to generate random variates 
    for the generalized error distribution. In
    addition maximum likelihood estimators are available 
    to fit the parameters of the distribution.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{[dpqr]ged} \tab Symmetric GED Distribution, \cr
    \code{[dpqr]sged} \tab Skew GED Distribution, \cr
    \code{gedFit} \tab MLE parameter fit for a GED distribution, \cr
    \code{sgedFit} \tab MLE parameter fit for a skew GED distribution, \cr
    \code{sgedSlider} \tab Displays interactively skew GED distribution. }
      
}
    
    
}


\usage{
dged(x, mean = 0, sd = 1, nu = 2)
pged(q, mean = 0, sd = 1, nu = 2)
qged(p, mean = 0, sd = 1, nu = 2)
rged(n, mean = 0, sd = 1, nu = 2)

dsged(x, mean = 0, sd = 1, nu = 2, xi = 1.5)
psged(q, mean = 0, sd = 1, nu = 2, xi = 1.5)
qsged(p, mean = 0, sd = 1, nu = 2, xi = 1.5)
rsged(n, mean = 0, sd = 1, nu = 2, xi = 1.5)

gedFit(x, \dots)
sgedFit(x, \dots)

sgedSlider(type = c("dist", "rand"))
}


\arguments{

    \item{mean, sd, nu, xi}{
        location parameter \code{mean},
        scale parameter \code{sd},
        shape parameter \code{nu}, 
        skewness parameter \code{xi}.
        }
    \item{n}{
        the number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{type}{
        a character string denoting which interactive plot should
        be displayed. Either a distribution plot \code{type="dist"},
        the default value, or a random variates plot, \code{type="rand"}.
        }
    \item{x, q}{
        a numeric vector of quantiles.
        }
    \item{\dots}{
        parameters parsed to the optimization function \code{nlm}. 
        }
    
}


\value{

    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates, \cr
    all values are numeric vectors.
    
    \code{*Fit} returns a list with the following components: \cr
    \item{estimate}{
        the point at which the maximum value of the log liklihood 
        function is obtained.
        }
    \item{objective}{
        the value of the estimated maximum, i.e. the value of the
        log liklihood function.
        }
    \item{message}{
        an integer indicating why the optimization process terminated.
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
    
}


\details{
    
    \bold{Parameter Estimation:}
    \cr\cr
    The function \code{\link{nlm}} is used to minimize the "negative" maximum 
    log-likelihood function. \code{nlm} carries out a minimization using 
    a Newton-type algorithm.
    
}


\references{

Nelson D.B. (1991);
    \emph{Conditional Heteroscedasticity in Asset Returns: A New Approach},
    Econometrica, 59, 347--370.
    
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## sged -
   par(mfrow = c(2, 2))
   set.seed(1953)
   r = rsged(n = 1000)
   plot(r, type = "l", main = "sged", col = "steelblue")
   
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   box()
   x = seq(min(r), max(r), length = 201)
   lines(x, dsged(x), lwd = 2)
   
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue",
     ylab = "Probability")
   lines(x, psged(x), lwd = 2)
   
   # Compute quantiles:
   round(qsged(psged(q = seq(-1, 5, by = 1))), digits = 6)
       
## sgedFit -
   sgedFit(r, print.level = 2)
}


\keyword{distribution}

