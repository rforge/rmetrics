\name{snorm}


\alias{snorm}

\alias{dsnorm}
\alias{psnorm}
\alias{qsnorm}
\alias{rsnorm}

\alias{normFit}
\alias{snormFit}


\title{Skew Normal Distribution and Parameter Estimation}


\description{
    
    Functions to compute density, distribution function, 
    quantile function and to generate random variates 
    for the skew normal distribution. In
    addition maximum likelihood estimators are available to 
    fit the parameters of the distribution.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{[dpqr]std} \tab Symmetric Student-t Distribution, \cr
    \code{[dpqr]snorm} \tab Skew Student-t Distribution, \cr
    \code{stdFit} \tab MLE parameter fit for a Sudent-t distribution, \cr
    \code{snormFit} \tab MLE parameter fit for a skew Sudent-t distribution. }
       
}


\usage{
dsnorm(x, mean = 0, sd = 1, xi = 1.5)
psnorm(q, mean = 0, sd = 1, xi = 1.5)
qsnorm(p, mean = 0, sd = 1, xi = 1.5)
rsnorm(n, mean = 0, sd = 1, xi = 1.5)

normFit(x, \dots)
snormFit(x, \dots)
}


\arguments{

    \item{mean, sd, nu, xi}{
        location parameter \code{mean},
        scale parameter \code{sd},
        skewness parameter \code{xi}.
        }
    \item{n}{
        the number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
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
    
    \code{*Fit} return a list with the following components: \cr
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
  
    \bold{Symmetric Normal Distibution:}
    \cr\cr
    The functions for the normal distribution are part of R's
    base package. The functions for the symmetric Student-t 
    distribution are rescaled in such a way that they have unit 
    variance in contrast to the Student-t family \code{dt}, \code{pt}, 
    \code{qt} and \code{rt} which are part of R's base package.
    The generalized error distribution functions are defined as 
    described by Nelson (1991).
    \cr
    
    \bold{Skew Normal Distribution:}
    \cr\cr
    The skew normal distribution functions are defined as described
    by Fernandez and Steel (2000).
    cr
    
    \bold{Parameter Estimation:}
    \cr\cr
    The function \code{\link{nlm}} is used to minimize the "negative" maximum 
    log-likelihood function. \code{nlm} carries out a minimization using 
    a Newton-type algorithm.
    
}


\references{
    
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## snorm -
   par(mfrow = c(2, 2))
   set.seed(1953)
   r = rsnorm(n = 1000)
   plot(r, type = "l", main = "snorm", col = "steelblue")
   
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   box()
   x = seq(min(r), max(r), length = 201)
   lines(x, dsnorm(x), lwd = 2)
   
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue",
     ylab = "Probability")
   lines(x, psnorm(x), lwd = 2)
   
   # Compute quantiles:
   round(qsnorm(psnorm(q = seq(-1, 5, by = 1))), digits = 6)
       
## snormFit -
   snormFit(r, print.level = 2)
}


\keyword{distribution}

