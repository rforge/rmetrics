\name{sstd}

\alias{std}

\alias{dstd}
\alias{pstd}
\alias{qstd}
\alias{rstd}

\alias{dsstd}
\alias{psstd}
\alias{qsstd}
\alias{rsstd}

\alias{stdFit}
\alias{sstdFit}

\alias{sstdSlider}


\title{Skew Student-t Distribution and Parameter Estimation}


\description{
    
    Functions to compute density, distribution function, 
    quantile function and to generate random variates 
    for the standardized skew Student-t distribution. In
    addition maximum likelihood estimators are available to 
    fit the parameters of the distribution.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{[dpqr]std} \tab Symmetric Student-t Distribution, \cr
    \code{[dpqr]sstd} \tab Skew Student-t Distribution, \cr
    \code{stdFit} \tab MLE parameter fit for a Sudent-t distribution, \cr
    \code{sstdFit} \tab MLE parameter fit for a skew Sudent-t distribution, \cr
    \code{sstdSlider} \tab Displays interactively skew GED distribution. }
       
}


\usage{
dstd(x, mean = 0, sd = 1, nu = 5)
pstd(q, mean = 0, sd = 1, nu = 5)
qstd(p, mean = 0, sd = 1, nu = 5)
rstd(n, mean = 0, sd = 1, nu = 5)

dsstd(x, mean = 0, sd = 1, nu = 5, xi = 1.5)
psstd(q, mean = 0, sd = 1, nu = 5, xi = 1.5)
qsstd(p, mean = 0, sd = 1, nu = 5, xi = 1.5)
rsstd(n, mean = 0, sd = 1, nu = 5, xi = 1.5)

stdFit(x, \dots)
sstdFit(x, \dots)

sstdSlider(type = c("dist", "rand"))
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
   
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## sstd -
   par(mfrow = c(2, 2))
   set.seed(1953)
   r = rsstd(n = 1000)
   plot(r, type = "l", main = "sstd", col = "steelblue")
   
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   box()
   x = seq(min(r), max(r), length = 201)
   lines(x, dsstd(x), lwd = 2)
   
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue",
     ylab = "Probability")
   lines(x, psstd(x), lwd = 2)
   
   # Compute quantiles:
   round(qsstd(psstd(q = seq(-1, 5, by = 1))), digits = 6)
       
## sstdFit -
   sstdFit(r, print.level = 2)
}


\keyword{distribution}

