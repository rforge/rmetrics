\name{snorm}


\alias{snorm}

\alias{dsnorm}
\alias{psnorm}
\alias{qsnorm}
\alias{rsnorm}

\alias{normFit}
\alias{snormFit}

\alias{snormSlider}


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
    \code{[dpqr]snorm} \tab Skew Normal Distribution, \cr
    \code{normFit} \tab MLE parameter fit for Normal distribution, \cr
    \code{snormFit} \tab MLE parameter fit for skew Normal distribution, \cr
    \code{snormSlider} \tab Displays interactively skew Normal distribution. }
       
}


\usage{
dsnorm(x, mean = 0, sd = 1, xi = 1.5)
psnorm(q, mean = 0, sd = 1, xi = 1.5)
qsnorm(p, mean = 0, sd = 1, xi = 1.5)
rsnorm(n, mean = 0, sd = 1, xi = 1.5)

normFit(x, \dots)
snormFit(x, \dots)

snormSlider(type = c("dist", "rand"))
}


\arguments{

    \item{mean, sd, xi}{
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
    
    \code{[s]normFit} returns a list with the following components: 
    \cr
    \item{par}{
        The best set of parameters found. 
        }
    \item{objective}{
        The value of objective corresponding to \code{par}.
        }
    \item{convergence}{
        An integer code. 0 indicates successful convergence.
        }  
    \item{message}{
        A character string giving any additional information 
        returned by the optimizer, or NULL. For details, see 
        PORT documentation.
        }  
    \item{iterations}{
        Number of iterations performed.
        } 
    \item{evaluations}{
        Number of objective function and gradient function 
        evaluations.
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
   
   # Ranbdom Numbers:
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
   snormFit(r)
   
\dontrun{
## snormSlider -
   snormSlider("dist")
   snormSlider("rand")
}
}


\keyword{distribution}

