\name{nig}


\alias{nig}
\alias{dnig}
\alias{pnig}
\alias{qnig}
\alias{rnig}


\title{Normal Inverse Gaussian Distribution}


\description{
    
    Density, distribution function, quantile function 
    and random generation for the normal inverse
    Gaussian distribution.
    
}


\usage{
dnig(x, alpha = 1, beta = 0, delta = 1, mu = 0, log = FALSE)
pnig(q, alpha = 1, beta = 0, delta = 1, mu = 0)
qnig(p, alpha = 1, beta = 0, delta = 1, mu = 0)
rnig(n, alpha = 1, beta = 0, delta = 1, mu = 0)
}


\arguments{
 
    \item{alpha, beta, delta, mu}{
        shape parameter \code{alpha};
        skewness parameter \code{beta}, \code{abs(beta)} is in the 
        range (0, alpha);
        scale parameter \code{delta}, \code{delta} must be zero or 
        positive; 
        location parameter \code{mu}, by default 0.
        These are the parameters in the first parameterization.
        }
    \item{log}{
        a logical flag by default \code{FALSE}. 
        Should labels and a main title drawn to the plot?
        }
    \item{n}{
        number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{x, q}{
        a numeric vector of quantiles.
        }
    
}


\value{

    All values for the \code{*nig} functions are numeric vectors: 
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.
    
}


\details{
  
    The random deviates are calculated with the method described by 
    Raible (2000).
    
}


\author{

    David Scott for code implemented from \R's 
    contributed package \code{HyperbolicDist}.
    
}


\references{

Atkinson, A.C. (1982); 
    \emph{The simulation of generalized inverse Gaussian and hyperbolic 
    random variables},
    SIAM J. Sci. Stat. Comput. 3, 502--515. 

Barndorff-Nielsen O. (1977);
    \emph{Exponentially decreasing distributions for the logarithm of 
    particle size}, 
    Proc. Roy. Soc. Lond., A353, 401--419. 

Barndorff-Nielsen O., Blaesild, P. (1983); 
    \emph{Hyperbolic distributions. In Encyclopedia of Statistical 
    Sciences}, 
    Eds., Johnson N.L., Kotz S. and Read C.B., 
    Vol. 3, pp. 700--707. New York: Wiley. 

Raible S. (2000);
    \emph{Levy Processes in Finance: Theory, Numerics and Empirical Facts},
    PhD Thesis, University of Freiburg, Germany, 161 pages.
    
}


\examples{   
## nig -
   set.seed(1953)
   r = rnig(5000, alpha = 1, beta = 0.3, delta = 1)
   plot(r, type = "l", col = "steelblue",
     main = "nig: alpha=1 beta=0.3 delta=1")
 
## nig - 
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   x = seq(-5, 5, 0.25)
   lines(x, dnig(x, alpha = 1, beta = 0.3, delta = 1))
 
## nig -  
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, pnig(x, alpha = 1, beta = 0.3, delta = 1))
   
## nig -
   # Compute Quantiles:
   qnig(pnig(seq(-5, 5, 1), alpha = 1, beta = 0.3, delta = 1), 
     alpha = 1, beta = 0.3, delta = 1) 
}


\keyword{distribution}

