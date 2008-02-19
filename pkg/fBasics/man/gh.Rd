\name{gh}

\alias{gh}

\alias{dgh}
\alias{pgh}
\alias{qgh}
\alias{rgh}


\title{Generalized Hyperbolic Distribution}


\description{
    
    Density, distribution function, quantile function 
    and random generation for the generalized
    hyperbolic distribution.
    
}


\usage{
dgh(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1, log = FALSE)
pgh(q, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
qgh(p, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
rgh(n, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
}


\arguments{

    \item{alpha, beta, delta, mu, lambda}{
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
    \item{\dots}{
        arguments to be passed to the function \code{integrate}.
        }
    
}


\value{

    All values for the \code{*gh} functions are numeric vectors: 
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.
    
}


\details{
  
    The generator \code{rgh} is based on the GH algorithm given 
    by Scott (2004).
 
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


\author{

    David Scott for code implemented from \R's 
    contributed package \code{HyperbolicDist}.
    
}


\examples{   
## rgh -
   set.seed(1953)
   r = rgh(5000, alpha = 1, beta = 0.3, delta = 1)
   plot(r, type = "l", col = "steelblue",
     main = "gh: alpha=1 beta=0.3 delta=1")
 
## dgh - 
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   x = seq(-5, 5, 0.25)
   lines(x, dgh(x, alpha = 1, beta = 0.3, delta = 1))
 
## pgh -  
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, pgh(x, alpha = 1, beta = 0.3, delta = 1))
   
## qgh -
   # Compute Quantiles:
   qgh(pgh(seq(-5, 5, 1), alpha = 1, beta = 0.3, delta = 1), 
     alpha = 1, beta = 0.3, delta = 1) 
}


\keyword{distribution}

