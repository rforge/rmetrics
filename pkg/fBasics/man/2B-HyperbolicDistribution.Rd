\name{HyperbolicDistribution}

\alias{HyperbolicDistribution}

\alias{dgh}
\alias{pgh}
\alias{qgh}
\alias{rgh}

\alias{dhyp}
\alias{phyp}
\alias{qhyp}
\alias{rhyp}

\alias{hypMode}

\alias{dnig}
\alias{pnig}
\alias{qnig}
\alias{rnig}

\alias{hypSlider}
\alias{nigSlider}


\title{Generalized Hyperbolic Distribution}


\description{
    
    A collection and description of functions to compute
    density, distribution function, quantile function
    and to generate random variates for three cases
    of the generalized hyperbolic distribution: the 
    generalized hyperbolic distribution itself, the 
    hperbolic distribution and the normal inverse Gaussian 
    distribution.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{[dpqr]gh} \tab The generalized hyperbolic distribution, \cr
    \code{[dpqr]hyp} \tab The hyperbolic distribution, \c
    \code{[dpqr]nig} \tab The normal inverse Gaussian distribution, \cr
    \code{hypMode} \tab the hyperbolic mode, \cr
    \code{hypSlider} \tab interactive hyperbolic distribution display, \cr
    \code{nigSlider} \tab interactive NIG distribution display. }
    
}


\usage{
dgh(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
pgh(q, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
qgh(p, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
rgh(n, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)

dhyp(x, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4))
phyp(q, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4), ...)
qhyp(p, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4), ...)
rhyp(n, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4))

dnig(x, alpha = 1, beta = 0, delta = 1, mu = 0)
pnig(q, alpha = 1, beta = 0, delta = 1, mu = 0)
qnig(p, alpha = 1, beta = 0, delta = 1, mu = 0)
rnig(n, alpha = 1, beta = 0, delta = 1, mu = 0)

hypMode(alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4))

hypSlider()
nigSlider()
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
    \item{n}{
        number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{pm}{
        an integer value between \code{1} and \code{4} for the 
        selection of the parameterization. The default takes the
        first parameterization.       
        }
    \item{x, q}{
        a numeric vector of quantiles.
        }
    \item{\dots}{
        arguments to be passed to the function \code{integrate}.
        }
    
}


\value{

    All values for the \code{*gh}, \code{*hyp}, and \code{*nig} 
    functions are numeric vectors: 
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.
    
    The function \code{hyp*Mode} returns the mode in the appropriate
    parameterization. A numeric value.
    
    The functions \code{hypSlider} and \code{nigSlider} 
    display for educational purposes the densities and probabilities
    of the hyperbolic and normal inverse Gaussian distributions.
    
}


\details{
  
    \bold{Generalized Hyperbolic Distibution:}
    \cr\cr
    The generator \code{rgh} is based on the GH algorithm given 
    by Scott (2004).
    \cr
    
    \bold{Hyperbolic Distibution:}
    \cr\cr
    The generator \code{rhyp} is based on the HYP algorithm given 
    by Atkinson (1982).
    \cr
    
    \bold{Normal Inverse Gaussian Distribution:}
    \cr\cr
    The random deviates are calculated with the method described by 
    Raible (2000).
    
}


\note{

    An undocumented R function for the modified Bessel function 
    K1 named \code{.BesselK1(X)} is availalble, which is called
    by the S-Plus version of the program. 
}


\author{

    David Scott for the HYP Generator from R's "HyperbolicDist" package, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
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

