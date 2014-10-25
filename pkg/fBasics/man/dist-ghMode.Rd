\name{ghMode}


\alias{ghMode}


\title{Generalized Hyperbolic Mode}


\description{
    
    Computes the mode of the generalized hyperbolic function.
    
}


\usage{
ghMode(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
}


\arguments{

    \item{alpha, beta, delta, mu, lambda}{
        shape parameter \code{alpha};
        skewness parameter \code{beta}, \code{abs(beta)} is in the 
        range (0, alpha);
        scale parameter \code{delta}, \code{delta} must be zero or 
        positive; 
        location parameter \code{mu}, by default 0.
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
    
}


\value{
    
    returns the mode for the generalized hyperbolic distribution. 
    A numeric value.
    
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
## ghMode -
   ghMode()
}


\keyword{distribution}

