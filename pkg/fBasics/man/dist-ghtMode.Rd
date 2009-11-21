\name{ghtMode}


\alias{ghtMode}


\title{Generalized Hyperbolic Student-t Mode}


\description{
    
    Computes the mode of the generalized hyperbolic 
    Student-t distribution.
    
}


\usage{
ghtMode(beta = 0.1, delta = 1, mu = 0, nu = 10)
}


\arguments{

    \item{beta, delta, mu}{
        skewness parameter \code{beta}, \code{abs(beta)} is in the 
        range (0, alpha);
        scale parameter \code{delta}, \code{delta} must be zero or 
        positive; 
        location parameter \code{mu}, by default 0.
        These are the parameters in the first parameterization.
        }
    \item{nu}{
        a numeric value, the number of degrees of freedom.
        }
    
}


\value{
    
    returns the mode for the generalized hyperbolic Student-t
    distribution. A numeric value.
    
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
## ghtMode -
   ghtMode()
}


\keyword{distribution}

