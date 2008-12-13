\name{nigShapeTriangle}


\alias{nigShapeTriangle}


\title{NIG Shape Triangle}


\description{
    
    Plots the normal inverse Gaussian Shape Triangle.
    
}


\usage{
nigShapeTriangle(object, add = FALSE, labels = TRUE, \dots)
}


\arguments{

    \item{object}{
        an object of class \code{"fDISTFIT"} as returned by the function
        \code{nigFit}.
        } 
    \item{add}{
        a logical value. Should another point added to the NIG
        shape triangle? By default FALSE, a new plot will be created.
        } 
    \item{labels}{
        a logical flag by default \code{TRUE}. 
        Should the logarithm of the density be returned?
        }
    \item{\dots}{
        arguments to be passed to the function \code{integrate}.
        }
    
}


\value{

    displays the parameters of fitted distributions in the
    NIG shape triangle.
    
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
## nigShapeTriangle -
   #
}


\keyword{distribution}

