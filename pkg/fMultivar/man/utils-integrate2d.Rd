\name{utils-integrate2d}


\alias{integrate2d}


\title{Bivariate Integration Tools}


\description{
    
    Integrates over the unit square.

}


\usage{
integrate2d(fun, error = 1.0e-5, \dots)
}


\arguments{

    \item{fun}{
        the function to be integrated. The first argument requests
        the x values, the second the y values, and the remaining
        are reserved for additional parameters. The integration is 
        over the unit square "[0,1]\^2".
    }
        \item{error}{
        the error bound to be achieved by the integration formula.
        A numeric value.
        }
    \item{\dots}{
        parameters passed to the function to be integrated.     
    }
        
}


\value{

    \code{integrate2d}
    returns a list with the \code{$value} of the integral over the
    unit square [0,1]\^2, an \code{$error} estimate and the number 
    of grid \code{$points} used by the integration function. 
    
}


\references{
    
Azzalini A., (2004);
    \emph{The sn Package};
    R Reference Guide available from www.r-project.org.
    
Venables W.N., Ripley B.D., (2002); 
    \emph{Modern Applied Statistics with S},
    Fourth Edition, Springer.
    
Warnes G.R., (2004);
    \emph{The gregmisc Package};
    R Reference Guide available from www.r-project.org.
         
}


\author{

    W.N. Venables and B.D. Ripley for the underlying kde2d function, \cr
    Gregory R. Warnes for the underlying hist2d function, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{math}

