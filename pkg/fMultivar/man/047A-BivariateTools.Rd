\name{BivariateTools}

\alias{BivariateTools}

\alias{grid2d}
\alias{density2d}
\alias{hist2d}
\alias{integrate2d}

\alias{pnorm2d}
\alias{dnorm2d}
\alias{rnorm2d}


\alias{pcauchy2d}
\alias{dcauchy2d}
\alias{rcauchy2d}

\alias{pt2d}
\alias{dt2d}
\alias{rt2d}

\alias{delliptical2d}


\title{Handling Missing Values}


\description{
    
    A collection and description of functions 
    for managing bivariate data sets. The functions
    include a grid generator, a kernel density 
    estimator, a histogram counter, and a suite of
    bivariate elliptical distribution functions.
    \cr
    
    The functions are:
    \cr
 
    \tabular{ll}{  
    \code{grid2d} \tab Creates from two vectors x-y grid coordinates, \cr
    \code{density2d} \tab returns 2D Kernel Density Estimates, \cr
    \code{hist2d} \tab returns 2D Histogram Counts, \cr
    \code{integrate2d} \tab integrates over a two dimensional unit square, \cr
    \code{pnorm2d} \tab computes bivariate normal distribution function, \cr
    \code{dnorm2d} \tab computes bivariate normal density function, \cr 
    \code{rnorm2d} \tab generates bivariate normal random deviates, \cr
    \code{pcauchy2d} \tab computes bivariate Cauchy distribution function, \cr
    \code{dcauchy2d} \tab computes bivariate Cauchy density function, \cr 
    \code{rcauchy2d} \tab generates bivariate Cauchy random deviates, \cr
    \code{pt2d} \tab computes bivariate Student-t distribution function, \cr
    \code{dt2d} \tab computes bivariate Student-t density function, \cr 
    \code{rt2d} \tab generates bivariate Student-t random deviates, \cr
    \code{delliptical2d} \tab computes  bivariate density for elliptical distributions. }

}


\usage{
grid2d(x = (0:10)/10, y = x)
density2d(x, y = NULL, n = 20, h = NULL, limits = c(range(x), range(y))) 
hist2d(x, y = NULL, n = c(20, 20))
integrate2d(fun, error = 1.0e-5, \dots)

pnorm2d(x, y = x, rho = 0)
dnorm2d(x, y = x, rho = 0)
rnorm2d(n, rho = 0)

pcauchy2d(x, y = x, rho = 0)
dcauchy2d(x, y = x, rho = 0)
rcauchy2d(n, rho = 0)

pt2d(x, y = x, rho = 0, nu = 4)
dt2d(x, y = x, rho = 0, nu = 4)
rt2d(n, rho = 0, nu = 4)

delliptical2d(x, y = x, rho = 0, param = NULL, type = c("norm", "cauchy", "t", 
    "logistic", "laplace", "kotz", "epower"), output = c("vector", "list"))
}


\arguments{

    \item{error}{
        [integrate2d] - \cr
        the error bound to be achieved by the integration formula.
        A numeric value.
        }
    \item{fun}{
        [integrate2d] - \cr
        the function to be integrated. The first argument requests
        the x values, the second the y values, and the remaining
        are reserved for additional parameters. The integration is 
        over the unit square "[0,1]\^2".
        }
    \item{h}{
        [density2d] - \cr
        a vector of bandwidths for \code{x} and \code{y} directions. 
        Defaults to normal reference bandwidth. 
        }
    \item{limits}{
        [hist2d] - \cr
        the limits of the rectangle covered by the grid.   
        }
     \item{n}{
        [density2d] - \cr
        n - an integer specifying the number of grid points in each 
        direction. The default value is 20.\cr
        [hist2D] - \cr
        In this case \code{n} may be a scalar or a two element vector. 
        The default value is 20.\cr
        [rnorm2d] - \cr
        the number of random deviates to be generated, an integer value.
        }
    \item{nu}{
        [*t2d] - \cr
        the number of degrees of freedom, a numeric value ranging between 
        two and infinity, by default four.      
        }
    \item{output}{
        [*elliptical2d] - \cr
        output - a character string specifying how the output should be 
        formatted. By default a vector of the same length as \code{u} 
        and \code{v}. If specified as \code{"list"} then \code{u} and 
        \code{v} are expected to span a two-dimensional grid as outputted 
        by the function \code{grid2d} and the function returns a list with
        elements \code{$x}, \code{y}, and \code{z} which can be directly 
        used for example by 2D plotting functions.
        }
    \item{param}{
        [delliptical2d] - \cr
        additional parameters to specify the bivariate density function.
        Only effective for the Kotz and Exponential Power distribution.
        For the Kotz distribution we can specify a numeric value, by default
        defined as \code{param=c(r=sqrt(2))}, and for the Exponential
        Power distribution a numeric vector, by default defined as
        \code{param=c(r=sqrt(2)),s=1/2}.
        }
    \item{rho}{
        [*norm2d][*cauchy2d][*t2d][*elliptical2d] - \cr
        the correlation parameter, a numeric value ranging between 
        minus one and one, by default zero.     
    }
    \item{type}{
        [*elliptical2d] - \cr
        the type of the elliptical copula. A character string selected 
        from: \code{"norm"}, \code{"cauchy"}, \code{"t"}, \code{"laplace"}, 
        \code{"kotz"}, or \code{"epower"}.
        }
    \item{x, y}{
        [grid2d][*norm2d][*t2d] - \cr
        two numeric vectors defining the x and y coordinates. \cr
        [density2D][hist2D] - \cr
        two vectors of coordinates of data. If \code{y} is NULL then
        \code{x} is assumed to be a two column matrix, where the first 
        column contains the \code{x} data, and the second column the 
        \code{y} data. 
        }
    \item{\dots}{
        [integrate2d] - \cr
        parameters passed to the function to be integrated.     
    }
        
}


\value{

    \code{grid2d} 
    \cr
    returns a list with two vectors named \code{$x} and \code{$y} spanning 
    the grid defined by the coordinate vectors \code{x} and \code{y}.
    
    \code{density2d}\cr
    \code{hist2d}
    \cr
    returns a list with three elements \code{$x}, \code{$y}, and 
    \code{$z}. \code{x} and \code{y} are vectors spanning the two 
    dimensional grid and \code{z} the corresponding matrix. The 
    output can directly serve as input to the plotting functions 
    \code{image}, \code{contour} and \code{persp}.
    
    \code{integrate2d}
    \cr
    returns a list with the \code{$value} of the integral over the
    unit square [0,1]\^2, an \code{$error} estimate and the number 
    of grid \code{$points} used by the integration function. 
    
    \code{pnorm2d}
    \cr
    returns a vector of probabilities for the normal distribution 
    function. 
    \code{pnorm2d}
    \cr
    returns a two column matrix of random deviates generated from
    the bivariate normal distribution function.
    
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


\examples{
## SOURCE("fMultivar.047A-BivariateTools")

## -
}


\author{

    W.N. Venables and B.D. Ripley for the underlying kde2d function, \cr
    Adelchi Azzalini for the underlying pnorm2d function, \cr
    Gregory R. Warnes for the underlying hist2d function, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{math}

