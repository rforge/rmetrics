\name{elliptical2d}


\alias{elliptical2d}
\alias{delliptical2d}


\title{Bivariate Elliptical Densities}


\description{
    
    Density function for bivariate elliptical 
    distributions. 
}


\usage{
delliptical2d(x, y = x, rho = 0, param = NULL, type = c("norm", "cauchy", "t", 
    "logistic", "laplace", "kotz", "epower"), output = c("vector", "list"))
}


\arguments{

    \item{output}{
        output - a character string specifying how the output should be 
        formatted. By default a vector of the same length as \code{u} 
        and \code{v}. If specified as \code{"list"} then \code{u} and 
        \code{v} are expected to span a two-dimensional grid as outputted 
        by the function \code{grid2d} and the function returns a list with
        elements \code{$x}, \code{y}, and \code{z} which can be directly 
        used for example by 2D plotting functions.
        }
    \item{param}{
        additional parameters to specify the bivariate density function.
        Only effective for the Kotz and Exponential Power distribution.
        For the Kotz distribution we can specify a numeric value, by default
        defined as \code{param=c(r=sqrt(2))}, and for the Exponential
        Power distribution a numeric vector, by default defined as
        \code{param=c(r=sqrt(2)),s=1/2}.
        }
    \item{rho}{
        the correlation parameter, a numeric value ranging between 
        minus one and one, by default zero.     
        }
    \item{type}{
        the type of the elliptical copula. A character string selected 
        from: \code{"norm"}, \code{"cauchy"}, \code{"t"}, \code{"laplace"}, 
        \code{"kotz"}, or \code{"epower"}.
        }
    \item{x, y}{
        two numeric vectors defining the x and y coordinates. \cr
        }
        
}


\value{
 
    \code{delliptical2d}
    \cr
    returns a two column matrix of densities for the selected bivariate 
    elliptical distribution function.
    
}


\references{
    
Azzalini A., (2004);
    \emph{The sn Package};
    R Reference Guide available from www.r-project.org.
    
Venables W.N., Ripley B.D., (2002); 
    \emph{Modern Applied Statistics with S},
    Fourth Edition, Springer.
         
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## Kotz' Elliptical Density:
   x = (-40:40)/10
   X = grid2d(x)
   z = delliptical2d(X$x, X$y, rho = 0.5, type = "kotz")
   Z = list(x = x, y = x, z = matrix(z, ncol = length(x)))
   persp(Z, theta = -40, phi = 30, col = "steelblue")
}


\keyword{math}

