\name{BesselFunctions}

\alias{BesselFunctions}

\alias{BesselI}
\alias{BesselK}
\alias{BesselDI}
\alias{BesselDK}


\title{Modified Bessel Functions}


\description{

    A collection and description of special mathematical
    functions which compute the modified Bessel functions
    of integer order of the first and second kind as well 
    as their derivatives.     
    \cr     

    The functions are:

    \tabular{ll}{
    \code{BesselI} \tab modified Bessel function of the 1st Kind, \cr
    \code{BesselDI} \tab its derivative, \cr
    \code{BesselK} \tab the modified Bessel function of the 3nd Kind, \cr
    \code{BesselDK} \tab its derivative. }   
    
}


\usage{
BesselI(x, nu, expon.scaled = FALSE) 
BesselK(x, nu, expon.scaled = FALSE)
BesselDI(x, nu) 
BesselDK(x, nu)
}

\arguments{
  
    \item{expon.scaled}{
        a logical; if TRUE, the results are exponentially scaled.
        }
    \item{nu}{
        an integer value greater or equal to zero, the integer
        order of the modified Bessel function.
        }
    \item{x}{
        a positive numeric value or a vector of positive numerical 
        values.   
        }
}


\value{
  
    The functions return the values of the selected special mathematical
    function.
 
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\references{

Abramowitz M., Stegun I.A. (1972); 
    \emph{Handbook of Mathematical Functions with Formulas, Graphs, 
        and Mathematical Tables}, 
    9th printing, New York, Dover Publishing.  

Weisstein E.W. (2004);
    \emph{MathWorld -- A Wolfram Web Resource},
    http://mathworld.wolfram.com

}


\examples{
## SOURCE("fOptions.083D-BesselFunctions")

## Bessel I0 and K0 - 
   # Abramowitz-Stegun: Table 9.8, p. 416-422
   x = c(0.0, 0.01, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50)  
   data.frame(x, I = exp(-x)*BesselI(x, 0), K = exp(x)*BesselK(x, 0)) 
   # Compare with R's internal function:
   # data.frame(x, ratio = BesselI(x, 0) / besselI(x, 0))
   # data.frame(x, ratio = BesselK(x, 0) / besselK(x, 0))
   
## x = 0:
   c(BesselI(0, 0), BesselI(0, 1), BesselI(0, 2), BesselI(0, 5))
   # Compare with R's internal function:
   # c(besselI(0, 0), besselI(0, 1), besselI(0, 2), besselI(0, 5))
   c(BesselK(0, 0), BesselK(0, 1), BesselK(0, 2), BesselK(0, 5))
   # Compare with R's internal function:
   # c(besselK(0, 0), besselK(0, 1), besselK(0, 2), besselK(0, 5))
   
## Bessel I2 and K2 - 
   # Abramowitz-Stegun: Table 9.8, p. 416-422
   x = c(0.0, 0.01, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50)  
   data.frame(x, I = BesselI(x, 2)/x^2, K = BesselK(x, 2)*x^2) 
   # Compare with R's internal function:
   # data.frame(x, ratio = BesselI(x, 0) / besselI(x, 0))
   # data.frame(x, ratio = BesselK(x, 0) / besselK(x, 0))
   # data.frame(x, ratio = BesselI(x, 1) / besselI(x, 1))
   # data.frame(x, ratio = BesselK(x, 1) / besselK(x, 1))
   # data.frame(x, ratio = BesselI(x, 5) / besselI(x, 5))
   # data.frame(x, ratio = BesselK(x, 5) / besselK(x, 5))
   # data.frame(x, ratio = BesselI(x,50) / besselI(x,50))
   # data.frame(x, ratio = BesselK(x,50) / besselK(x,50))
}


\keyword{math}

