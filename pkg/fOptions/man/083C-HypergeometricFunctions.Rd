\name{HypergeometricFunctions}

\alias{HypergeometricFunctions}

\alias{kummerM}
\alias{kummerU}
\alias{whittakerM}
\alias{whittakerW}
\alias{hermiteH}


\title{Confluent Hypergeometric Functions}


\description{

    A collection and description of special mathematical
    functions which compute the confluent hypergeometric 
    and related  functions. For example, these functions 
    are required to valuate Asian Options based on the 
    theory of exponential Brownian motion.     
    \cr     

    The functions are:

    \tabular{ll}{
    \code{kummerM} \tab the Confluent Hypergeometric Function of the 1st Kind, \cr
    \code{kummerU} \tab the Confluent Hypergeometric Function of the 2nd Kind, \cr
    \code{whittakerM} \tab the Whittaker M Function, \cr
    \code{whittakerW} \tab the Whittaker W Function, \cr
    \code{hermiteH} \tab the Hermite Polynomials. }   
    
}


\usage{
kummerM(x, a, b, lnchf = 0, ip = 0) 
kummerU(x, a, b, ip = 0)
whittakerM(x, kappa, mu, ip = 0) 
whittakerW(x, kappa, mu, ip = 0)
hermiteH(x, n, ip = 0)
}

\arguments{
  
    \item{x}{
        [kummer*] - \cr
        a complex numeric value or vector.
        }
    \item{a, b}{
        [kummer*] - \cr
        complex numeric indexes of the Kummer functions.
        }
    \item{ip}{
        an integer value that specifies how many array positions are 
        desired, usually 10 is sufficient. Setting \code{ip=0} causes 
        the function to estimate the number of array positions.   
        }
    \item{kappa, mu}{
        complex numeric indexes of the Whittaker functions.
        }
    \item{lnchf}{
        an integer value which selects how the result should be 
        represented.  A \code{0} will return the value in standard 
        exponential form, a \code{1} will return the LOG of the result.
        }
    \item{n}{
        [hermiteH] - \cr
        the index of the Hermite polynomial, a positive integer value.
        }
}


\details{

    The functions use the TOMS707 Algorithm by M. Nardin, W.F. Perger  
    and A. Bhalla (1989).    
    A numerical evaluator for the confluent hypergeometric function for 
    complex arguments with large magnitudes using a direct summation of 
    the Kummer series. The method used allows an accuracy of up to thirteen      
    decimal places through the use of large real arrays and a single final 
    division.  
                                                                 
    The confluent hypergeometric function is the solution to  
    the differential equation:                                
     
             zf"(z) + (a-z)f'(z) - bf(z) = 0    
             
    The Whittaker functions and the Hermite Polynomials are dervived
    from Kummer's functions.
    
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
## SOURCE("fOptions.083N-HypergeometricFunctions")

## kummerM - 
   # Abramowitz-Stegun: Formula 13.6.3/13.6.21
   x = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)  
   nu = 1; a = nu+1/2; b = 2*nu+1
   M = Re ( kummerM(x = 2*x, a = a, b = b) )
   Bessel = gamma(1+nu) * exp(x)*(x/2)^(-nu) * BesselI(x, nu)
   cbind(x, M, Bessel) 

## kummerM -
   # Abramowitz-Stegun: Formula 13.6.14
   x = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)  
   M = Re ( kummerM(2*x, a = 1, b = 2) )
   Sinh = exp(x)*sinh(x)/(x)
   cbind(x, M, Sinh)
   # Now the same for complex x:
   y = rep(1, length = length(x))
   x = complex(real = x, imag = y)
   M = kummerM(2*x, a = 1, b = 2)
   Sinh = exp(x)*sinh(x)/(x)
   cbind(x, M, Sinh)

## kummerU -
   # Abramowitz-Stegun: Formula 13.1.3
   x = c(0.001, 0.01, 0.1, 1, 10, 100, 1000) 
   a = 1/3; b = 2/3
   U = Re ( kummerU(x, a = a, b = b) )
   cbind(x, U)
 
## whittakerM - 
   # Abramowitz-Stegun: Example 13
   AS = c(1.10622, 0.57469)
   W = c(
     whittakerM(x = 1, kappa = 0, mu = -0.4),
     whittakerW(x = 1, kappa = 0, mu = -0.4) )
   data.frame(AS, W)

## kummerM
   # Abramowitz-Stegun: Example 17
   x = seq(0, 16, length = 200)
   plot(x = x, y = kummerM(x, -4.5, 1), type = "l", ylim = c(-25,125),
     main = "Figure 13.2:  M(-4.5, 1, x)")
   lines(x = c(0, 16), y = c(0, 0), col = 2)
}


\keyword{math}

