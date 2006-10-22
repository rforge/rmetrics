\name{GammaFunctions}

\alias{GammaFunctions}

\alias{erf}
\alias{Psi}
\alias{igamma}
\alias{cgamma}
\alias{Pochhammer}


\title{Gamma and Related Functions}


\description{

    A collection and description of special mathematical
    functions. The functions include the error function, 
    the Psi function, the incomplete Gamma function, the 
    Gamma function for complex argument, and the 
    Pochhammer symbol. The Gamma function the logarithm  
    of the Gamma function, their first four derivatives, 
    and the Beta function and the logarithm of the Beta 
    function are part of R's base package. For example,
    these functions are required to valuate Asian Options
    based on the theory of exponential Brownian motion.
    \cr     
    
    The functions are:

    \tabular{ll}{
    \code{erf} \tab the Error function, \cr
    \code{gamma*} \tab the Gamma function, \cr
    \code{lgamma*} \tab the logarithm of the Gamma function, \cr
    \code{digamma*} \tab the first derivative of the Log Gamma function, \cr
    \code{trigamma*} \tab the second derivative of the Log Gamma function, \cr
    \code{tetragamma*} \tab the third derivative of the Log Gamma function, \cr
    \code{pentagamma*} \tab  the fourth derivative of the Log Gammafunction, \cr
    \code{beta*} \tab the Beta function, \cr
    \code{lbeta*} \tab the logarithm of the Beta function, \cr
    \code{Psi} \tab Psi(x) the Psi or Digamma function, \cr
    \code{igamma} \tab P(a,x) the incomplete Gamma function, \cr
    \code{cgamma} \tab Gamma function for complex argument, \cr
    \code{Pochhammer} \tab the Pochhammer symbol. }
                     
    The functions marked by an asterisk are part of R's base
    package.
    
}


\usage{
erf(x)
Psi(x)
igamma(x, a)
cgamma(x, log = FALSE)
Pochhammer(x, n)
}

\arguments{
  
    \item{x}{
        [erf] - \cr
        a real numeric value or vector. \cr
        [Psi][*gamma][Pochhammer] - \cr
        a complex numeric value or vector.
        }
    \item{a}{
        a complex numeric value or vector.
        }
    \item{n}{
        an integer value \code{n >= 0}. A notation used in the theory 
        of special functions for the rising factorial, also known as the 
        rising factorial power, Graham et al. 1994. 
        }
    \item{log}{
        a logical, if \code{TRUE} the logarithm of the complex Gamma 
        function is calculated otherwise if \code{FALSE}, the complex 
        Gamma function itself will be calculated.
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

Artin, E. (1964);
    \emph{The Gamma Function},
    New York, Holt, Rinehart, and Winston Publishing. 

Weisstein E.W. (2004);
    \emph{MathWorld--A Wolfram Web Resource},
    http://mathworld.wolfram.com

}


\examples{
## SOURCE("fOptions.3B-GammaFunctions")

## Calculate Error, Gamma and Related Functions

## gamma -
   # Abramowitz-Stegun: Figure 6.1
   x = seq(-4.01, 4.01, by = 0.011)
   plot(x, gamma(x), ylim = c(-5,5), type = "l", main = "Gamma Function")
   lines(x = c(-4, 4), y = c(0, 0))
     
## Psi -
   # Abramowitz-Stegun: Figure 6.1
   x = seq(-4.01, 4.01, by = 0.011)
   plot(x, Psi(x), ylim = c(-5, 5), type = "l", main = "Psi Function")
   lines(x = c(-4, 4), y = c(0, 0))
   # Note: Is digamma defined for positive values only ?

## igamma -
   # Abramowitz-Stegun: Figure 6.3. 
   gammaStar = function(x, a) { igamma(x,a)/x^a }
   # ... create Figure as an exercise.
  
## igamma -
   # Abramowitz-Stegun: Formula 6.5.12
   # Relation to Confluent Hypergeometric Functions
   a = sqrt(2)
   x = pi
   Re ( (x^a/a) * kummerM(-x, a, 1+a) )
   Re ( (x^a*exp(-x)/a) * kummerM(x, 1, 1+a) )
   pgamma(x,a) * gamma(a)
   igamma(x, a)
 
## cgamma -
   # Abramowitz-Stegun: Tables 6.7
   x = 1
   y = seq(0, 5, by = 0.1); x = rep(x, length = length(y))
   z = complex(real = x, imag = y)
   c = cgamma(z, log = TRUE)
   cbind(y, Re(c), Im(c))
    
## cgamma -
   # Abramowitz-Stegun: Examples 4-8:
   options(digits = 10)
   gamma(6.38); lgamma(56.38)                            # 1/2
   Psi(6.38); Psi(56.38)                                 # 3/4
   cgamma(complex(real = 1, imag = -1), log = TRUE )     # 5
   cgamma(complex(real = 1/2, imag = 1/2), log = TRUE )  # 6
   cgamma(complex(real = 3, imag = 7), log = TRUE )      # 7/8 
}


\keyword{math}

