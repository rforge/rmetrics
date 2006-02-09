\name{EBMDistribution}

\alias{EBMDistribution}

\alias{dlognorm}
\alias{plognorm}

\alias{dgam}
\alias{pgam}

\alias{drgam}
\alias{prgam}

\alias{djohnson}
\alias{pjohnson}

\alias{mnorm}
\alias{mlognorm}


\alias{mrgam}
\alias{mjohnson}
\alias{masian}

\alias{derivative}

\alias{dEBM}
\alias{pEBM}
\alias{d2EBM}
\alias{dasymEBM}



\title{Exponential Brownian Motion Distributions}


\description{

    A collection and description of distributions and 
    related functions which are useful in the theory of 
    exponential Brownian motion and Asian option valuation. 
    The functions compute densities and probabilities for 
    the log-Normal distribution, the Gamma distribution, 
    the Reciprocal-Gamma distribution, and the Johnson 
    Type-I distribution. Functions are made available for 
    the compution of moments including the Normal, the 
    log-Normal, the Reciprocal-Gamma, and the Asian-Option 
    Density. In addition a function is given to compute 
    numerically first and second derivatives of a given  
    function.   
    \cr     
    
    The functions are:

    \tabular{ll}{
    \code{dlognorm} \tab the log-Normal density and derivatives, \cr
    \code{plognorm} \tab the log-Normal, a synonyme for R's plnorm, \cr
    \code{dgam} \tab the Gamma density, a synonyme for R's dgamma, \cr
    \code{pgam} \tab the Gamma probability, a synonyme for R's pgamma, \cr
    \code{drgam} \tab the Reciprocal-Gamma density, \cr
    \code{prgam} \tab the Reciprocal-Gamma probability, \cr
    \code{djohnson} \tab the Johnson Type I density, \cr
    \code{pjohnson} \tab the Johnson Type I probability, \cr
    \code{mnorm} \tab the Moments of Normal density, \cr
    \code{mlognorm} \tab the Moments of log-Normal density, \cr
    \code{mrgam} \tab the Moments of reciprocal-Gamma density, \cr
    \code{masian} \tab the Moments of Asian Option density, \cr
    \code{derivative} \tab the First and second numerical derivative. }
            
}


\usage{
dlognorm(x, meanlog = 0, sdlog = 1, deriv = c(0, 1, 2))
plognorm(q, meanlog = 0, sdlog = 1)
dgam(x, alpha, beta)
pgam(q, alpha, beta, lower.tail = TRUE)
drgam(x, alpha, beta, deriv = c(0, 1, 2))
prgam(q, alpha, beta, lower.tail = TRUE)
djohnson(x, a = 0, b = 1, c = 0, d = 1, deriv = c(0, 1, 2))
pjohnson(q, a = 0, b = 1, c = 0, d = 1)

mnorm(mean = 0, sd = 1)
mlognorm(meanlog = 0, sdlog = 1)
mrgam(alpha = 1/2, beta = 1)
mjohnson(a, b, c, d)
masian(Time = 1, r = 0.045, sigma = 0.30)

derivative(x, y, deriv = c(1, 2))

dEBM(u, t = 1)
pEBM(u, t = 1)
d2EBM(u, t = 1)
dasymEBM(u, t = 1)
}

\arguments{
  
    \item{a, b, c, d}{
        [*johnson] - \cr
        the parameters of the Johnson Type I distribution. The default
        values are \code{a=1}, \code{b=1}, \code{c=0}, and \code{d=1}.
        }
    \item{alpha, beta}{
        [*gam] - \cr
        the parameters of the Gamma distribution. 
        }
    \item{deriv}{
        an integer value, the degree of differentiation, either 0, 1 
        or 2.
        }
    \item{lower.tail}{
        a logical, if \code{TRUE}, the default, then the probabilities 
        are \code{P[X <= x]}, otherwise, \code{P[X > x]}.
        }
    \item{mean, sd}{
        [*lognorm] - \cr
        the parameters of the Normal distribution, the mean and the
        standard deviation respectively. The default values are
        \code{mean=0} and \code{sd=1}.
        }
    \item{meanlog, sdlog}{
        [*lognorm] - \cr
        the parameters of the Log Normal distribution, the mean and 
        the standard deviation respectively. The default values are
        \code{mean=0} and \code{sd=1}. 
        }     
    \item{q}{
        a real numeric value or vector.
        } 
    \item{t}{
        ...
        } 
    \item{Time, r, sigma}{
        the parameters of the Asian Option distribution.
        }
    \item{u}{
        ...
        } 
    \item{x}{
        a real numeric value or vector.
        } 
    \item{y}{
        [derivative] - \cr
        a real numeric value or vector, the function values from
        which to compute the first and second derivative.
        }
}


\value{
  
    The functions \code{d*} and \code{p*} return the values or 
    numeric vectors of the density and probability of the the 
    corresponding distribution.
    
    The functions \code{m*} return a list with three elements, 
    the values of the first four moments \code{rawMoments}, 
    the values of the first four central moments \code{centralMoments},
    and the skewness and kurtosis \code{fisher}, also called Fisher
    parameters. 
    
    The function \code{derivative} returns a list of two elemtes, 
    \code{$x} and \code{$y}, where \code{$y($x)} is either the first
    or second derivative of \code{y(x)} as selected by the argument
    \code{deriv}.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fOptions.E1-EBMDistribution")

## Calculate Log-Normal Density and its Derivaties,
   x = exp(seq(-2.8, 1.2, length = 100))
   y0 = dlognorm(x, deriv = 0)
   y1 = dlognorm(x, deriv = 1)
   y2 = dlognorm(x, deriv = 2) 
   # Compare with Numerical Differentiation:
   par(mfrow = c(2, 2))
   xa = exp(seq(-2.5, 1.5, length = 20))
   plot(x, y0, type = "l", main = "Log-Normal Density")
   plot(x, y1, type = "l", main = "1st Derivative")
   z = derivative(xa, dlognorm(xa, deriv = 0), deriv = 1)
   points(z$x, z$y, col = "steelblue4")
   plot(x, y2, type = "l", main = "2nd Derivative")
   z = derivative(xa, dlognorm(xa, deriv = 0), deriv = 2)
   points(z$x, z$y, col = "steelblue4")

## Calculate Reciprocal-Gamma Density and its Derivaties:
   alpha = 2; beta = 1
   x = exp(seq(-2.8, 1.2, length = 100))
   y0 = drgam(x, alpha, beta, deriv = 0)
   y1 = drgam(x, alpha, beta, deriv = 1)
   y2 = drgam(x, alpha, beta, deriv = 2)
   # Compare with Numerical Differentiation:
   par(mfrow = c(2, 2))
   xa = exp(seq(-2.5, 1.5, length = 20))
   plot(x, y0, type = "l", main = "Rec-Gamma Density")
   plot(x, y1, type = "l", main = "1st Derivative")
   z = derivative(xa, drgam(xa, alpha, beta, deriv = 0), deriv = 1)
   points(z$x, z$y, col = "steelblue4")
   plot(x, y2, type = "l", main = "2nd Derivative")
   z = derivative(xa, drgam(xa, alpha, beta, deriv = 0), deriv = 2)
   points(z$x, z$y, col = "steelblue4")
    
## Calculate Johnson-Type-I Density and its Derivaties:
   a = 0.3; b = 1.2; c = -0.2; d = 0.8
   x = exp(seq(-2.8, 1.2, length = 100))
   y0 = djohnson(x, a, b, c, d, deriv = 0)
   y1 = djohnson(x, a, b, c, d, deriv = 1)
   y2 = djohnson(x, a, b, c, d, deriv = 2)
   # Compare with Numerical Differentiation:
   par(mfrow = c(2, 2))
   xa = exp(seq(-2.5, 1.5, length = 20))
   plot(x, y0, type = "l", main = "Johnson Type I Density")
   plot(x, y1, type = "l", main = "1st Derivative")
   z = derivative(xa, djohnson(xa, a, b, c, d, deriv = 0), deriv = 1)
   points(z$x, z$y, col = "steelblue4")
   plot(x, y2, type = "l", main = "2nd Derivative")
   z = derivative(xa, djohnson(xa, a, b, c, d, deriv = 0), deriv = 2)
   points(z$x, z$y, col = "steelblue4")
}


\keyword{math}

