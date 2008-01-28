\name{maxdd}

\alias{maxdd}

\alias{dmaxdd}
\alias{pmaxdd}
\alias{rmaxdd}

\alias{maxddStats}


\title{Drawdown Statistics}


\description{
  
    This is a collection and description of 
    functions which compute drawdown statistics. 
    Included are density, distribution function,  
    and random generation for the maximum drawdown 
    distribution. In addition the expectation of 
    drawdowns for Brownian motion can be computed.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{dmaxdd} \tab the Density function, \cr
    \code{pmaxdd} \tab the Distribution function, \cr
    \code{rmaxdd} \tab the random number generator, \cr
    \code{maxddStats} \tab the expectation of drawdowns. }
    
}
    

\usage{
dmaxdd(x, sd = 1, horizon = 100, N = 1000) 
pmaxdd(q, sd = 1, horizon = 100, N = 1000) 
rmaxdd(n, mean = 0, sd = 1, horizon = 100)

maxddStats(mean = 0, sd = 1, horizon = 1000)
}


\arguments{
  
    \item{x, q}{
        a numeric vector of quantiles.
        }  
    \item{n}{
        an integer value, the number of observations.
        }   
    \item{mean, sd}{
        two numeric values, the mean and standard deviation.
        }
    \item{horizon}{
        an integer value, the (run time) horizon of the investor.
        }
    \item{N}{
        an integer value, the precession index for summations. 
        Before you change this value please inspect Magdon-Ismail
        et. al. (2003).
        }
        
}


\note{

    Currrently, for the functions \code{dmaxdd} and \code{pmaxdd} only the
    trend or driftless case is implemented. 
    
}


\value{

    \code{dmaxdd} 
    \cr
    returns for a trendless Brownian process \code{mean=0} and
    standard deviation "sd" the density from the probability 
    that the maximum drawdown "D" is larger or equal to "h"  
    in the interval [0,T], where "T" denotes the time \code{horizon}  
    of the investor.
    \cr
    
    \code{pmaxdd} 
    \cr
    returns for a trendless Brownian process \code{mean=0} and
    standard deviation "sd" the the probability 
    that the maximum drawdown "D" is larger or equal to "h"  
    in the interval [0,T], where "T" denotes the time \code{horizon}  
    of the investor.
    \cr
    
    \code{rmaxdd}
    \cr
    returns for a Brownian Motion process with mean \code{mean} and 
    standard deviation \code{sd} random variates of maximum drawdowns.
  
    \code{maxddStats} 
    \cr
    returns the expectation Value E[D] of maximum drawdowns of
    Brownian Motion for a given drift \code{mean}, variance \code{sd},  
    and runtime \code{horizon} of the Brownian Motion process. 
  
}


\references{

Magdon-Ismail M., Atiya A.F., Pratap A., Abu-Mostafa Y.S. (2003);
    \emph{On the Maximum Drawdown of a Brownian Motion},
    Preprint, CalTech, Pasadena USA, p. 24. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## rmaxdd -
   # Set a random seed
   set.seed(1953)               
   # horizon of the investor, time T
   horizon = 1000               
   # number of MC samples, N -> infinity
   samples = 1000               
   # Range of expected Drawdons
   xlim = c(0, 5)*sqrt(horizon) 
   # Plot Histogram of Simulated Max Drawdowns:
   r = rmaxdd(n = samples, mean = 0, sd = 1, horizon = horizon)
   hist(x = r, n = 40, probability = TRUE, xlim = xlim, 
     col = "steelblue4", border = "white", main = "Max. Drawdown Density") 
   points(r, rep(0, samples), pch = 20, col = "orange", cex = 0.7)
   
## dmaxdd - 
   x = seq(0, xlim[2], length = 200)
   d = dmaxdd(x = x, sd = 1, horizon = horizon, N = 1000)
   lines(x, d, lwd = 2)
    
## pmaxdd -
   # Count Frequencies of Drawdowns Greater or Equal to "h":
   n = 50
   x = seq(0, xlim[2], length = n)
   g = rep(0, times = n)
   for (i in 1:n) g[i] = length (r[r > x[i]]) / samples
   plot(x, g, type ="h", lwd = 3,
     xlab = "q", main = "Max. Drawdown Probability")
   # Compare with True Probability "G_D(h)":
   x = seq(0, xlim[2], length = 5*n)
   p = pmaxdd(q = x, sd = 1, horizon = horizon, N = 5000)
   lines(x, p, lwd = 2, col="steelblue4") 
   
## maxddStats -
   # Compute expectation Value E[D]:
   maxddStats(mean = -0.5, sd = 1, horizon = 10^(1:4))
   maxddStats(mean =  0.0, sd = 1, horizon = 10^(1:4))
   maxddStats(mean =  0.5, sd = 1, horizon = 10^(1:4))
}


\keyword{distribution}
