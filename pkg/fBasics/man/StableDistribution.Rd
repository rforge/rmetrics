\name{StableDistribution}

\alias{StableDistribution}

\alias{dstable}
\alias{pstable}
\alias{qstable}
\alias{rstable}
\alias{stableMode}

\alias{stableSlider}

\title{Stable Distribution Function}


\description{
    
    A collection and description of functions to compute 
    density, distribution function, quantile function and 
    to generate random variates, the stable distribution,
    and the stable mode. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{[dpqr]stable} \tab the skewed stable distribution, \cr
    \code{stableMode} \tab the stable mode, \cr
    \code{stableSlider} \tab interactive stable distribution display. }

}


\usage{
dstable(x, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
pstable(q, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
qstable(p, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
rstable(n, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))

stableMode(alpha, beta)

stableSlider()
}


\arguments{

    \item{alpha, beta, gamma, delta}{
        value of the index parameter \code{alpha} with \code{alpha = (0,2]};
        skewness parameter \code{beta}, in the range [-1, 1];
        scale parameter \code{gamma}; and
        shift parameter \code{delta}.
        }
    \item{n}{
        number of observations, an integer value.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{pm}{
        parameterization, an integer value by default \code{pm=0}, 
        the 'S0' parameterization.
        }
    \item{x, q}{
        a numeric vector of quantiles.
        }
    
}


\value{

    All values for the \code{*stable} functions
    are numeric vectors: 
    \code{d*} returns the density, 
    \code{p*} returns the distribution function, 
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
    The function \code{stableMode} returns a numeric value, the
    location of the stable mode.
    
    The function \code{stableSlider} 
    displays for educational purposes the densities and probabilities
    of the skew stable distribution.
    
}


\details{
    
    \bold{Skew Stable Distribution:}
    \cr\cr
    The function uses the approach of J.P. Nolan for general 
    stable distributions. Nolan derived expressions in form of 
    integrals based on the charcteristic function for standardized 
    stable random variables. These integrals are numerically 
    evaluated using R's function \code{integrate}. 
    \cr
    "S0" parameterization [pm=0]: based on the (M) representation
    of Zolotarev for an alpha stable distribution with skewness
    beta. Unlike the Zolotarev (M) parameterization, gamma and 
    delta are straightforward scale and shift parameters. This
    representation is continuous in all 4 parameters, and gives 
    an intuitive meaning to gamma and delta that is lacking in 
    other parameterizations.
    \cr
    "S" or "S1" parameterization [pm=1]: the parameterization used 
    by Samorodnitsky and Taqqu in the book Stable Non-Gaussian 
    Random Processes. It is a slight modification of Zolotarev's 
    (A) parameterization.
    \cr
    "S*" or "S2" parameterization [pm=2]: a modification of the S0 
    parameterization which is defined so that (i) the scale gamma 
    agrees with the Gaussian scale (standard dev.) when alpha=2 
    and the Cauchy scale when alpha=1, (ii) the mode is exactly at 
    delta.
    \cr
    "S3" parameterization [pm=3]: an internal parameterization. The 
    scale is the same as the S2 parameterization, the shift is 
    \eqn{-beta*g(alpha)}, where \eqn{g(alpha)} is defined in 
    Nolan [1999].
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\references{

Chambers J.M., Mallows, C.L. and Stuck, B.W. (1976);
    \emph{A Method for Simulating Stable Random Variables},
    J. Amer. Statist. Assoc. 71, 340--344.

Nolan J.P. (1999);
    \emph{Stable Distributions},
    Preprint, University Washington DC, 30 pages.
    
Nolan J.P. (1999);
    \emph{Numerical Calculation of Stable Densities and Distribution
    Functions},
    Preprint, University Washington DC, 16 pages.

Samoridnitsky G., Taqqu M.S. (1994);
    \emph{Stable Non-Gaussian Random Processes, Stochastic Models
    with Infinite Variance},
    Chapman and Hall, New York, 632 pages.

Weron, A., Weron R. (1999);
    \emph{Computer Simulation of Levy alpha-Stable Variables and
    Processes},
    Preprint Technical Univeristy of Wroclaw, 13 pages.
    
}

\examples{    
## stable - 
   # Plot rvs Series
   set.seed(1953)
   r = rstable(n = 1000, alpha = 1.9, beta = 0.3)
   plot(r, type = "l", main = "stable: alpha=1.9 beta=0.3", 
     col = "steelblue")
   grid()
 
## stable -  
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", 
     col = "steelblue")
   x = seq(-5, 5, 0.4)
   lines(x, dstable(x = x, alpha = 1.9, beta = 0.3))
   
## stable -   
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", pch = 19, 
     col = "steelblue")
   lines(x, pstable(q = x, alpha = 1.9, beta = 0.3))
   grid()
   
## stable -
   # Compute quantiles:
   qstable(pstable(seq(-4, 4, 1), alpha = 1.9, beta = 0.3), 
     alpha = 1.9, beta = 0.3) 
}


\keyword{distribution}

