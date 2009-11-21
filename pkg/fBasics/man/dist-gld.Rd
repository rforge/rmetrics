\name{gld}

\alias{gld}

\alias{dgld}
\alias{pgld}
\alias{qgld}
\alias{rgld}


\title{Generalized Lambda Distribution}


\description{
    
    Density, distribution function, quantile function 
    and random generation for the generalized lambda
    distribution.
    
}


\usage{
dgld(x, lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
pgld(q, lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
qgld(p, lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
rgld(n, lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8)
}


\arguments{

    \item{lambda1, lambda2, lambda3, lambda4}{
        are numeric values where
        \code{lambda1} is the location parameter,
        \code{lambda2} is the location parameter,
        \code{lambda3} is the first shape parameter, and
        \code{lambda4} is the second shape parameter.
        }
    \item{n}{
        number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{x, q}{
        a numeric vector of quantiles.
        }
    
}


\value{

    All values for the \code{*gld} functions are numeric vectors: 
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.
    
}


%\references{
%    go here ...
%}


\author{

    Gu for code implemented from \R's 
    contributed package \code{gld}.
    
}


\examples{   
## rgld -
   set.seed(1953)
   r = rgld(500, lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8)
   plot(r, type = "l", col = "steelblue",
     main = "gld: lambda1=0 lambda2=-1 lambda3/4=-1/8")
 
## dgld - 
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   x = seq(-5, 5, 0.25)
   lines(x, dgld(x, , lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8))
 
## pgld -  
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, pgld(x, lambda1 = 1, lambda2 = 0.3, lambda3 = 1))
   
## qgld -
   # Compute Quantiles:
   qgld(pgld(seq(-5, 5, 1), lambda1 = 1, lambda2 = 0.3, lambda3 = 1), 
     lambda1 = 1, lambda2 = 0.3, lambda3 = 1) 
}


\keyword{distribution}

