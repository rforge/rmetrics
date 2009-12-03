\name{ssdFit}

\alias{ssdFit}

\title{Fit Density Using Smoothing Splines }


\description{

    Estimates the parameters of a density function
    using smoothing splines.

}


\usage{   
ssdFit(x) 
}


\arguments{
        
    \item{x}{
        a numeric vector. 
        }

}


\value{
  
    The function \code{ssdFit}, \code{hypFit} returns an object of class
    \code{ssden}.
    The returned object can be used to evaluate density, probabilities
    and quantiles.
              
}


\author{

    Diethelm Wuertz, Chong Gu for the underlying \code{gss} package.
    
}


\references{

Gu, C. (2002), 
    \emph{Smoothing Spline ANOVA Models}, 
    New York Springer--Verlag.

Gu, C. and Wang, J. (2003), 
    \emph{Penalized likelihood density estimation: 
    Direct cross-validation and scalable approximation},
    Statistica Sinica, 13, 811--826. 
    
}


\examples{     
## ssdFit -
   set.seed(1953)
   r = rnorm(500)
   hist(r, breaks = "FD", probability = TRUE,
     col = "steelblue", border = "white")
 
## ssdFit - 
   param = ssdFit(r)
   
## dssd -  
   u = seq(min(r), max(r), len = 301)
   v = dssd(u, param)
   lines(u, v, col = "orange", lwd = 2)
}


\keyword{distribution}

