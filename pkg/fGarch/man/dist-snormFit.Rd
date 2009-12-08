\name{snormFit}

\alias{snormFit}


\title{Skew Normal Distribution Parameter Estimation}


\description{
    
    Function to fit the parameters of the skew normal
    distribution. 
    
}


\usage{
snormFit(x, \dots)
}


\arguments{

    \item{x}{
        a numeric vector of quantiles.
        }
    \item{\dots}{
        parameters parsed to the optimization function \code{nlm}. 
        }
    
}


\value{

    
    \code{snormFit} returns a list with the following components: 
    \cr
    \item{par}{
        The best set of parameters found. 
        }
    \item{objective}{
        The value of objective corresponding to \code{par}.
        }
    \item{convergence}{
        An integer code. 0 indicates successful convergence.
        }  
    \item{message}{
        A character string giving any additional information 
        returned by the optimizer, or NULL. For details, see 
        PORT documentation.
        }  
    \item{iterations}{
        Number of iterations performed.
        } 
    \item{evaluations}{
        Number of objective function and gradient function 
        evaluations.
        } 
 
}


\references{
    
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## rsnorm -
   set.seed(1953)
   r = rsnorm(n = 1000)
       
## snormFit -
   snormFit(r)
}


\keyword{distribution}

