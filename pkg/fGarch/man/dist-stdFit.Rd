\name{stdFit}

\alias{stdFit}


\title{Student-t Distribution Parameter Estimation}


\description{
    
    Function to fit the parameters of the Student-t  
    distribution. 
       
}


\usage{
stdFit(x, \dots)
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

    \code{stdFit} returns a list with the following components: 
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
## std -
   set.seed(1953)
   r = rstd(n = 1000)

       
## stdFit -
   stdFit(r)
}


\keyword{distribution}

