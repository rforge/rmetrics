\name{gedFit}

\alias{gedFit}


\title{Generalized Error Distribution Parameter Estimation}


\description{
    
    Function to fit the parameters of the generalized error 
    distribution. 
    
}


\usage{
gedFit(x, \dots)
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

    
    \code{gedFit} returns a list with the following components: 
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

Nelson D.B. (1991);
    \emph{Conditional Heteroscedasticity in Asset Returns: A New Approach},
    Econometrica, 59, 347--370.
    
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## rged -
   set.seed(1953)
   r = rged(n = 1000)
       
## gedFit -
   gedFit(r)
}


\keyword{distribution}

