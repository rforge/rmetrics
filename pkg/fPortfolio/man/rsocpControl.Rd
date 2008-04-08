\name{socpControl}


\alias{socpControl}
\alias{rsocpControl}


\title{Control List for Rsocp Solver}


\description{

    List of control parameters for "Rdonlp2" solver.

}


\usage{
rsocpControl(abs.tol = 1e-18, rel.tol = 1e-16, target = 0, 
    max.iter = 500, Nu = 10, out.mode = 0, BigM.K = 2, 
    BigM.iter = 5, scale = 1, penalty = 0)
}


\arguments{

    \item{abs.tol}{
        Absolute tolerance.
        }    
    \item{rel.tol}{
        Relative tolerance.
        }  
    \item{target}{
        Target value < 0, only used if rel.tol < 0.
        }  
    \item{max.iter}{
        the maximum number of iterations, socp is aborted if more 
        are required for convergence.
        }  
    \item{Nu}{
        The parameter that controls the rate of convergence, 
        Nu > 1, recommended range 5 to 50.
        }  
    \item{out.mode}{
        Specifies what should be output: 0 - nothing, 1 - duality 
        gap for initial point and after each iteration, 2 - duality 
        gap and deviation from centrality, for initial point and 
        after each iteration.
        }  
    \item{BigM.K, BigM.iter}{
        iterataion parameters, The default values are \code{BigM.iter=5} 
        and \code{BigM.K = 2}.
        } 
    \item{scale}{
        function scaling factor.
        }
    \item{penalty}{
        penalty factor for linear function.
        }  
        
}


\seealso{
  
    \code{socp}, and \code{donlp2}.
    
}


\keyword{optimize}

