\name{RlpSolve}


\alias{RlpSolve}
\alias{lpSolve}
\alias{rlpSolve}

\alias{Rlp}
\alias{lp}
\alias{rlp}


\title{Linear Programming Solver}


\description{

    Solve Linear Programming Problem using 'lpSolver' solver.
    
}


\usage{
rlpSolve(direction = "min", objective.in, const.mat, const.dir, const.rhs, 
    transpose.constraints = TRUE, int.vec, presolve = 0, compute.sens = 0)
    
rlp(obj, A, b, Aeq = NULL, beq = NULL, lb = 0.0, ub = Inf,
    intvec = integer(0), control = list())
}


\arguments{
    
    For \code{lpSolve} Interface calling function \code{rlp}:
    
    % rlpSolve(direction = "min", objective.in, const.mat, const.dir, const.rhs, 
    % transpose.constraints = TRUE, int.vec, presolve = 0, compute.sens = 0)
    
    \item{direction}{
        ...
        }
    \item{objective.in}{
        ...
        }
    \item{const.mat}{
        ...
        }
    \item{const.dir}{
        ...
        }
    \item{const.rhs}{
        ...
        }
    \item{transpose.constraints}{
        ...
        }
    \item{int.vec}{
        ...
        }
    \item{presolve}{
        ...
        }
    \item{compute.sens}{
        ...
        }
        
    For \code{Rlp} Interface calling function \code{rlpSolve}:
    
    % rlp(obj, A, b, Aeq = NULL, beq = NULL, lb = 0.0, ub = Inf,
    % intvec = integer(0), control = list())
    
    \item{obj}{
        ...
        }
    \item{A}{
        ...
        }
    \item{b}{
        ...
        }
    \item{Aeq}{
        ...
        }
    \item{beq}{
        ...
        }
    \item{lb}{
        ...
        }
    \item{ub}{
        ...
        }
    \item{intvec}{
        ...
        }
    \item{control}{
        ...
        }
 
}


%\details{}


\note{

    IMPORTANT NOTE: %Due to copyright and license reasons  
    The R package \code{RlpSolve} is not part of the Rmetrics distribution. 
    
    Please contact "wuertz@phys.ethz.ch".
    
}


\value{

    For \code{rlpSolve} a list with the following elements:
    
    \item{\dots}{
        ...
        }
        
    For \code{rlp} a list with the following elements:
    
    \item{\dots}{
        ...
        }
        
}


%\references{}


%\author{}


\keyword{optimize}

