\name{donlp2}


\alias{donlp2}
\alias{rdonlp2}


\title{Solve constrained nonlinear minimization problem}


\description{

    Solve constrained nonlinear minimization problem.
    
}


\usage{
rdonlp2(par, fn,
    par.upper=rep(+Inf, length(par)), par.lower=rep(-Inf, length(par)),
    A = NULL, 
    lin.upper = rep(+Inf, length(par)), lin.lower = rep(-Inf, length(par)),
    nlin = list(), 
    nlin.upper = rep(+Inf, length(nlin)), nlin.lower = rep(-Inf, length(nlin)),
    control = rdonlp2Control(), control.fun = function(lst){return(TRUE)},
    env = .GlobalEnv, name = NULL)
}


\arguments{


    \item{fn}{
        the objective function to be minimized. Currently, \code{fn}
        must take only one argument, and the parameter vector(\code{par})
        will be passed to \code{fn} during the optimization. The first
        element of return value must be the evaluated value.
        }
    \item{par}{
        parameter vector(vector object).
        }
    \item{par.upper, par.lower}{
        upper and lower bounds for parameter vector,
        respectively. Their length must equal to
        \code{length(par)}. If some elements are unbounded, specify
        \code{+Inf} or \code{-Inf} explicitly.
        }
    \item{A}{
        the matrix object that represents linear constraints. Its
        columns must be equal to \code{length(par)}, and its rows
        must be equal to the number of linear constraints.
        }
    \item{lin.upper, lin.lower}{
        upper and lower bounds for linear constraints,
        respectively. Their length must equal to the number of linear
        constraints. If some elements are unbounded, specify \code{+Inf} 
        or \code{-Inf} explicitly.
        } 
    \item{nlin}{
        list object whose elements are functions that represents
        nonlinear constraints.  Rule for argument and return value is the
        same as \code{fn}, i.e., these functions take only one
        arugument(\code{par}), and return a vector object whose first element
        is the evaluated value.
        }
    
    \item{nlin.upper, nlin.lower}{
        upper and lower bounds for nonlinear constraints,
        respectively. Their length must equal to \code{length(nlin)}. If 
        some elements are unbounded, specify \code{+Inf} or \code{-Inf}
        explicitly.  
        }
    
    \item{control}{
        "control parameters" that defines the behavior of
        Rdonlp2. See \code{\link{rdonlp2Control}} for details.
        }
    
    \item{control.fun}{
        \code{rdonlp2()} reports a group of optimization
        parameters in every iteration(see below for details). This (read-only)
        information can be available within \code{control.fun()}, in which user
        can decide whether the optimization should be iterrupted. Set its
        return value to \code{FALSE} to interrupt \code{rdonlp2()}.
        }
    
    \item{env}{
        the environment in which objective, constraint, control
        functions are evaluated. 
        }
    
    \item{name}{
        an character object that specify file name(without
        extension, max 40 characters) of output file. If not \code{NULL},
        DONLP2 creates 2 files(\code{name}.pro and \code{name}.mes) in
        current working directory which contain detailed information during
        the optimization. The amount of information depends
        \code{te0,te1,te2,te3} specified in \code{\link{rdonlp2Control}}.
        }
    
}


\note{

    IMPORTANT: Due to license reasons the R package \code{Rdonlp2} is
    not part of the Rmetrics distribution. You can download and install
    the package from\cr
    https://svn.r-project.org/Rmetrics/trunk/Rdonlp2/
    
}


\value{

    For \code{n=length(par)} parameters, \code{lin} linear constraints,
    and \code{nlin} nonlinear constraints, a list with following elements:
    
    \item{par}{
        parameters returned by DONLP2.
        }
    \item{gradf}{
        gradient evaluated at \code{par}.
        }
    \item{u}{
        \code{2*(n+lin+nlin)} vector of lagrange multipliers for
        constraints.
        } 
    \item{w}{
        \code{2*(n+lin+nlin)} vector of  penalty term.
        }
    \item{step.nr}{
        total number of iterations.
        }
    \item{fx}{
        the value of objective function \code{fn}.
        }
    \item{sci}{
        scaling of \code{fn}.
        }  
    \item{psi}{
        psi the weighted penalty term.
        }
    \item{upsi}{
        the unweighted penalty term(L1 norm of constraint
        vector).
        }
    \item{del.k.1}{
        bound for the last active constraints.
        }
    \item{b2n0}{
        weighted L2 norm of projected gradients.
        }
    \item{b2n}{
        L2norm of gradients based on \code{del.k.1}.
        }
    \item{nr}{
        number of binding constraints.
        }
    \item{sing}{
        value other than \texttt{-1} indicates working set is
        singular.
        }
    \item{umin}{
        infinity norm of negative part of inequalities
        multipliers.
        }
    \item{not.used}{
        always \code{0}(currently not used).
        }
    \item{cond.r}{
        condition number of diagonal part of qr decomposition of
        normalized gradients of binding constraints.
        }
    \item{cond.h}{
        condition number of diagonal of cholesky factor
        of updated full Hessian.
        }
    \item{scf0}{
        the relative damping of tangential component if
        \code{upsi > tau0/2}.
        }
    \item{xnorm}{
        L2 norm of \code{par}.
        }
    \item{dnorm}{
        unsclaed norm of \code{d}, correction from eqp/qp subproblem.
        }
    \item{phase}{
        \code{-1}:infeasibility improvement phase, \code{0}:
        initial optimization, \code{1}:binding constraints unchanged,
        \code{2}:\code{d} small, maratos correction is in use.
        }
    \item{c.k}{
        number of decreases of penalty weights.
        }
    \item{wmax}{
        infinity norm of weights.
        }
    \item{sig.k}{
        stepsize from uidimensional minimization(backgracking).
        }
    \item{cfincr}{
        number of objective function evaluations for stepsize
        algorithm}
    \item{dirder}{
        scaled derectional derivative of penalty function along
        \code{d}.
        }
    \item{dscal}{
        scaling factor for \code{d}.
        }
    \item{cosphi}{
        cosine of arc between \code{d} and previous \code{d}.
        }
    \item{violis}{
        number of constraints not binding at current values of
        \code{par} but hit during line search.
        }
    \item{hesstype}{
        one of 4 values indicating type of update for Hessian. 
        \code{1}: normal P&M-BFGS update, 
        \code{0}: update suppressed,
        \code{-1}: restart with scaled unit matrix, 
        \code{2}: standard BFGS,
        \code{3}: BFGS modified by Powell's Device.
        }
    \item{modbifgs}{
        modification factor for damping the projector into the
        BFGS or pantoja-mayne update.
        }
    \item{modnr}{
        modification factor for daming the quasi-newton-relation
        in BFGS.
        }
    \item{qpterm}{
        \code{0}:if \code{sing==-1}, termination indicator of the QP solver, 
        \code{1}:successful, \code{-1}:\code{\tau} becomes larger than 
        \code{tauqp} without slack variables becoming sufficiently
        small.
        }
    \item{tauqp}{
        weight of slack variables in QP solver.
        } 
    \item{infeas}{
        L1 norm of  slack variables in QP solver.
        }
    \item{nr.update}{
        the approximated newton-raphson update in upper
        trianglar form.
        }
    \item{hessian}{
        numeric Hessian matrix if \code{hessian=TRUE} in 
        \code{\link{rdonlp2Control}}.
        }
    \item{runtime}{
        the elapsed time for the optimization.
        }
    \item{message}{
        the termination message.
        }

}


\author{ 

    Rdonlp2(R port) is copyrighted by Ryuichi Tamura. Original
    DONLP2 is copyrighted software written by Peter Sperucci.  
    
}


\references{
  
    \url{http://plato.la.asu.edu/donlp2.html}(original DONLP2), 
    \url{http://arumat.net/Rdonlp2/}(Rdonlp2)
    
}

\seealso{

    \code{\link{rdonlp2Control}}
    
}


\keyword{optimize}

