\name{Rsocp}


\alias{Rsocp}
\alias{socp}
\alias{rsocp}


\title{Second Order Cone Programming Solver}


\description{

    Solve Second-order Cone Problem by primal-dual interior point method.
    
}


\usage{
rsocp(f, A, b, C, d, N, x, z, w, control = list())
}


\arguments{
    \item{f}{
        Vector defining linear objective, 
        \code{length(f)==length(x)} 
        }
    \item{A}{
        Matrix with the \code{A(i)} vertically stacked:  A = [ A(1);
        A(2); \ldots A(L)]
        }
    \item{b}{
        Vector with the \code{b(i)} vertically stacked: b = [ b(1);
        b(2); \ldots b(L)]
        }
    \item{C}{
        Matrix with the \code{c(i)'} vertically stacked: C = [ c(1)';
        c(2)'; \ldots c(L)']
        }
    \item{d}{
        Vector with the \code{d(i)} vertically stacked: d = [ d(1);
        d(2); \ldots d(L)]
        }
    \item{N}{
        vector of size \code{L}, defining the size of each
        constraint.
        }
    \item{x}{
        Primal feasible initial point. Must satisfy:
        norm(A(i)*x+b(i)) < c(i)'*x+d(i) ,  i=1,\ldots,L
        }
    \item{z}{
        dual feasible initial point.
        }
    \item{w}{
        dual feasible initial point.
        }
    \item{control}{
        A list of control parameters. See below for details.
        }
}


\details{

    minimize \deqn{f'*x}{f'*x}
    subject to  \deqn{norm(A_i*x+b_i) <= c_i'*x+d_i , i=1,\ldots,
    L}{norm(A_i*x+b_i) <=c_i'*x+d_i , i=1,\ldots, L}
             
    The dual problem is:
    
    maximize  \deqn{-(b'*z+d*w)}{-(b'*z+d*w)} 
    subject to
    \deqn{A'*z+C'*w == f}{A'*z+C'*w == f} 
    \deqn{norm(z_i) <= w_i ,  i=1,\ldots,L}{norm(z_i) <= w_i ,  i=1,\ldots,L}
    
    (see description of input arguments for definition of A, b, C, d, z, w)
    
    \emph{STOPPING CRITERIA:} (stop when any of the following is met)
    
    \code{abs_tol} -- maximum absolute error in objective function;
    guarantees that for any x:  \eqn{abs(f'*x - f'*x\_opt) <=
    abs\_tol}{abs(f'*x - f'*x_opt) <= abs_tol}
    
    \code{rel_tol} -- maximum relative error in objective function;
    guarantees that for any x:
    \eqn{abs(f'*x - f'*x\_opt)/(f'*x\_opt) <= rel\_tol  (if f'*x\_opt >
    0)}{abs(f'*x - f'*x_opt)/(f'*x_opt) <= rel_tol  (if f'*x_opt > 0)} 
    Negative value has special meaning, see target
    
    \code{target} -- if \eqn{rel\_tol<0}{rel_tol<0}, stops when
    \eqn{f'*x<target or -b'*z>=target}{f'*x<target or -b'*z>=target}
    
    \code{max_iter} -- limit on number of algorithm outer iterations.
    Most problems can be solved in less than 50 iterations.
    Called with \code{max_iter=0} only checks feasibility of \code{x} and
    \code{z}, (and returns gap and deviation from centrality).
    
    \emph{OTHER PARAMETERS:}
    
    \code{Nu} -- duality gap vs. deviation from centrality reduction weight.
    As a general rule, larger values of Nu yield faster convergence,
    up to a point where the deviation from centrality becomes too
    large and the convergence becomes very slow.
    Required \eqn{Nu>0}, recommended \eqn{Nu>1}, typical range
    \eqn{2<=Nu<=50}. Try \eqn{Nu=10}.
    
    \code{out_mode} -- specifies what will be output in hist:
    \tabular{ll}{
    \code{0} \tab hist is returned empty (default value)\cr
    \code{1} \tab vector with duality gap (an upper bound on absolute\cr
             \tab error), for the initial point and after each iteration\cr
    \code{2} \tab  matrix with duality gap and deviation from centrality.}
    
}


\note{

    IMPORTANT NOTE: Due to copyright and license reasons the 
    R package \code{Rsocp} is not part of the Rmetrics distribution. 
    
    Please contact "wuertz@phys.ethz.ch".
    
}


\value{

    A list with the following elements:
    
    \item{x}{
        a numeric vect, the solution.
        }
    \item{z}{
        solution to the dual problem.
        }
    \item{iter}{
        Number of iterations performed.
        }
    \item{hist}{
        see \code{out.mode}.
        }
    \item{convergence}{
        A logical code. \code{TRUE} indicates successful convergence.
        }
    \item{info}{A numerical code. It indicates if the convergence was 
        successful.
        }
    \item{message}{A character string giving any additional information
        returned by the optimizer.
        }
}


\references{

    SOCP, Software for Second-order Cone Programming User's Guide,
    Beta Version April 1997,
    Miguel Sousa Lobo, Lieven Vandenberghe, Stephen Boyd.

    \url{http://www.stanford.edu/~boyd/old_software/SOCP.html}
    
}


\author{ 

    Rsocp (R port) is copyrighted by Yohan Chalabi and Diethelm Wuertz. 
    
    Original SOCP is copyrighted software written by written by  
    M. Lobo, L. Vandenberghe, and S. Boyd.
    
}


\keyword{optimize}

