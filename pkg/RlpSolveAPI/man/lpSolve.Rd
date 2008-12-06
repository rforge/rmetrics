\name{lpSolveAPI}


\alias{lpSolveAPI}


\title{Linear Program Solver}

\description{
    
    Solves the linear program


    \preformatted{
           min:  obj * x
    subject to:    A \%*\% x <= b
                 Aeq \%*\% x == beq
                 lb[j] <= x[j] <= ub[j]
    }

    where \code{x} is a vector of \code{n} decision variables, \code{obj} is 
    a vector of \code{n} obective coefficients, \code{A} is an \code{ldA} 
    by \code{n} matrix of inequality constraint coefficients, \code{b} is a 
    vector of \code{ldA} inequality constraint values, \code{Aeq} is 
    an \code{ldAeq} by \code{n} matrix of equality constraint coefficients, 
    \code{beq} is a vector of \code{ldAeq} equailty constraint values, and 
    \code{ub} and \code{lb} are respectively vectors of \code{n} upper and 
    lower bounds on the decision variables.
}

\usage{
lpSolveAPI(obj, A, b, Aeq = NULL, beq = NULL, lb = 0, ub = Inf,
        intvec = integer(0), control = list())
}

\arguments{

    \item{obj}{
        a numeric vector of length \code{n} containing the coefficients 
        of the linear objective function.
        }
    \item{A}{
        a numeric matrix with \code{ldA} rows and \code{n} columns 
        containing the coefficients of the linear inequality constraints.
        }
    \item{b}{a numeric vector of length \code{ldA} containing the values 
        for the linear inequality constraints.
        }
    \item{Aeq}{
        a numeric matrix with \code{n} columns containing the coefficients 
        of the linear equality constraints.
        }
    \item{beq}{
        a numeric vector of length \code{ldA} containing the values for 
        the linear equality constraints.
        }
    \item{lb}{
        a numeric vector of length \code{n} containing the lower bounds 
        for the decision variables.
        }
    \item{ub}{
        a numeric vector of length \code{n} containing the upper bounds 
        for the decision variables.
        }
    \item{intvec}{
        a vector of unique positive integer values from \code{1} to \code{n} 
        indicating decision variables that are restricted to integer values.
        }
    \item{control}{
        a list containing control parameters for the MILP solver.
        }

}


\value{
  
    A list with the following components:
  
    \item{objective}{
        a single numeric value containing the optimal value of the 
        objective function (provided a finite optimal solution is found).
        }
    \item{x}{
        a numeric vector of length \code{n} containing the values of the 
        decision variables at the optimal solution.
        }
    \item{status}{
        an integer value indicating the exit status of the MILP solver. 
        A value of \code{0} indicates that an optimal solution was found. 
        Other possibilities are given in the note below.
        }
    \item{message}{
        a character string describing the value of \code{status}.
        }
    \item{control}{
        a list containing the control parameters used in the MILP solver.
        }

        
    \emph{Possible exit statuses of the MILP solver:}

    \tabular{rl}{
       0: \tab "optimal solution found" \cr
       1: \tab "the model is sub-optimal" \cr
       2: \tab "the model is infeasible" \cr
       3: \tab "the model is unbounded" \cr
       4: \tab "the model is degenerate" \cr
       5: \tab "numerical failure encountered" \cr
       6: \tab "process aborted" \cr
       7: \tab "timeout" \cr
       9: \tab "the model was solved by presolve" \cr
      10: \tab "the branch and bound routine failed" \cr
      11: \tab "the branch and bound was stopped because of a break-at-first or break-at-value" \cr
      12: \tab "a feasible branch and bound solution was found" \cr
      13: \tab "no feasible branch and bound solution was found"}
}


\references{

    lp\_solve: \url{http://lpsolve.sourceforge.net/5.5/index.htm}
    
}


\author{

    Kjell Konis \email{kjell.konis@epfl.ch}.
    
}


\examples{

  # maximize:
  #   x1 + 9 x2 +   x3 
  # subject to:
  #   x1 + 2 x2 + 3 x3 <= 9
  # 3 x1 + 2 x2 + 2 x3 <= 15

  obj <- c(1, 9, 3)
  A <- matrix(c(1, 2, 3, 3, 2, 2), 2, 3, byrow = TRUE)
  b <- c(9, 15)

  lpSolveAPI(-obj, A, b)
  
}


\keyword{optimize}

