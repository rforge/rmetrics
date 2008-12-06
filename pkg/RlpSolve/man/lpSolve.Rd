\name{lpSolve}

\alias{lpSolve}

\title{Linear and Integer Programming}


\description{Interface to lp\_solve linear/integer programming system}


\usage{
lpSolve(direction = "min", objective.in, const.mat, const.dir, const.rhs,
	transpose.constraints = TRUE, int.vec, presolve=0, compute.sens=0,
    binary.vec, all.int=FALSE, all.bin=FALSE, scale = 196, dense.const, 
    num.bin.solns=1, use.rw=FALSE)
}


\arguments{

    \item{direction}{
        Character string giving direction of optimization:
        "min" (default) or "max."}
    
    \item{objective.in}{
        Numeric vector of coefficients of objective function}
    
    \item{const.mat}{
        Matrix of numeric constraint coefficients, one row 
        per constraint, one column per variable (unless 
        transpose.constraints = 
        FALSE; see below).}
    
    \item{const.dir}{
        Vector of character strings giving the direction of 
        the constraint: each value should be one 
        of "<," "<=," "=," "==," ">," or ">=".
        (In each pair the two values are identical.)}
    
    \item{const.rhs}{
        Vector of numeric values for the right-hand sides of 
        the constraints.}
    
    \item{transpose.constraints}{By default each constraint occupies a row 
        of const.mat, and that matrix needs to be transposed before being passed 
        to the optimizing code.  For very large constraint matrices it may be wiser 
        to construct the constraints in a matrix column-by-column. In that case set 
        transpose.constraints to FALSE.}
    
    \item{int.vec}{
        Numeric vector giving the indices of variables that are 
        required to be integer. The length of this vector will therefore be the 
        number of integer variables.}
    
    \item{presolve}{
        Numeric: presolve? Default 0 (no); any
        non-zero value means "yes." Currently ignored.}
    
    \item{compute.sens}{
        Numeric: compute sensitivity? Default 0 (no); any
        non-zero value means "yes."}
    
    \item{binary.vec}{
        Numeric vector like int.vec giving the indices of variables 
        that are required to be binary.}
    
    \item{all.int}{
        Logical: should all variables be integer? Default: FALSE.}
    
    \item{all.bin}{
        Logical: should all variables be binary? Default: FALSE.}
    
    \item{scale}{
        Integer: value for lpSolve scaling. Details can be found in
        the lpSolve documentation. Set to 0 for no scaling. Default: 196}
    
    \item{dense.const}{
        Three column dense constraint array. This is ignored if
        const.mat is supplied. Otherwise the columns are constraint 
        number, column number, and value; there should be one row for 
        each non-zero entry in the constraint matrix.}
    
    \item{num.bin.solns}{
        Integer: if all.bin=TRUE, the user can request up to
        num.bin.solns optimal solutions to be returned.}
    
    \item{use.rw}{Logical: if TRUE and num.bin.solns > 1, write the lp out to a
        file and read it back in for each solution after the first. This is just to
        defeat a bug somewhere. Although the default is FALSE, we recommend you set
        this to TRUE if you need num.bin.solns > 1, until the bug is found.}
}


\details{

    This function calls the lp\_solve 5.5 solver. That system has many options not
    supported here. The current version is maintained at
    \url{ftp://ftp.es.ele.tue.nl/pub/lp_solve}
    
}


\value{

    An lp object. 
    
}


\author{

    Sam Buttrey, \email{buttrey@nps.edu}
    
}



\keyword{optimize}
