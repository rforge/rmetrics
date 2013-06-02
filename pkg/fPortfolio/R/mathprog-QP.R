
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA


################################################################################
# FUNCTION:              DESCRIPTION:
#  quadprogQP             Function wrapper for solver solve.QP() from quadprog
#  quadprogControl        Returns default controls for solver quadprog
# FUNCTION:              DESCRIPTION:
#  ipopQP                 Function wrapper for solver ipop() from kernlab
#  ipopControl            Returns default controls for solver
################################################################################


quadprogQP <- 
function(
    objective=list(dvec=NULL, Dmat=NULL), 
    par.lower=NULL, par.upper=NULL, 
    eqA=NULL, eqA.bound=NULL,  
    ineqA=NULL, ineqA.lower=NULL, ineqA.upper=NULL,
    control=list())
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Implements Goldberg-Idnani Algorithm
    
    # FUNCTION:
    
    # Control List:
    ctrl <- quadprogControl()
    if (length(control) > 0)
        for (name in names(control)) ctrl[name] = control[name]
    control <- ctrl
    
    # General Settings:
    dvec <- -objective$dvec
    Dmat <- objective$Dmat
    Names <- colnames(rbind(dvec, Dmat))
    N <- ncol(rbind(dvec, Dmat))
    
    # Box Constraints:
    if (length(par.lower) == 1) par.lower <- rep(par.lower, N)
    if (length(par.upper) == 1) par.upper <- rep(par.upper, N)

    # Constraints Settings:
    Amat <- eqA
    if (!is.null(ineqA)) Amat <- rbind(eqA, ineqA, -ineqA)
    Amat <- rbind(Amat, diag(N), -diag(N))  
    Amat <- t(Amat) 
    bvec <- eqA.bound
    if (!is.null(ineqA.lower)) bvec <- c(bvec, ineqA.lower)
    if (!is.null(ineqA.upper)) bvec <- c(bvec, -ineqA.upper)
    bvec <-  c(bvec, par.lower)
    if (!is.null(par.upper)) bvec <- c(bvec, -par.upper)
    if (is.null(eqA)) meq <- 0 else meq <- nrow(eqA)
    Amat <- Amat[, is.finite(bvec)]
    bvec <- bvec[is.finite(bvec)]
    
    # Optimize:
    optim <- quadprog::solve.QP(
        Dmat = Dmat, 
        dvec = dvec, 
        Amat = Amat, 
        bvec = bvec, 
        meq = meq)
        
    # Note:
    #   DW: if quadprog::solve.QP fails with non-zero status optim$ierr=
    #   =1: stop("constraints are inconsistent, no solution!")
    #   =2: stop("matrix D in quadratic function is not positive definite!")
    #   this is ugly!
    
    # Version:
    package <- packageDescription(pkg="quadprog")
    version <- paste(package$Package, package$Version, package$Date)
        
    # Return Value:
    value <- list(
        opt = optim, 
        solution = optim$solution, 
        objective = optim$value,
        status = optim$ierr,
        message = "Not available",
        solver = "quadprog",
        version = version)
    class(value) = c("solver", "list")
    value
}


# -----------------------------------------------------------------------------


quadprogControl <-
    function(solver="quadprog", trace=FALSE)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns control parameter list
    
    # Arguments:
    #   trace - al logical flag, should the function be traced?
    
    # Details:
    #   Note there are no control paramters supported in 
    #   quadprog::solve.QP
    
    # FUNCTION:
    
    # Control Parameter:
    control <- list(trace = trace)
    
    # Return Value:
    control
}


###############################################################################


ipopQP <- 
function(
    objective=list(dvec=NULL, Dmat = NULL), 
    par.lower=NULL, par.upper=NULL, 
    eqA=NULL, eqA.bound=NULL,
    ineqA=NULL, ineqA.lower=NULL, ineqA.upper=NULL,
    control=list())
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Implements Vandenberg's LOQO Algorithm
    
    # Details:
    #   minimize     c' * primal + 1/2 primal' * H * primal
    #   subject to   b <= A*primal <= b + r
    #                l <= primal <= u
    #                d is the optimizer itself
    #   returns primal and dual variables (i.e. x and the Lagrange
    #   multipliers for b <= A * primal <= b + r)
    #   for additional documentation see
    #       R. Vanderbei
    #       LOQO: An Interior Point Code for Quadratic Programming, 1992
    #   Author:      R version Alexandros Karatzoglou, 
    #                orig. matlab Alex J. Smola
    #   Created:     12/12/97
    #   R Version:   12/08/03
    #   Updated:     13/10/05
    #   Code:        A modified copy from contributed R package kernlab
    #                released under the GNU Public License.
    #   Note:        A QP Solver entirely written in R from contributed 
    #                Package kernlab.
    
    # FUNCTION:
    
    # Control List:
    ctrl <- ipopControl()
    if (length(control) > 0)
        for (name in names(control)) ctrl[name] <- control[name]
    control <- ctrl
    
    # General Settings:
    dvec <- objective$dvec
    Dmat <- objective$Dmat 
    Names <- colnames(rbind(dvec, Dmat))
    N <- ncol(rbind(dvec, Dmat))
        
    # Solve:
    optim <- kernlab::ipop(
        c = matrix(objective$dvec, ncol=1),
        H = objective$Dmat, 
        A = rbind(eqA, ineqA), 
        b = c(eqA.bound, ineqA.lower), 
        l = matrix(par.lower, ncol=1), 
        u = matrix(par.upper, ncol=1), 
        r = c(eqA.bound, ineqA.upper) - c(eqA.bound, ineqA.lower), 
        sigf = control$sigf, 
        maxiter = control$maxiter, 
        margin = control$margin, 
        bound = control$bound, 
        verb = control$verb)
    
    # Version:
    package <- packageDescription(pkg="kernlab")
    version <- paste(package$Package, package$Version, package$Date)
        
    # Return Value:
    Status <- if (optim@how == "converged") Status <- 0 else Status <- 1
    ans <- list(
        opt = optim, 
        solution = optim@primal, 
        objective = c( dvec %*% par + 0.5 * par %*% Dmat %*% par )[[1]],
        status = Status,
        message = optim@how,
        solver = "ipop",
        version = version)
    class(ans) = c("solver", "list")
    ans
}


# -----------------------------------------------------------------------------


ipopControl <- 
function(
    sigf=7, maxiter=40, margin=0.05, bound=10, verb=0, trace=FALSE)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns control parameter list
    
    # FUNCTION:
    
    # Control Parameter:
    control <- list(
        sigf = sigf, 
        maxiter = maxiter, 
        margin = margin, 
        bound = bound, 
        verb = verb,
        solver = "ipop",
        trace = trace)
        
    # Return Value:
    control
}   


###############################################################################


.demoQP <-
function()
{

     # Example from Package kernlab:
         
     # Solve the Support Vector Machine optimization problem
     require(kernlab)
     data(spam)
     m <- 500
     set <- sample(1:dim(spam)[1],m)
     x <- scale(as.matrix(spam[,-58]))[set,]
     y <- as.integer(spam[set,58])
     y[y==2] <- -1
     C <- 5
     rbf <- rbfdot(sigma = 0.1)
     H <- kernelPol(rbf, x, , y)
     c <- matrix(rep(-1,m))
     A <- t(y)
     b <- 0
     l <- matrix(rep(0, m))
     u <- matrix(rep(C, m))
     r <- 0
     sv <- kernlab::ipop(c, H, A, b, l, u, r)
     print(sv)

     objective <- list(dvec=c[, 1], Dmat = H)
     par.lower <- l[, 1]
     par.upper <- u[, 1] 
     eqA <- A
     eqA.bound <- 0
     ineqA <- ineqA.lower <- ineqA.upper <- NULL
     ipop <- ipopQP(objective, par.lower, par.upper, eqA, eqA.bound)[-1]
     print(ipop)
     
     
     # Example from Package quadprog:
       
     require(quadprog)
     Dmat <- matrix(0, 3, 3)
     diag(Dmat) <- 1
     dvec <- c(0, 5, 0)
     Amat <- matrix(c(-4, -3, 0, 2, 1, 0, 0, -2, 1), 3, 3)
     bvec <- c(-8, 2, 0)
     solve.QP(Dmat, dvec, Amat, bvec=bvec)

     
     objective <- list(dvec=-dvec, Dmat=Dmat)
     par.lower <- NULL
     par.upper <- NULL
     eqA <- NULL
     eqA.bound <- NULL
     ineqA <- Amat
     ineqA.lower <- bvec
     ineqA.upper <- rep(Inf, 3)
     

     # Solve:
     quadprog <- quadprogQP(objective, par.lower=-Inf, par.upper=Inf,
         ineqA=ineqA, ineqA.lower=ineqA.lower, ineqA.upper=ineqA.upper)[-1]
     print(quadprog)
     
     
     # Return Value:
     invisible()
     
}


###############################################################################

