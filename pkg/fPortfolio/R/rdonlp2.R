
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

# Copyrights (C)
# for this R-port: 
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                    DESCRIPTION:
#  rdonlp2                      Portfolio interface to rdonlp2 solver
#  rdonlp2Control               Control list for "rdnlp2" Solver
################################################################################


# Package: Rdonlp2
# Version: 0.3-1
# Date: 2007-03-18
# Title: C library to use Peter Spelluci's DONLP2 from R.
# Author: Ryuichi Tamura(ry.tamura@gmail.com)
# Depends: R (>= 2.4.0)
# License: Free for research purpose.
# URL: http://arumat.net/Rdonlp2/
# Description: DONLP2(http://plato.la.asu.edu/donlp2.html) is a general 
#   purpose nonlinear constrained programming problem solver written 
#   by Peter Spelluci. Rdonlp2 is a wrapper library to use it from R.


# Builtin: Requires Rmetrics Package: Cdonlp2


rdonlp2 <- 
    function(
    par, fn,
    par.upper = rep(+Inf, length(par)),
    par.lower = rep(-Inf, length(par)),
    A = NULL,
    lin.upper = rep(+Inf, length(par)),
    lin.lower = rep(-Inf, length(par)),
    nlin = list(),
    nlin.upper = rep(+Inf, length(nlin)),
    nlin.lower = rep(-Inf, length(nlin)), 
    control = rdonlp2Control(),
    control.fun = function(lst){return(TRUE)},
    env = .GlobalEnv, name = NULL)
{    
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Portfolio interface to rdonlp2 Solver
    
    # FUNCTION:
    
    # Running ...
    # print("Running rdonlp2 ...")
    
    # Use analytical gradients?
    if (is.function(attr(fn, "gr")) &
        # DW:
        # all(lapply(nlin, function(e)is.function(attr(e,"gr"))))){
        all(unlist(lapply(nlin, function(e)is.function(attr(e,"gr")))))) {
        control["analyt"] = TRUE
    } else {
        control["analyt"] = FALSE
    }
  
    # Check parameter and its box constraints:
    if (length(par) != length(par.upper) | length(par) != length(par.lower) ){
        stop("# of elements for box constraints != # of parameters")
    }

    # Check linear constraints matrix A:
    if (is.null(A)){
        num.lin <- 0
        conmat <- c()
        lin.upper <- lin.lower <- c()
    } else {
        num.lin <- nrow(A)
        if (ncol(A) != length(par))
            stop("# of ncol(A) should be equal to # of par")
        if (length(lin.upper) != num.lin | length(lin.lower) != num.lin)
            stop("# of bounds for linear constraints should be equal to nrow(A)")
        conmat <- t(A)
    }
  
    # Nonlinear constraints:
    num.nlin <- length(nlin)
    if (length(nlin.upper)!=num.nlin | length(nlin.lower)!=num.nlin)
    stop("# of bounds for nonlinear constraints should be equal to length(nlin)")
    # Concatenate bounds for internal use:
    lbd <- c(par.lower, lin.lower, nlin.lower)
    ubd <- c(par.upper, lin.upper, nlin.upper)
    
    #
    # the wrapper for objective and constraint function 
    # (called from eval_extern())
    # mode == 0: EVAL_FN(evaluate objective and constraint function)
    # mode == 1: EVAL_GR(evaluate gr of objective and constraint function)
    #
    # fun.id == 0: evaluate objective function 'fn'
    # fun.id >= 1: evaluate constraint function 'nlin[[fun.id]]'
    #
    
    confun <- function(arg) {
        mode = arg[1]
        fun.id = arg[2]
        p = arg[c(-1,-2)]
        if (mode == 0){      # evaluate function values
            if (fun.id == 0){
                return(as.double(eval(fn(p), env)))
            }
            return(as.double(eval(nlin[[fun.id]](p), env)))
        } else if (mode == 1) { # evaluate gradient values
            if (fun.id == 0){
                # Modified by DW:
                # return(as.double(eval( fn@gr(p),               env)))
                  return(as.double(eval( (attributes(fn)$gr)(p), env)))
            }
            # Modified by DW:
            # return(as.double(eval( (nlin[[fun.id]]@gr)            (p), env)))
              return(as.double(eval( (attributes(nlin[[fun.id]])$gr)(p), env)))
        } else {
            stop("unknown evaluation mode: %d", mode)
        }
    }        

    # accfun
    accfun <- function(lst){
        return(as.logical(control.fun(lst)))
    }
    
    fsilent <- FALSE
    if (is.null(name)){
        fsilent <- TRUE
        name = "dummy"
    }
    
    # Start donlp2:
    tryCatch(
        # start donlp2
        ans <- .Call("call_donlp2",
            as.double(par),
            as.integer(num.lin),
            as.integer(num.nlin),
            fsilent,
            name,
            nchar(name),
            as.double(lbd), 
            as.double(ubd),
            as.double(conmat),
            control,
            accfun,
            confun, environment(confun), 
            PACKAGE = "Cdonlp2"),
            # ensure to free memory and close .mes .pro files if opened
            finally = .Call("teardown", 0, 
            PACKAGE = "Cdonlp2")
            )
    ans$nr.update <- matrix(ans$nr.update, nr = length(par))
    if (control$hessian) {
        ans$hessian = matrix(ans$hessian, nr = length(par))
    } else {
        ans$hessian = NULL
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rdonlp2Control <- 
    function(            
    iterma = 4000, nstep = 20,fnscale = 1, report = FALSE, rep.freq = 1,
    tau0 = 1.0, tau = 0.1, del0 = 1.0, epsx = 1e-5, delmin = 0.1*del0,
    epsdif = 1e-8, nreset.multiplier = 1, difftype = 3, epsfcn = 1e-16, 
    taubnd = 1.0, hessian = FALSE, te0 = TRUE, te1 = FALSE, te2 = FALSE, 
    te3 = FALSE, silent = FALSE, intakt = TRUE )
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Control list for "rdnlp2" Solver
    
    # FUNCTION:
    
    # Return Value:
    list(
        iterma = as.integer(iterma), 
        nstep = as.integer(nstep),
        fnscale = fnscale,
        report = report,
        rep.freq = as.integer(ifelse(rep.freq<1, 1, rep.freq)),
        tau0 = tau0, 
        tau = tau, 
        del0 = del0,
        epsx = epsx, 
        delmin = delmin, 
        epsdif = epsdif,
        nreset.multiplier = nreset.multiplier,
        difftype = as.integer(ifelse(!difftype%in%c(1,2,3), 3, difftype)),
        epsfcn = epsfcn, 
        taubnd = taubnd, 
        hessian = hessian,
        te0 = te0, 
        te1 = te1, 
        te2 = te2, 
        te3 = te3,
        silent = silent, 
        intakt = intakt)
}


################################################################################

