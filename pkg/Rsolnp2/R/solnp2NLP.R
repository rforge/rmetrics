
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:                DESCRIPTION:
#  solnp2NLP                Nonlinear programming with nonlinear constraints
################################################################################


solnp2NLP <- 
function(
    par, fun, 
    par.lower = NULL, par.upper = NULL,
    eqA = NULL, eqA.bound = NULL,
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
    eqFun = list(), eqFun.bound = NULL,
    ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL,
    control = list())
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Universal function wrapper for solver solnp().
    
    # Details:
    #   solnp <- function (
    #       pars, fun, grad = NULL, 
    #       eqfun = NULL, eqB = NULL, eqgrad = NULL, 
    #       ineqfun = NULL, ineqLB = NULL, ineqUB = NULL, ineqgrad = NULL, 
    #       LB = NULL, UB = NULL, 
    #       control = list(), ...) 
    
    # FUNCTION:
    
    # Environment Setting:
    env = .GlobalEnv
    
    # Control List:
    ctrl = solnp2Control()
    if (length(control) > 0)
        for (name in names(control)) ctrl[name] = control[name]
    control = ctrl
    
    # Equality Function Constraints:
    if (length(eqFun) > 0) {
        eqfun <- function(x) {
            ans = NULL
            if(!is.null(eqA)) ans = c(ans, as.vector(eqA %*% x))
            if (length(eqFun) > 0)
                for (i in 1:length(eqFun)) ans = c(ans, eqFun[[i]](x))
            ans }
    } else {
        eqfun = NULL
    }      
    eqB = c(eqA.bound, eqFun.bound)
    
    # Inequality Function Constraints:
    if (length(ineqFun) > 0) {
        ineqfun = function(x) {
            ans = NULL
            if(!is.null(eqA)) ans = c(ans, as.vector(ineqA %*% x))
            if (length(ineqFun) > 0)
                for (i in 1:length(ineqFun)) ans = c(ans, ineqFun[[i]](x))
            ans }
    } else {
        ineqfun = NULL
    }  
    ineqLB = c(ineqA.lower, ineqFun.lower)
    ineqUB = c(ineqA.upper, ineqFun.upper)
    
    # Control List:
    if(is.null(control)) control = list()

    # Solve:
    ans = solnp2(
        par = par, 
        fun = fun, 
        grad = NULL, 
        eqfun = eqfun, 
        eqB = eqB, 
        eqgrad = NULL, 
        ineqfun = ineqfun, 
        ineqLB = ineqLB, 
        ineqUB = ineqUB, 
        ineqgrad = NULL, 
        LB = par.lower, 
        UB = par.upper, 
        control = control) 
        
    # Return Value:
    list(
        par = ans$pars, 
        objective = fun(ans$pars), 
        convergence = ans$convergence)
}


# ##############################################################################

