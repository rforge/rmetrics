
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
#  donlp2NLP               Function wrapper for solver donlp2()
################################################################################


donlp2NLP <- 
function(
    par, fun, 
    
    par.lower = NULL, 
    par.upper = NULL,
    
    eqA = NULL, 
    eqA.bound = NULL,
    
    ineqA = NULL, 
    ineqA.lower = NULL, 
    ineqA.upper = NULL,
    
    eqFun = list(), 
    eqFun.bound = NULL,
    
    ineqFun = list(), 
    ineqFun.lower = NULL, 
    ineqFun.upper = NULL,
    
    control = list())
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Function wrapper for solver donlp2
    
    # Arguments:
    
    # Details:
    #   donlp2 <- function (
    #       par, fn, 
    #           par.upper = rep(+Inf, length(par)), 
    #           par.lower = rep(-Inf, length(par)), 
    #       A = NULL, 
    #           lin.upper = rep(+Inf, length(par)), 
    #           lin.lower = rep(-Inf, length(par)), 
    #       nlin = list(), 
    #           nlin.upper = rep(+Inf, length(nlin)), 
    #           nlin.lower = rep(-Inf, length(nlin)), 
    #       control = donlp2Control(), 
    #       control.fun = function(lst) {return(TRUE)}, 
    #       env = .GlobalEnv, 
    #       name = NULL)
    
    # FUNCTION:
    
    # Control List:
    ctrl = donlp2Control()
    if (length(control) > 0)
        for (name in names(control)) ctrl[name] = control[name]
    control = ctrl
    
    # Box Constraints:
    if (is.null(par.lower)) par.lower = rep(-Inf, length(par))
    if (is.null(par.upper)) par.upper = rep(+Inf, length(par))
    
    # Linear Constraints:
    A = rbind(eqA, ineqA)
    lin.lower = c(eqA.bound, ineqA.lower)
    lin.upper = c(eqA.bound, ineqA.upper)
    
    # Nonlinear Constraints:
    if ((length(eqFun) + length(ineqFun)) == 0) {
        nlin = list()
        nlin.lower = rep(-Inf, length(nlin))
        nlin.upper = rep(+Inf, length(nlin))
    } else {
        nlin = list()
        if (length(eqFun) > 0) nlin = c(nlin, eqFun)
        if (length(ineqFun) > 0) nlin = c(nlin, ineqFun)
        nlin.lower = c(eqFun.bound, ineqFun.lower)
        nlin.upper = c(eqFun.bound, ineqFun.upper)
    }
    
    # Control List:
    if(length(control) == 0) control = donlp2Control()
    
    # Solve:
    ans = donlp2(
        par = par, 
        fn = fun, 
        par.upper = par.upper, 
        par.lower = par.lower, 
        A = A, 
        lin.upper = lin.upper, 
        lin.lower = lin.lower, 
        nlin = nlin, 
        nlin.upper = nlin.upper, 
        nlin.lower = nlin.lower, 
        control = control, 
        control.fun = function(lst) {return(TRUE)}, 
        env = .GlobalEnv, 
        name = NULL)
        
    # Return Value:
    list(
        opt = ans,
        par = ans$par, 
        objective = fun(ans$par), 
        convergence = NA,
        message = ans$message)
}


################################################################################

