
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
#  nlminb2 NLP              Function wrapper for solver nlminb2()
################################################################################


nlminb2NLP <- 
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
    #   Function wrapper for solver nlminb2()
    
    # Details:
    #   nlminb2 <- function(
    #       start, objective, 
    #       eqFun = NULL, 
    #       leqFun = NULL, 
    #       lower = -Inf, upper = Inf,
    #       gradient = NULL, hessian = NULL, scale = 1, 
    #       control = nlminb2Control(), env = .GlobalEnv)
    
    # FUNCTION:
    
    # Environment Setting:
    env = .GlobalEnv
    
    # Control List:
    ctrl = nlminb2Control()
    if (length(control) > 0)
        for (name in names(control)) ctrl[name] = control[name]
    control = ctrl

    # Box Constraints:
    if (is.null(par.lower)) par.lower = -Inf
    if (is.null(par.upper)) par.upper = Inf
    
    # Equality Constraints:
    eqfun <- function(x) {
        ans = NULL
        if(!is.null(eqA)) {
            ans = c(ans, eqA %*% x - eqA.bound)
            ans = c(ans, eqA %*% x)
        }
        if (length(eqFun) > 0) 
            for (i in 1:length(eqFun)) 
                ans = c(ans, eqFun[[i]](x) - eqFun.bound[i])
        return(as.double(eval(ans, env)))
    }
    
    # Inequality Constraints:
    leqfun <- function(x) {
        ans = NULL
        if(!is.null(ineqA)) ans = c(ans, ineqA %*% x)
        if (length(ineqFun) > 0) 
            for (i in 1:length(ineqFun)) 
                ans = c(ans, ineqFun[[i]](x) - ineqFun.upper[i])
        if (length(ineqFun) > 0) 
            for (i in 1:length(ineqFun)) 
                ans = c(ans, -ineqFun[[i]](x) + ineqFun.lower[i])
        return(as.double(eval(ans, env)))
    }
    
    # Solve:
    ans = nlminb2(
        start = par, 
        objective = fun, 
        eqFun = eqfun, 
        leqFun = leqfun, 
        lower = par.lower, 
        upper = par.upper,
        gradient = NULL, 
        hessian = NULL,  
        control = control,
        env = env)
    
    # Return Value:
    list(
        opt = ans,
        par = ans$par, 
        objective = fun(ans$par), 
        convergence = ans$convergence,
        message = ans$message)
}        
   

################################################################################

  