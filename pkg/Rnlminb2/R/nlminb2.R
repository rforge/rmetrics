
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
#  nlminb2                  Nonlinear programming with nonlinear constraints
#  .Log                     Returns log taking care of negative values
################################################################################


nlminb2 <- 
function(
    start, objective, 
    
    eqFun = NULL, 
    leqFun = NULL,
     
    lower = -Inf, 
    upper = Inf,
    
    gradient = NULL, 
    hessian = NULL, 
    
    control = nlminb2Control(),
    
    env = .GlobalEnv)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Nonlinear programming with nonlinear constraints
    
    # Details:
    #                        min f(x)
    #
    #                 lower_i < x_i < upper_i
    #    s/t                h_i(x)  = 0
    #                       g_i(x) <= 0
    
    # Arguments:
    #   start - numeric vector of start values
    #   objective - objective function to be minimized f(x)
    #   eqFun - equal constraint functions h_i(x) = 0
    #   leqFun - less equal constraint functions g_i(x) <= 0
    #   lower, upper - lower and upper bounds
    #   gradient - optional gradient of f(x)
    #   hessian - optional hessian of f(x)
    #   scale - control parameter
    #   control - control list
    #       eval.max - maximum number of evaluations (200)
    #       iter.max - maximum number of iterations (150) 
    #       trace - value of the objective function and the parameters 
    #           is printed every trace'th iteration (0)
    #       abs.tol - absolute tolerance (1e-20)
    #       rel.tol - relative tolerance (1e-10) 
    #       x.tol - X tolerance (1.5e-8)
    #       step.min - minimum step size (2.2e-14)
    
    # Todo:
    #   R, N and alpha should become part of the control list.
   
    # FUNCTION:
    
    # Debug:
    DEBUG = FALSE
    
    # Arg Functions:
    if (DEBUG) {
        print(eqFun)
        print(eqFun(start))
        print(leqFun)
        print(leqFun(start))
    }
    
    # Composed Objective Function:
    if (is.null(eqFun(start))) {
        type = "leq"
        fun <- function(x, r) { 
            objective(x) - 
                r * sum(.Log(-leqFun(x))) }  
    } else if (is.null(leqFun(start))) {
        type = "eq"
        fun <- function(x, r) { 
            objective(x) +
                sum((eqFun(x))^2 / r) } 
    } else {
        type = "both"
        fun <- function(x, r) { 
            objective(x) +
                sum((eqFun(x))^2 / r) - 
                r * sum(.Log(-leqFun(x))) }  
    }
    
    # Compute in global environment:
    fun2 = function(x, r) {
        return(as.double(eval(fun(x, r), env)))
    }
       
    # Debug: 
    if (DEBUG) {
        print(fun)
        print(fun(start, 1))
    }
       
    # Minimization:
    steps.tol <- control$steps.tol
    R <- control$R
    beta <- control$beta
    scale <- control$scale
    
    trace = control$trace
    if (trace > 0) TRACE = TRUE else TRACE = FALSE
    
    control2 = control
    control2[["R"]] <- NULL
    control2[["beta"]] <- NULL
    control2[["steps.max"]] <- NULL
    control2[["steps.tol"]] <- NULL
    control2[["scale"]] <- NULL
    
    counts <- 0
    test <- 0
    while (counts < control$steps.max && test == 0) {
        counts = counts + 1
        ans = nlminb(
            start = start, objective = fun2, 
            gradient = gradient, hessian = hessian, 
            scale = scale, control = control2, lower = lower, upper = upper,
            r = R)
        start = ans$par
        tol = abs((fun(ans$par, R)-objective(ans$par))/objective(ans$par))
        if (!is.na(tol)) 
            if (tol < steps.tol) test = 1
        if (TRACE) {
            print(paste("counts:", counts, "R:", R))
            print(paste("   ", ans$convergence))
            print(paste("   ", ans$message))
            print(ans$par)
            print(fun(ans$par, R))
            print(objective(ans$par))
            print(tol)
        }
        R = beta * R
    } 
    
    if (TRACE) {
        print(paste("type:", type))
        cat("\n\n") 
    } 
    
    # Return Value:
    ans
} 


# ------------------------------------------------------------------------------


.Log <-
function(x) 
{
    # Description:
    #   Returns log taking care of negative values
    
    # FUNCTION:
    
    # Check for negative values:
    x[x < 0] <- 0
    
    # Return Value:
    log(x)
}
    

################################################################################

  