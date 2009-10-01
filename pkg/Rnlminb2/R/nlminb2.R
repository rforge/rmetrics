


################################################################################


.nlminb2 <- 
function(start, objective, eqFun, leqFun, lower = -Inf, upper = Inf,
    gradient = NULL, hessian = NULL, scale = 1, control = list(), ...)
{
    # DESCRIPTION:
    #   Nonlinear programming with nonlinear constraints
    #
    #                        min f(x)
    #
    #                 lower_i < x_i < upper_i
    #    s/t                h_i(x)  = 0
    #                       g_i(x) <= 0
    
    # ARGUMENTS:
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
    
    # Composed Objective Function:
    fun <- function(x, r) { 
        objective(x) + 
        sum((eqFun(x))^2 / r) - 
        r * sum(log(leqFun(x))) }
        
    # Minimization:
    R = 1
    N = 10
    alpha = 0.1
    for (i in 1:N) {
        start = nlminb(
            start = start, objective = fun, 
            gradient = gradient, hessian = hessian, 
            scale = scale, control = control, lower = lower, upper = upper,
            r = R, ...)$par
        R = alpha * R
    }  
    
    # Final Step: 
    ans = nlminb( 
        start = start, objective = fun, 
        gradient = gradient, hessian = hessian, 
        scale = scale, control = control, lower = lower, upper = upper,
        r = R, ...) 
    
    # Return Value:
    ans
} 
    

################################################################################

