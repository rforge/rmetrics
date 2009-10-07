
# Log
# .nlminb2
# .optim2

################################################################################


Log = function(x) {
    print(paste("LOG", x))
    x[abs(x) < sqrt(.Machine$double.eps)] <-  sqrt(.Machine$double.eps)
    ans = log(x)
    ans[is.infinite(ans)] <- -10
    ans
}


Log = function(x) {
    x[x < 0] <- 0
    # print("LOG")
    # print(x)
    ans = log(x)
    # print(x)
    ans
}


# ------------------------------------------------------------------------------


.nlminb2 <- 
function(start, objective, eqFun = NULL, leqFun = NULL, 
    lower = -Inf, upper = Inf,
    gradient = NULL, hessian = NULL, scale = 1, 
    R = 1, beta = 0.01, trace = FALSE, control = list())
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
    
    # DEBUG
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
                r * sum(Log(-leqFun(x))) }  
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
                r * sum(Log(-leqFun(x))) }  
    }
    .setnlminb2Env(fun = fun)
    if (DEBUG) {
        print(fun)
        print(fun(start, 1))
    }
       
    # Minimization:
    counts = 0
    test = 0
    while (counts < 10 && test == 0) {
        counts = counts + 1
        ans = nlminb(
            start = start, objective = fun, 
            gradient = gradient, hessian = hessian, 
            scale = scale, control = control, lower = lower, upper = upper,
            r = R)
        start = ans$par
        tol = abs((fun(ans$par, R)-objective(ans$par))/objective(ans$par))
        if (!is.na(tol)) 
            if (tol < 1e-6) test = 1
        if (trace) {
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
    
    if (trace) {
        print(paste("type:", type))
        cat("\n\n") 
    } 
    
    # Return Value:
    ans
} 
    

################################################################################

  