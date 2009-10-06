

# .optim2


################################################################################

    
.optim2 <- 
function(start, objective, eqFun = NULL, leqFun = NULL, 
    lower = -Inf, upper = Inf,
    gradient = NULL, 
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
            ans = objective(x) - r * sum(Log(leqFun(x))) 
            if(is.infinite(ans)) {
                print(ans)
                ans = 2*abs(objective(x))
            }
            ans }  
    } else if (is.null(leqFun(start))) {
        type = "eq"
        fun <- function(x, r) { 
            ans = objective(x) + sum((eqFun(x))^2 / r) 
            if(is.infinite(ans)) {
                print(ans)
                ans = 2*abs(objective(x))
            }
            ans }   
    } else {
        type = "both"
        fun <- function(x, r) { 
            ans1 = objective(x) 
            ans2 = sum((eqFun(x))^2 / r) 
            ans3 = r * sum(Log(leqFun(x))) 
            ans = ans1 + ans2 - ans3
            if(is.infinite(ans)) {
                print(c(ans1, ans2, ans3))
                ans = 2*abs(objective(x))
            }
            ans }  
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
        ans = optim(
            par = start, fn = fun, 
            method = "L-BFGS-B",
            gr = gradient, 
            control = control, lower = lower, upper = upper,
            r = R)    
        start = ans$par
        control$parscale = rep(mean(start), times = length(start))
        control$fnscale = abs(objective(start))
        tol = abs((fun(ans$par, R)-objective(ans$par))/objective(ans$par))
        if(tol < 1e-6) test = 1
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
        print(fun(ans$par, R/beta))
        print(objective(ans$par))
        cat("\n\n") 
    }
    
    # Return Value:
    ans
}   


################################################################################

  