
require(Rnlminb2)
require(Rdonlp2)
require(Rsolnp2)


################################################################################
    
    
solnpNLP <- 
function(
    par, fun, 
    par.lower = NULL, par.upper = NULL,
    eqA = NULL, eqA.bound = NULL,
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
    eqFun = list(), eqFun.bound = NULL,
    ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL,
    trace = FALSE, control = NULL)
{
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
    ans = solnp(
        pars = par, 
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
        trace = trace,
        control = control) 
        
    # Return Value:
    list(
        par = ans$pars, 
        objective = fun(ans$pars), 
        convergence = ans$convergence)
}


# ------------------------------------------------------------------------------

      
donlpNLP <- function(
    par, fun, 
    par.lower = NULL, par.upper = NULL,
    eqA = NULL, eqA.bound = NULL,
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
    eqFun = list(), eqFun.bound = NULL,
    ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL,
    trace = FALSE, control = list())
{
    # Description:
    #   Universal function wrapper for solver solnp().
    
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
        control.fun = function(lst) {return(TRUE) }, 
        env = .GlobalEnv, 
        name = NULL)
        
    # Return Value:
    list(
        par = ans$par, 
        objective = fun(ans$par), 
        convergence = NA)

}



# ------------------------------------------------------------------------------
    
       
nlminbNLP <- 
function(
    par, fun, 
    par.lower = NULL, par.upper = NULL,
    eqA = NULL, eqA.bound = NULL,
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
    eqFun = list(), eqFun.bound = NULL,
    ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL,
    scale = 1, trace = FALSE, R = 1, beta = 0.1, control = NULL)
{
    # Description:
    #   Universal function wrapper for solver solnp().
    
    # Details:
    #   .nlminb2 <- function(
    #       start, objective, 
    #       eqFun = NULL, 
    #       leqFun = NULL, 
    #       lower = -Inf, upper = Inf,
    #       gradient = NULL, hessian = NULL, scale = 1, 
    #       R = R, beta = beta, control = list())
    
    # FUNCTION:
    
    .setnlminb2Env(eqA = eqA)
    .setnlminb2Env(eqA.bound = eqA.bound)
    
    .setnlminb2Env(eqFun = eqFun)
    .setnlminb2Env(eqFun.bound = eqFun.bound)
    
    .setnlminb2Env(ineqA = ineqA)
    .setnlminb2Env(ineqA.lower = ineqA.lower)
    .setnlminb2Env(ineqA.upper = ineqA.upper)
    
    .setnlminb2Env(ineqFun = ineqFun)
    .setnlminb2Env(ineqFun.lower = ineqFun.lower)
    .setnlminb2Env(ineqFun.upper = ineqFun.upper)
    
    # Box Constraints:
    if (is.null(par.lower)) par.lower = -Inf
    if (is.null(par.upper)) par.upper = Inf
    
    # Equality Constraints:
    eqfun = function(x) {
        ans = NULL
        if(!is.null(eqA)) {
            ans = c(ans, eqA %*% x - eqA.bound)
            ans = c(ans, eqA %*% x)
        }
        if (length(eqFun) > 0) 
            for (i in 1:length(eqFun)) 
                ans = c(ans, eqFun[[i]](x) - eqFun.bound[i])
        ans
    }
    .setnlminb2Env(eqfun = eqfun)
    
    # Inequality Constraints:
    leqfun = function(x) {
        ans = NULL
        if(!is.null(ineqA)) ans = c(ans, ineqA %*% x)
        if (length(ineqFun) > 0) 
            for (i in 1:length(ineqFun)) 
                ans = c(ans, ineqFun[[i]](x) - ineqFun.upper[i])
        if (length(ineqFun) > 0) 
            for (i in 1:length(ineqFun)) 
                ans = c(ans, -ineqFun[[i]](x) + ineqFun.lower[i])
        ans
    }
    .setnlminb2Env(leqfun = leqfun)
    
    # Solve:
    ans = .nlminb2(
        start = par, 
        objective = fun, 
        eqFun = eqfun, 
        leqFun = leqfun, 
        lower = par.lower, 
        upper = par.upper,
        gradient = NULL, 
        hessian = NULL, 
        scale = scale, 
        trace = trace,
        control = list())
    
    # Return Value:
    list(
        par = ans$par, 
        objective = fun(ans$par), 
        convergence = ans$convergence)
}    


# ------------------------------------------------------------------------------
    
    
optimNLP <- 
function(
    par, fun, 
    par.lower = NULL, par.upper = NULL,
    eqA = NULL, eqA.bound = NULL,
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
    eqFun = list(), eqFun.bound = NULL,
    ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL,
    trace = FALSE, R = 1, beta = 0.1, control = NULL)
{
    # Description:
    #   Universal function wrapper for solver solnp().
    
    # Details:
    #   .optim <- function(
    #       start, objective, 
    #       eqFun = NULL, 
    #       leqFun = NULL, 
    #       lower = -Inf, upper = Inf,
    #       gradient = NULL, hessian = NULL, scale = 1, 
    #       R = R, beta = beta, control = list())
    
    # FUNCTION:
    
    .setnlminb2Env(eqA = eqA)
    .setnlminb2Env(eqA.bound = eqA.bound)
    
    .setnlminb2Env(eqFun = eqFun)
    .setnlminb2Env(eqFun.bound = eqFun.bound)
    
    .setnlminb2Env(ineqA = ineqA)
    .setnlminb2Env(ineqA.lower = ineqA.lower)
    .setnlminb2Env(ineqA.upper = ineqA.upper)
    
    .setnlminb2Env(ineqFun = ineqFun)
    .setnlminb2Env(ineqFun.lower = ineqFun.lower)
    .setnlminb2Env(ineqFun.upper = ineqFun.upper)
    
    # Box Constraints:
    if (is.null(par.lower)) par.lower = -Inf
    if (is.null(par.upper)) par.upper = Inf
    
    # Equality Constraints:
    eqfun = function(x) {
        ans = NULL
        if(!is.null(eqA)) {
            ans = c(ans, eqA %*% x - eqA.bound)
            ans = c(ans, eqA %*% x)
        }
        if (length(eqFun) > 0) 
            for (i in 1:length(eqFun)) 
                ans = c(ans, eqFun[[i]](x) - eqFun.bound[i])
        ans
    }
    .setnlminb2Env(eqfun = eqfun)
    
    # Inequality Constraints:
    leqfun = function(x) {
        ans = NULL
        if(!is.null(ineqA)) ans = c(ans, ineqA %*% x)
        if (length(ineqFun) > 0) 
            for (i in 1:length(ineqFun)) 
                ans = c(ans, ineqFun[[i]](x) - ineqFun.upper[i])
        if (length(ineqFun) > 0) 
            for (i in 1:length(ineqFun)) 
                ans = c(ans, -ineqFun[[i]](x) + ineqFun.lower[i])
        ans
    }
    .setnlminb2Env(leqfun = leqfun)
    
       
    # Solve:
    ans = .optim2(
        start = par, 
        objective = fun, 
        eqFun = eqfun, 
        leqFun = leqfun, 
        lower = par.lower, 
        upper = par.upper,
        gradient = NULL, 
        R = R,
        beta = beta,
        trace = trace, 
        control = list())
    
    # Return Value:
    list(
        par = ans$par, 
        objective = fun(ans$par), 
        convergence = ans$convergence)
}    
    
   
    
################################################################################


.powell <- 
function()
{
    # OK all 4
    
    start = c(-2, 2, 2, -1, -1)
    
    fun = function(x) { exp(x[1]*x[2]*x[3]*x[4]*x[5]) }
    
    eqFun = list(
        function(x) x[1]*x[1]+x[2]*x[2]+x[3]*x[3]+x[4]*x[4]+x[5]*x[5],
        function(x) x[2]*x[3]-5*x[4]*x[5],
        function(x) x[1]*x[1]*x[1]+x[2]*x[2]*x[2])
    eqFun.bound = c(10, 0, -1)
    
    ans.donlp = donlpNLP(start, fun, 
        eqFun = eqFun, eqFun.bound = eqFun.bound)
        
    ans.solnp = solnpNLP(start, fun, 
        eqFun = eqFun, eqFun.bound = eqFun.bound)
    
    ans.nlminb = nlminbNLP(start, fun, 
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        R = 1, beta = 0.01)
        
    ctrl <- list(
        trace = 0, fnscale = 1, parscale = rep.int(1, length(par)), 
        ndeps = rep.int(1e-6, length(start)), maxit = 1000L, 
        abstol = -Inf, reltol = .Machine$double.eps, alpha = 1, 
        beta = 0.5, gamma = 2, REPORT = 10, type = 1, lmm = 15, 
        factr = 10000, pgtol = 0, tmax = 10, temp = 10)
    ans.optim = optimNLP(start, fun, 
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        R = abs(fun(start)), beta = 0.01, trace = TRUE, control = ctrl)
        
    result.par = signif(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par, ans.optim$par), 6)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par), fun(ans.optim$par))
    cbind(result.par, result.fun) 
    
    rbind(
        signif(c(eqFun[[1]](ans.donlp$par), eqFun[[2]](ans.donlp$par), eqFun[[3]](ans.donlp$par)), 10),
        signif(c(eqFun[[1]](ans.solnp$par), eqFun[[2]](ans.solnp$par), eqFun[[3]](ans.solnp$par)), 10),
        signif(c(eqFun[[1]](ans.nlminb$par), eqFun[[2]](ans.nlminb$par), eqFun[[3]](ans.nlminb$par)), 10),
        signif(c(eqFun[[1]](ans.optim$par), eqFun[[2]](ans.optim$par), eqFun[[3]](ans.optim$par)), 10))
    
}


# ------------------------------------------------------------------------------


.wright4 <- 
function()
{
    # OK 1 2
    
    start = c(1, 1, 1, 1, 1)
    
    fun <- function(x){
        (x[1]-1)^2+(x[1]-x[2])^2+(x[2]-x[3])^3+(x[3]-x[4])^4+(x[4]-x[5])^4}
        
    eqFun = list(
        function(x) x[1]+x[2]*x[2]+x[3]*x[3]*x[3],
        function(x) x[2]-x[3]*x[3]+x[4],
        function(x) x[1]*x[5] ) 
    eqFun.bound = c(2+3*sqrt(2), -2+2*sqrt(2), 2)
    
    ans.donlp = donlpNLP(start, fun, 
        eqFun = eqFun, eqFun.bound = eqFun.bound)
        
    ans.solnp = solnpNLP(start, fun, 
        eqFun = eqFun, eqFun.bound = eqFun.bound)
       
    ans.nlminb = nlminbNLP(start, fun, 
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        R = 1, beta = 0.1, trace = TRUE)
        
    ans.optim = optimNLP(start, fun, 
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        R = abs(fun(start)), beta = 0.1, trace = TRUE)
        
    result.par = signif(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par, ans.optim$par), 6)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par), fun(ans.optim$par))
    cbind(result.par, result.fun) 
    
    rbind(
        signif(c(eqFun[[1]](ans.donlp$par), eqFun[[2]](ans.donlp$par), eqFun[[3]](ans.donlp$par)), 10),
        signif(c(eqFun[[1]](ans.solnp$par), eqFun[[2]](ans.solnp$par), eqFun[[3]](ans.solnp$par)), 10),
        signif(c(eqFun[[1]](ans.nlminb$par), eqFun[[2]](ans.nlminb$par), eqFun[[3]](ans.nlminb$par)), 10),
        signif(c(eqFun[[1]](ans.optim$par), eqFun[[2]](ans.optim$par), eqFun[[3]](ans.optim$par)), 10))
    
}


# ------------------------------------------------------------------------------


.box <-
function()
{
    # OK all 4
    
    start = c(1.1, 1.1, 9)
    
    fun <- function(x) { -x[1]*x[2]*x[3] }
    par.lower = rep(1, 3)
    par.upper = rep(10, 3)
    
    eqFun <- list(
        function(x) 4*x[1]*x[2]+2*x[2]*x[3]+2*x[3]*x[1] )
    eqFun.bound = 100

    ans.donlp = donlpNLP(start, fun, 
        par.lower = par.lower, par.upper = par.upper, 
        eqFun = eqFun, eqFun.bound = eqFun.bound)
        
    ans.solnp = solnpNLP(start, fun, 
        par.lower = par.lower, par.upper = par.upper, 
        eqFun = eqFun, eqFun.bound = eqFun.bound)
        
    ans.nlminb = nlminbNLP(start, fun, 
        par.lower = par.lower, par.upper = par.upper, 
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        R = 1, beta = 0.1)  
        
    ans.optim = optimNLP(start, fun, 
        par.lower = par.lower, par.upper = par.upper, 
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        R = abs(fun(start)), beta = 0.1)  
        
    result.par = signif(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par, ans.optim$par), 6)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par), fun(ans.optim$par))
    cbind(result.par, result.fun) 
    
    rbind(
        signif(eqFun[[1]](ans.donlp$par), 10),
        signif(eqFun[[1]](ans.solnp$par), 10),
        signif(eqFun[[1]](ans.nlminb$par),10),
        signif(eqFun[[1]](ans.optim$par), 10))
         
}


# ------------------------------------------------------------------------------


.wright9 <-
function()
{
    start = c(1, 1, 1, 1, 1)
    
    fun <- function(x){
        10*x[1]*x[4]-6*x[3]*x[2]*x[2]+x[2]*(x[1]*x[1]*x[1])+
                9*sin(x[5]-x[3])+x[5]^4*x[4]*x[4]*x[2]*x[2]*x[2] }
    
    ineqFun <- list(
        function(x) x[1]*x[1]+x[2]*x[2]+x[3]*x[3]+x[4]*x[4]+x[5]*x[5],
        function(x) x[1]*x[1]*x[3]-x[4]*x[5],
        function(x) x[2]*x[2]*x[4]+10*x[1]*x[5]) 
    ineqFun.lower = c(-100,  -2,   5)
    ineqFun.upper = c(  20, 100, 100)
    
    ans.donlp = donlpNLP(start, fun = fun, 
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper)
        
    ans.solnp = solnpNLP(start, fun = fun, 
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper)
    
    start = ans.solnp$par
    ans.nlminb = nlminbNLP(start, fun = fun, 
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper, 
        R = 1, beta = 0.1, trace = TRUE)
        
    ans.optim = optimNLP(start, fun = fun, 
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper, 
        R = 1, beta = 0.1, trace = TRUE)
        
    result.par = signif(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par), 6)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par))
    cbind(result.par, result.fun)   
    
}


# ------------------------------------------------------------------------------


.alkylation <-
function()
{
    start = c(17.45, 12, 110, 30, 19.74, 89.2, 92.8, 8, 3.6, 155)
    
    fun <- function(x) { -0.63*x[4]*x[7]+50.4*x[1]+3.5*x[2]+x[3]+33.6*x[5] }
    par.lower = c( 0,  0,   0, 10,  0, 85, 10, 3, 1, 145)
    par.upper = c(20, 16, 120, 50, 20, 93, 95,12, 4, 162)
     
    eqFun <- list(
        function(x) 98*x[3]-0.1*x[4]*x[6]*x[9]-x[3]*x[6],
        function(x) 1000*x[2]+100*x[5]-100*x[1]*x[8],
        function(x) 122*x[4]-100*x[1]-100*x[5])
    eqFun.bound = c(0, 0, 0)
    
    ineqFun <- list(
        function(x) (1.12*x[1]+0.13167*x[1]*x[8]-0.00667*x[1]*x[8]*x[8])/x[4],
        function(x) (1.098*x[8]-0.038*x[8]*x[8]+0.325*x[6]+57.25)/x[7],
        function(x) (-0.222*x[10]+35.82)/x[9],
        function(x) (3*x[7]-133)/x[10])
    ineqFun.lower = c(  0.99,   0.99,  0.9,   0.99)
    ineqFun.upper = c(100/99, 100/99, 10/9, 100/99)
    
    ans.donlp = donlpNLP(start, fun = fun, 
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper)
    
    ctrl = list(rho = 0, trace = 0)
    ans.solnp = solnpNLP(start, fun = fun, 
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper,
        control = ctrl)
        
    ans.nlminb = nlminbNLP(start, fun = fun, 
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper,
        R = 1, beta = 0.1, trace = TRUE)
        
    ans.optim = optimNLP(start, fun = fun, 
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper,
        R = abs(fun(start)), beta = 0.1)
    
    result.par = signif(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par, ans.optim$par), 6)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par), fun(ans.optim$par))
    cbind(result.par, result.fun)   
    
}


# ------------------------------------------------------------------------------


.entropy <- 
function()
{
    set.seed(1953)
    start = runif(10, 0, 1)
    
    fun <- function(x) {
        m = length(x)
        f = -sum(log(x[i]))
        vnorm = sum((x-1)^2)^(1/2) 
        f - log(vnorm + 0.1) 
        }
    par.lower = rep(0, 10)
     
    eqFun <- list(
        function(x) sum(x) )
    eqFun.bound = 10
    
    ans.donlp = donlpNLP(start, fun, 
        par.lower = par.lower,  
        eqFun = eqFun, eqFun.bound = eqFun.bound)
        
    ans.solnp = solnpNLP(start, fun, 
        par.lower = par.lower,  
        eqFun = eqFun, eqFun.bound = eqFun.bound)
        
    ans.nlminb = nlminbNLP(start, fun, 
        par.lower = par.lower,  
        eqFun = eqFun, eqFun.bound = eqFun.bound)   
        
    ans.optim = optimNLP(start, fun, 
        par.lower = par.lower,  
        eqFun = eqFun, eqFun.bound = eqFun.bound)   
        
    result.par = round(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par, ans.optim$par), 8)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par), fun(ans.optim$par))
    cbind(result.par, result.fun)     

}


# ------------------------------------------------------------------------------


.rosensuzuki <- 
function()
{
    start = 2*c(1, 1, 1, 1)
    
    fun <- function(x) 
        x[1]*x[1]+x[2]*x[2]+2*x[3]*x[3]+x[4]*x[4]-5*x[1]-5*x[2]-21*x[3]+7*x[4]

    ineqFun <- list(
        function(x) 8-x[1]*x[1]-x[2]*x[2]-x[3]*x[3]-x[4]*x[4]-x[1]+x[2]-x[3]+x[4],
        function(x) 10-x[1]*x[1]-2*x[2]*x[2]-x[3]*x[3]-2*x[4]*x[4]+x[1]+x[4],
        function(x) 5-2*x[1]*x[1]-x[2]*x[2]-x[3]*x[3]-2*x[1]+x[2]+x[4] )
    ineqFun.lower = rep(   0, 3)
    ineqFun.upper = rep(1000, 3)
    
    ans.donlp = donlpNLP(start, fun,  
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper)
        
    ans.solnp = solnpNLP(start, fun,  
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper)
        
    ans.nlminb = nlminbNLP(start, fun,  
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper, 
        trace = TRUE)     
        
    ans.optim = optimNLP(start, fun,  
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper, 
        trace = TRUE)     
        
    result.par = signif(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par, ans.optim$par), 6)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par), fun(ans.optim$par))
    cbind(result.par, result.fun) 
    
    rbind(
        signif(c(ineqFun[[1]](ans.donlp$par), ineqFun[[2]](ans.donlp$par), ineqFun[[3]](ans.donlp$par)), 10),
        signif(c(ineqFun[[1]](ans.solnp$par), ineqFun[[2]](ans.solnp$par), ineqFun[[3]](ans.solnp$par)), 10),
        signif(c(ineqFun[[1]](ans.nlminb$par),ineqFun[[2]](ans.nlminb$par),ineqFun[[3]](ans.nlminb$par)), 10),
        signif(c(ineqFun[[1]](ans.optim$par), ineqFun[[2]](ans.optim$par), ineqFun[[3]](ans.optim$par)), 10))  

}


#-------------------------------------------------------------------------------


.sharperatio <-
function()
{
    # Sharpe ratio
    
    require(fEcofin)
    data(LPP2005REC)
    ret = as.matrix(LPP2005REC[, 2:7])
    .setnlminb2Env(ret = ret)
    Mean = colMeans(ret)
    Cov = cov(ret)
    .setnlminb2Env(Mean = colMeans(ret))
    .setnlminb2Env(Cov = cov(ret))
    
    set.seed(1953)
    r = runif(6)
    start = r/sum(r)
    
    fun <- function(x) {
        return = (Mean %*% x)[[1]]
        risk = (t(x) %*% Cov %*% x)[[1]]
        -return/risk }
    par.lower = rep(0, 6)
    par.upper = rep(1, 6)  
    
    eqFun <- list(
        function(x) { sum(x) } )
    eqFun.bound = 1
       
    ans.donlp = donlpNLP(start, fun,  
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound )
      
    ctrl = list(delta = 1e-10, tol = 1e-8, trace = 0)  
    ans.solnp = solnpNLP(start, fun,  
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound, control = ctrl )
          
    ans.nlminb = nlminbNLP(start, fun,  
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound, 
        scale = 1, trace = FALSE,
        control = list() )  
        
    ans.optim = optimNLP(start, fun,  
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound, 
        trace = FALSE )  
        
    result.par = signif(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par, ans.optim$par), 6)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par), fun(ans.optim$par))
    cbind(result.par, result.fun)  
    
    rbind(
        signif(eqFun[[1]](ans.donlp$par), 10),
        signif(eqFun[[1]](ans.solnp$par), 10),
        signif(eqFun[[1]](ans.nlminb$par),10),
        signif(eqFun[[1]](ans.optim$par), 10))
        
}


# ------------------------------------------------------------------------------


    require(fEcofin)
    data(LPP2005REC)
    ret = as.matrix(LPP2005REC[, 2:7])
    .setnlminb2Env(ret = ret)
    Mean = colMeans(ret)
    Cov = cov(ret)
    .setnlminb2Env(Mean = colMeans(ret))
    .setnlminb2Env(Cov = cov(ret))
    
    set.seed(1953)
    r = runif(6)
    start = r/sum(r)
    
    .VaR <- function(x) { 
        quantile(x, probs = 0.05, type = 1) }
    .CVaR <- function(x) {   
        VaR = .VaR(x)
        VaR - 0.5 * mean(((VaR-ret) + abs(VaR-ret))) / 0.05 }     
    fun <- function(x) {
        port = as.vector(ret %*% x)
        (-.CVaR(-port) / .CVaR(port))[[1]] }
    par.lower = rep(0, 6)
    par.upper = rep(1, 6)  
    
    eqFun <- list(
        function(x) sum(x) )
    eqFun.bound = 1
    
   
    ans.donlp = donlpNLP(start, fun,  
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound )
      
    ctrl = list(delta = 1e-10, tol = 1e-8, trace = 0)  
    ans.solnp = solnpNLP(start, fun,  
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound, control = ctrl )
          
    ans.nlminb = nlminbNLP(start, fun,  
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound )  
        
    ans.optim = optimNLP(start, fun,  
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound )  
        
    result.par = signif(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par, ans.optim$par), 6)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par), fun(ans.optim$par))
    cbind(result.par, result.fun)
    
    rbind(
        signif(eqFun[[1]](ans.donlp$par), 10),
        signif(eqFun[[1]](ans.solnp$par), 10),
        signif(eqFun[[1]](ans.nlminb$par), 10),
        signif(eqFun[[1]](ans.optim$par), 10))


#-------------------------------------------------------------------------------


.kapparatio <- 
function()
{
    data(dji30ret)
    dj30=as.matrix(dji30ret)
    
    .kappa <- function(port, r, n)
    {
        z = mean((port< r) * (r-port)^n)
        sg = sign(z)
        (mean(port) - r) / (sg*abs(z)^(1/n))
    }
    
    
    .fn1 <- function(x, ret, r, n)
    {
        port = ret%*%x
        obj = -.kappa(port,r,n)
        return(obj)
    }
    
    # abs(sum) of weights ==1
    .eqn1  <- function(x, ret, r, n)
    {
        sum(abs(x))
    }
    
    LB = rep(0,30)
    UB = rep(0.1,30)    
    .x0 = rep(1/30,30)
    ctrl = list(delta = 1e-10, tol = 1e-8, trace = 0)
    ans = solnp(.x0, fun = .fn1, eqfun = .eqn1, eqB = 1, LB = LB, UB = UB, 
        control=ctrl, ret=dj30, r = 0, n = 2)
   

}


################################################################################


.markowitz <-
function()
{
    # Markowitz Portfolio:
    
    require(fEcofin)
    data(LPP2005REC)
    ret = 100 * as.matrix(LPP2005REC[, 2:7])
    .setnlminb2Env(ret = ret)
    Mean = colMeans(ret)
    Cov = cov(ret)
    targetReturn = mean(Mean)
    .setnlminb2Env(Mean = colMeans(ret))
    .setnlminb2Env(Cov = cov(ret))
    .setnlminb2Env(targetReturn = targetReturn)
    

    # start = rep(1/6, times = 6)
    start = Mean/sum(Mean)
    # start = rep(0, times = 6); start[which.max(Mean)] = 1
    
    fun <- function(x) {
        risk = (t(x) %*% Cov %*% x)[[1]]
        risk }
    par.lower = rep(0, 6)
    par.upper = rep(1, 6)  
    
    eqFun <- list(
        function(x) sum(x), 
        function(x) (Mean %*% x)[[1]] )
    eqFun.bound = c(1, targetReturn)
    
    ineqFun <- list(
        function(x) x[1]+x[4],
        function(x) x[2]+x[5]+x[6])
    ineqFun.lower = c( 0.2,   0)
    ineqFun.upper = c( 1.0, 0.8)
    
    ans.donlp = donlpNLP(start, fun = fun, 
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper)
    
    ctrl = list(rho = 0, trace = 0)
    ans.solnp = solnpNLP(start, fun = fun, 
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper,
        control = ctrl)
        
    ans.nlminb = nlminbNLP(start, fun = fun, 
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper,
        R = 1, beta = 0.1, trace = TRUE)
        
    ctrl <- list(
        trace = 0, fnscale = 1, parscale = rep.int(1, length(par)), 
        ndeps = rep.int(1e-6, length(start)), maxit = 1000L, 
        abstol = -Inf, reltol = .Machine$double.eps, alpha = 1, 
        beta = 0.5, gamma = 2, REPORT = 10, type = 1, lmm = 15, 
        factr = 10000, pgtol = 0, tmax = 10, temp = 10)
    #ctrl = list()
    ans.optim = optimNLP(start, fun = fun, 
        par.lower = par.lower, par.upper = par.upper,
        eqFun = eqFun, eqFun.bound = eqFun.bound,
        ineqFun = ineqFun, 
        ineqFun.lower = ineqFun.lower, ineqFun.upper = ineqFun.upper,
        R = fun(start), beta = 0.1, trace = TRUE, control = ctrl)
    
    result.par = 100*round(rbind(ans.donlp$par, ans.solnp$par, ans.nlminb$par, ans.optim$par), 3)
    result.fun = c(fun(ans.donlp$par), fun(ans.solnp$par), fun(ans.nlminb$par), fun(ans.optim$par))
    cbind(result.par, result.fun)   
    sum(ans.optim$par)
    (Mean %*% ans.optim$par)[[1]] - targetReturn

}
