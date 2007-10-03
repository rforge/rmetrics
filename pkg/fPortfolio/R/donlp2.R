

# DW
# .onLoad <- function(libname, pkgname){
#   verbose <- .Options$Hverbose
#   if(!length(verbose) || verbose){
#     cat("Rdonlp2 - a wrapper library for \"DONLP2 (C) Peter Spellucci\"\n\n")
#   }
#   library.dynam("Rdonlp2", pkgname, libname)
#   invisible()
# }


# ------------------------------------------------------------------------------


rdonlp2Control <- 
function(            
    # setup
    iterma = 4000, nstep = 20,fnscale = 1,
    report = FALSE, rep.freq = 1,
    # perfomance and tunings
    tau0 = 1.0, tau = 0.1, del0 = 1.0,
    # termination criteria
    epsx = 1e-5, delmin = 0.1*del0,
    epsdif = 1e-8, nreset.multiplier = 1,
    # numerical differentiation
    difftype = 3, epsfcn = 1e-16, taubnd = 1.0,
    hessian = FALSE,
    # information
    te0 = TRUE, te1 = FALSE, te2 = FALSE, te3 = FALSE,
    silent = FALSE, intakt = TRUE )
{
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


# ------------------------------------------------------------------------------


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
    
    # FUNCTION:
    
    # use analytical gradients?
    if (is.function(attr(fn, "gr")) &
        all(lapply(nlin, function(e)is.function(attr(e,"gr"))))){
        control["analyt"] = TRUE
    } else {
        control["analyt"] = FALSE
    }
  
    # check parameter and its box constraints
    if (length(par) != length(par.upper) | length(par) != length(par.lower) ){
        stop("# of elements for box constraints != # of parameters")
    }

    # check linear constraints matrix A
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
  
    # nonlinear constraints
    num.nlin <- length(nlin)
    if (length(nlin.upper)!=num.nlin | length(nlin.lower)!=num.nlin)
    stop("# of bounds for nonlinear constraints should be equal to length(nlin)")
    # concatenate bounds for internal use
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
                return(as.double(eval(fn@gr(p), env)))
            }
            return(as.double(eval((nlin[[fun.id]]@gr)(p), env)))
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
    
    # start donlp2
    tryCatch(
        # start donlp2
        ans <- .Call("call_donlp2",
            as.double(par),
            as.integer(num.lin),
            as.integer(num.nlin),
            fsilent,
            name,
            nchar(name),
            as.double(lbd), as.double(ubd),
            as.double(conmat),
            control,
            accfun,
            confun, environment(confun), 
            PACKAGE = "Rdonlp2"),
            # ensure to free memory and close .mes .pro files if opened
            finally=.Call("teardown", 0, 
            PACKAGE = "Rdonlp2"))
    ans$nr.update <- matrix(ans$nr.update, nr = length(par))
    if (control$hessian) {
        ans$hessian = matrix(ans$hessian, nr = length(par))
    } else {
        ans$hessian = NULL
    }
    
    # Return Value:
    ans
}


################################################################################

