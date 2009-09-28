

################################################################################


donlp2 <- 
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
    
    control = donlp2Control(),
    control.fun = function(lst){return(TRUE)},
    env = .GlobalEnv, 
    name = NULL)
{
    # use analytical gradients?
    if (is.function(attr(fn, "gr")) &
        # DW:
        # all(lapply(nlin, function(e) is.function(attr(e,"gr"))))){
        all(unlist(lapply(nlin, function(e)is.function(attr(e,"gr")))))) {
        control["analyt"] = TRUE
    } else {
        control["analyt"] = FALSE
    }
  
    # check parameter and its box constraints
    if (length(par)!=length(par.upper) | length(par)!=length(par.lower) ){
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
        if (length(lin.upper)!=num.lin | length(lin.lower)!=num.lin)
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

    # the wrapper for objective and constraint function 
    # (called from eval_extern())
    # mode == 0: EVAL_FN(evaluate objective and constraint function)
    # mode == 1: EVAL_GR(evaluate gr of objective and constraint function)

    # fun.id == 0: evaluate objective function 'fn'
    # fun.id >= 1: evaluate constraint function 'nlin[[fun.id]]'
    #
    confun <- function(arg){
        mode = arg[1]; fun.id = arg[2]; p = arg[c(-1,-2)]
        if (mode == 0){      # evaluate function values
            if (fun.id == 0){
                return(as.double(eval(fn(p), env)))
            }
            return(as.double(eval(nlin[[fun.id]](p), env)))
        } else if (mode == 1){ # evaluate gradient values
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
        fsilent <- TRUE; name="dummy"
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
            confun, 
            environment(confun), 
            PACKAGE = "Rdonlp2"),
        # ensure to free memory and close .mes .pro files if opened
        finally=.Call("teardown", 0, PACKAGE = "Rdonlp2"))
            
    ans$nr.update <- matrix(ans$nr.update, nr=length(par))
    if (control$hessian)
        ans$hessian = matrix(ans$hessian, nr=length(par))
    else
        ans$hessian = NULL
    ans
}


################################################################################