

################################################################################


lpSolveAPI <- 
function(obj, A, b, Aeq = NULL, beq = NULL, lb = 0.0, ub = Inf,
    intvec = integer(0), control = list())
{
    p <- length(obj)

    if(!is.null(A) && !is.null(Aeq)) {
        dimnames(A) <- dimnames(Aeq) <- names(b) <- names(beq) <- NULL
        ldAeq <- dim(A)[1] + 1
        A <- rbind(A, Aeq)
        ldA <- dim(A)[1]
        b <- c(0.0, b, beq)
    } else if(is.null(A) && !is.null(Aeq)) {
        dimnames(Aeq) <- names(b) <- NULL
        ldAeq <- 1
        A <- Aeq
        ldA <- dim(A)[1]
        b <- c(0.0, beq)
    } else if(!is.null(A) && is.null(Aeq)) {
        dimnames(A) <- names(b) <- NULL
        ldAeq <- dim(A)[1] + 1
        ldA <- dim(A)[1]
        b <- c(0.0, b)
    } else
        stop("no constraints provided")

    if(length(lb) == 1)
        lb <- rep(lb, p)

    if(length(lb) != p)
        stop("number of lower bounds must match number of decision variables")

    if(length(ub) == 1)
        ub <- rep(ub, p)

    if(length(ub) != p)
        stop("number of upper bounds must match number of decision variables")

    n.ints <- length(intvec)

    ## Process control arguments ##

    pivoting.rule <- -1
    if(!is.null(control$pivoting)) {
    
        pricer <- match(control$pivoting[1], 
            c("firstindex", "dantzig", "devex", "steepestedge")) - 1

        if(length(control$pivoting[-1])) {
            price <- match(control$pivoting[-1], 
                c("primalfallback", "multiple", "partial", "adaptive",
                "NOTUSED", "randomized", "NOTUSED", "autopartial", 
                "loopleft", "loopalternate",
                "harristwopass", "NOTUSED", "truenorminit"))
            price <- 2^(price + 1)
        } else {
            price <- numeric(0)
        }
        pivoting.rule <- sum(c(pricer, price))
    }

    simplex.type <- -1
    if(!is.null(control$simplex.type)) {
    
        phase1 <- control$simplex.type[1]
        phase2 <- control$simplex.type[2]

        if(phase1 == "primal" && phase2 == "primal")
            simplex.type <- 5

        else if(phase1 == "primal" && phase2 == "dual")
            simplex.type <- 9

        else if(phase1 == "dual" && phase2 == "primal")
            simplex.type <- 6

        else if(phase1 == "dual" && phase2 == "dual")
            simplex.type <- 10
    }

    basis <- -1
    if(!is.null(control$basis))
        basis <- c(0, control$basis)

    epslevel <- -1
    eps <- c(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0)
    
    if(!is.null(control$eps)) {
        if(is.character(control$eps)) {
            epslevel <- match(control$eps[1],
                c("tight", "medium", "loose", "baggy"),
                nomatch = 0) - 1
        } else {
          if(!is.na(control$eps["epsb"]))
            eps[1] <- control$eps["epsb"]
    
          if(!is.na(control$eps["epsd"]))
            eps[2] <- control$eps["epsd"]
    
          if(!is.na(control$eps["epsel"]))
            eps[3] <- control$eps["epsel"]
    
          if(!is.na(control$eps["epsint"]))
            eps[4] <- control$eps["epsint"]
    
          if(!is.na(control$eps["epsperturb"]))
            eps[5] <- control$eps["epsperturb"]
    
          if(!is.na(control$eps["epspivot"]))
            eps[6] <- control$eps["epspivot"]
        }
    }
  
    presolve <- -1
    if(!is.null(control$presolve)) {
        presolve <- unique(control$presolve)
        presolve.choices <- c(none = 0, rows = 1, cols = 2, lindep = 4,
        sos = 32, reducemip = 64, knapsack = 128, elimeq2 = 256,
        impliedfree = 512, reducegcd = 1024, probefix = 2048,
        probereduce = 4096, rowdominate = 8192, coldominate = 16384,
        mergerows = 32768, impliedslk = 65536, colfixdual = 131072,
        bounds = 262144, duals = 524288, sensduals = 1048576)
        presolve <- presolve.choices[presolve]
        presolve <- sum(presolve)
    }

    storage.mode(A) <- "double"

    lps <- .C("linprog",
        obj = as.double(obj),
        A = A,
        ldA = as.integer(ldA),
        p = as.integer(p),
        ldAeq = as.integer(ldAeq),
        b = as.double(b),
        lb = as.double(lb),
        ub = as.double(ub),
        n.ints = as.integer(n.ints),
        intvec = as.integer(intvec),
        pivoting.rule = as.integer(pivoting.rule),
        simplex.type = as.integer(simplex.type),
        basis = as.integer(basis),
        epslevel = as.integer(epslevel),
        eps = as.double(eps),
        presolve = as.integer(presolve),
        objective = double(1),
        x = double(p),
        status = integer(1),
        NAOK = TRUE,
        PACKAGE = "RlpSolveAPI")

    if(lps$status == -2) {
        status.message = "out of memory"
    } else {
        message = c(
            "optimal solution found",
            "the model is sub-optimal",
            "the model is infeasible",
            "the model is unbounded",
            "the model is degenerate",
            "numerical failure encountered",
            "process aborted",
            "timeout",
            "NOT USED",
            "model was solved by presolve",
            "branch and bound routine failed",
            "branch and bound stopped because of a break-at-first or break-at-value",
            "a feasible branch and bound solution was found",
            "no feasible branch and bound solution was found")[lps$status+1]
    }       

    control$eps <- lps$eps
    names(control$eps) <- 
        c("epsb", "epsd", "epsel", "epsint", "epsperturb", "epspivot")

    list(
        objective = lps$objective,
        x = lps$x,
        status = lps$status,
        message = message,
        control = control)
}


################################################################################

