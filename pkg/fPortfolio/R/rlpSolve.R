
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port:
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                    DESCRIPTION:
#  rlp                          Interface to CRAN's "lpSolve" solver
#  rlpSolve                     Interface to CRAN's lpSolve solver
#  .cvarSolver
#  .cdarSolver
################################################################################


rlp <-
function(
    direction = "min", 
    objective.in, 
    const.mat, 
    const.dir, 
    const.rhs, 
    transpose.constraints = TRUE, 
    int.vec, 
    presolve = 0, 
    compute.sens = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Call to function lp() from "lpSolve" package
    
    # FUNCTION:

    # Solve:   
    ans = lp(direction, objective.in, const.mat, const.dir, 
        const.rhs, transpose.constraints, int.vec, presolve, 
        compute.sens) 
        
    # Return Value:
    ans
}


################################################################################


rlpSolve <- 
    function(obj, A, b, Aeq = NULL, beq = NULL, lb = 0.0, ub = Inf,
    intvec = integer(0), control = list())
{
    # A function originally written by Kjell Konis
    
    # Description:
    
    # FUNCTION:
    
    # Solve:
    n <- length(obj)
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
    } else {
        stop("no constraints provided")
    }
    if(length(lb) == 1)
        lb <- rep(lb, n)
    if(length(lb) != n)
        stop("Number of lower bounds must match number of decision variables")
    if(length(ub) == 1)
        ub <- rep(ub, n)
    if(length(ub) != n)
        stop("Number of upper bounds must match number of decision variables")
    n.ints <- length(intvec)

    # Process control arguments 
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
        if(phase1 == "primal" && phase2 == "primal") simplex.type <- 5
        else if(phase1 == "primal" && phase2 == "dual") simplex.type <- 9
        else if(phase1 == "dual" && phase2 == "primal") simplex.type <- 6
        else if(phase1 == "dual" && phase2 == "dual") simplex.type <- 10
    }
    basis <- -1
    if(!is.null(control$basis)) basis <- c(0, control$basis)
    epslevel <- -1
    eps <- c(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0)
    if(!is.null(control$eps)) {
        if(is.character(control$eps)) {
            epslevel <- match(control$eps[1],
                c("tight", "medium", "loose", "baggy"), nomatch = 0) - 1
    } else {
        if(!is.na(control$eps["epsb"])) eps[1] <- control$eps["epsb"]
        if(!is.na(control$eps["epsd"])) eps[2] <- control$eps["epsd"]
        if(!is.na(control$eps["epsel"])) eps[3] <- control$eps["epsel"]
        if(!is.na(control$eps["epsint"])) eps[4] <- control$eps["epsint"]
        if(!is.na(control$eps["epsperturb"])) eps[5] <- control$eps["epsperturb"]
        if(!is.na(control$eps["epspivot"])) eps[6] <- control$eps["epspivot"]
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
        n = as.integer(n),
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
        x = double(n),
        status = integer(1),
        NAOK = TRUE,
        PACKAGE = "RlpSolve")
    if(lps$status == -2)
        message <- "out of memory"
    else
        message <- c(
            "optimal solution found",
            "the model is sub-optimal",
            "the model is infeasible",
            "the model is unbounded",
            "the model is degenerate",
            "numerical failure encountered",
            "process aborted",
            "timeout",
            "NOT USED",
            "the model was solved by presolve",
            "the branch and bound routine failed",
            "the branch and bound was stopped because of a break-at-first or break-at-value",
            "a feasible branch and bound solution was found",
            "no feasible branch and bound solution was found")[lps$status+1]
    control$eps <- lps$eps
    names(control$eps) <- 
        c("epsb", "epsd", "epsel", "epsint", "epsperturb", "epspivot")
    
    # Return Value:
    list(objective = lps$objective,
        x = lps$x,
        status = lps$status,
        message = message,
        control = control)
}


################################################################################
# Conditional Value at Risk solver
# Research Reference : Rockafellar & Uryasev (2000)
# Solver type: Linear
# Copyrights (C)
# Alexios Ghalanos 2008
# alexios at 4dscape.com
################################################################################


.cvarSolver <- 
    function(
        scenario, 
        forecasts, 
        targetReturn, 
        alpha, 
        group, 
        maxGroup, 
        minGroup, 
        maxAsset, 
        minAsset)
{
    # A function implemented by Alexios Ghalanos
    
    # Description:
    #   Conditional Value at Risk solver
    
    # FUNCTION:
    
    n <- dim(scenario)[2]
    s <- dim(scenario)[1]
    S <- as.matrix(scenario)
    colnames(S) <- NULL
    rownames(S) <- NULL
    group <- as.matrix(group)
    g <- dim(group)[1]
    colnames(group) <- NULL
    rownames(group) <- NULL
    maxg <- matrix(maxGroup, ncol = 1)
    ming <- matrix(minGroup, ncol = 1)
    minw <- as.numeric(minAsset)
    maxw <- as.numeric(maxAsset)
    forecasts <- as.numeric(forecasts)

    alpha <- as.numeric(alpha)
    
    gbGroup = rbind(group, -1*group)
    gbCons = rbind(maxg, -1*ming)
    
    objL <- c(1, rep(0, n), rep(1/((alpha)*s), s))
    
    Amat <- cbind(matrix(-1, nrow = s, ncol = 1), -S, -diag(s))
    Amat <- rbind(Amat, cbind(matrix(0, nrow = 2*g, ncol = 1), 
        gbGroup, matrix(0, nrow = 2*g, ncol = s)))
    
    bvec <- matrix(0, nrow = s, ncol = 1)
    bvec <- rbind(bvec, gbCons)
    
    
    lb <- rbind(-100, matrix(minw, ncol = 1))
    lb <- rbind(lb, matrix(0, ncol = 1, nrow = s))
    
    ub <- rbind(100, matrix(maxw, ncol = 1))
    ub <- rbind(ub, matrix(1, nrow = s, ncol = 1))
    
    Aeq <- cbind(0, matrix(1, ncol = n, nrow = 1), matrix(0, 
        ncol = s, nrow = 1))
    Aeq <- rbind(Aeq, cbind(0, matrix(forecasts, ncol = n, nrow = 1), 
        matrix(0, ncol = s, nrow = 1)))
    beq <- c(1, targetReturn)
    
    # Solve:
    ret <- rlpSolve(obj = objL,  A = Amat,  b = bvec,  Aeq = Aeq,  
        beq = beq,  lb = lb,  ub = ub) 
    solution <- list(weights = ret$x[2:(n+1)], objective = ret$objective, 
        status = ret$status, mess = ret$message, sol = ret)
        
    # Return Value:
    return(solution)
}


################################################################################
# Conditional DrawDown at Risk solver
# Research Reference : Chekhlov, Uryasev & Zabarankin (2000)
# Solver type: Linear
# Copyrights (C)
# Alexios Ghalanos 2008
# alexios at 4dscape.com
################################################################################


.cdarSolver <-
function(
    scenario, 
    forecasts, 
    targetReturn, 
    alpha, 
    group, 
    maxGroup, 
    minGroup, 
    maxAsset, 
    minAsset)
{
    # A function implemented by Alexios Ghalanos
    
    # Description:
    #   Conditional Drawdown-at-Risk solver
    
    # Arguments:
    #   Scenario - s x n matrix of returns representing the expected 
    #       realization of future path, or alternatively a historical 
    #       returns matrix
    #   forecasts - n x 1 vector of forecast returns
    #   targetReturn - 1 x 1 vector of portfolio target return
    #   alpha - 1 x 1 vector of alpha coefficient representing % of worst 
    #       drawdown to minimize over
    #   group - g x n matrix of group level constraints
    #   maxGroup - g x 1 vector of maximum group weight constraint
    #   minGroup - g x 1 vector of minimum group weight constraint
    #   maxAsset - n x 1 vector of maximum asset weights constraint
    #   minAsset - n x 1 vector of minimum asset weight constraint
    
    # Details:
    #   P.31 of Chekhlov, Uryasev & Zabarankin, 
    #   "DRAWDOWN MEASURE IN PORTFOLIO OPTIMIZATION"
    #   provides the linearization of the CDaR function 
    #   using appropriate auxiliary variables
    
    # Notes:
    #   custom lpsolve uses:
    #       min: objL * x
    #       subject to: A x <= b
    #       Aeq x == beq
    #       lb[j] <= x[j] <= ub[j] 
    
    # FUNCTION:

    n <- dim(scenario)[2]
    s <- dim(scenario)[1]
    S <-scenario
    colnames(S) <- NULL
    rownames(S) <- NULL
    
    group <- as.matrix(group)
    g <- dim(group)[1]
    colnames(group) <- NULL
    rownames(group) <- NULL
    maxg <- matrix(maxGroup,ncol=1)
    ming <- matrix(minGroup,ncol=1)
    
    minw <- as.numeric(minAsset)
    maxw <- as.numeric(maxAsset)
    
    forecasts<-as.numeric(forecasts)
    
    targetReturn <- as.numeric(targetReturn)
    
    alpha <- as.numeric(alpha)
    
    gbGroup <- rbind(group,-1*group)
    gbCons <- rbind(maxg,-1*ming)
    
    xm <- as.matrix(-diag(s))
    idx <- which(xm==(-1), arr.ind = TRUE)
    myrow <- idx[, 1]
    mycol <- idx[, 2]
    mycol[2:length(mycol)] <- mycol[2:length(mycol)]-1
    diag(xm[myrow[2:length(myrow)], mycol[2:length(mycol)]]) <- 1
    
    Amat = cbind(matrix(-1, nrow = s, ncol = 1),
        matrix(0,ncol = n, nrow = s), -diag(s), diag(s))
    Amat = rbind(Amat,cbind(matrix(0, nrow = s, ncol = 1), -S,
        matrix(0,ncol = s, nrow = s), as.matrix(xm)))
    
    Amat = rbind(Amat,cbind(matrix(0, nrow = 2*g,ncol = 1),
        gbGroup, matrix(0, nrow = 2*g, ncol = 2*s)))
        
    bvec = matrix(0,nrow=2*s,ncol=1)
    bvec = rbind(bvec,gbCons)
    
    objL = c(1, rep(0, n), rep(1/((1-alpha)*s), s), rep(0, s))
    
    lb = rbind(0,matrix(minw,ncol=1))
    lb = rbind(lb,matrix(0,ncol=1,nrow=2*s))
    
    ub = rbind(1, matrix(maxw, ncol = 1))
    ub = rbind(ub, matrix(100, nrow = 2*s, ncol = 1))
    ub[2+n+s] <- 0
    
    Aeq = cbind(0,matrix(1, ncol = n, nrow = 1),
        matrix(0, ncol = 2*s, nrow = 1))
    Aeq = rbind(Aeq, cbind(0, matrix(forecasts, ncol = n, nrow = 1),
        matrix(0, ncol = 2*s, nrow = 1)))
    beq = c(1, targetReturn)
    
    ret <- rlpSolve(obj = objL, A = Amat, b = bvec, Aeq = Aeq, beq = beq, 
        lb = lb, ub = ub) 
    
    solution <- list(weights=ret$x[2:(n+1)],objective = ret$objective,
        status = ret$status, mess = ret$message, sol = ret)
    
    # Return Value:
    return(solution)
}


################################################################################


if (FALSE) {
       
    # Dataset contains daily price data on 29 countries for the 
    # period 12/02/96-12/31/98 (m/d/y)
    indices.ts <- as.timeSeries(read.csv("indices.csv"))
    indices.r.ts <- returns(indices.ts, method = "discrete")
    nAssets <- NCOL(indices.r.ts)

    # Create some sample grouping
    group <- matrix(0, nrow = 3, ncol = nAssets)
    group[1,1:6] <- 1     #asia
    group[2,7:25] <- 1    #greater europe
    group[3,26:29] <- 1   #americas

    # Prepare Inputs and constraints
    scenario <- as.matrix(indices.r.ts)
    rownames(scenario) <- NULL
    scenario <- as.matrix(scenario[,])
    forecasts <- apply(scenario,2, FUN = function(x) mean(x))
    minAsset <- rep(-0.15,nAssets)
    maxAsset <- rep(0.15,nAssets)
    minGroup <- c(-0.9,-0.9,-0.9)
    maxGroup <- c(0.9,0.9,0.9)
    targetReturn <- 0.1/252
    alpha = 0.9
    lambda = 4
    threshold <- 0
    moment = 1
    ## V <- giniVmatrix(scenario, method = "fast")
    
    # Run Optimizers
    cvar.test <- .CVaR.solver(scenario, forecasts, targetReturn, alpha,
        group, maxGroup, minGroup, maxAsset, minAsset)
    cdar.test <- .CDaR.solver(scenario, forecasts, targetReturn, alpha,
        group, maxGroup, minGroup, maxAsset, minAsset)
    mad.test <- .MAD.solver(scenario,as.numeric(forecasts),targetReturn,
        group, maxGroup, minGroup, maxAsset, minAsset)
    minimax.test <- .MiniMax.solver(scenario, forecasts, targetReturn,
        group, maxGroup, minGroup, maxAsset, minAsset)
    hmu.test <- .HMU.solver(scenario,forecasts,lambda,targetReturn,
        group, maxGroup, minGroup, maxAsset, minAsset, w0 = NULL)
    lpm.test <- .LPM.solver(scenario, forecasts, threshold, moment, 
        targetReturn, group, maxGroup, minGroup, maxAsset, minAsset)
    gini.test <- .MG.solver(V,forecasts, targetReturn,
        group, maxGroup, minGroup, minAsset, maxAsset)

}


################################################################################
    
    