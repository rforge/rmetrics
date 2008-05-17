
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
#  solveRlp                     Calls linear programming solver
#  solveRlpSolve                Calls linear programming solver
################################################################################


solveRlp <-
    function(data, spec, constraints)
{
    # A function implemented by Rmetrics

    # Description:
    #   Linear Solver from R package lpSolve for Mean-CVaR Problems

    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # Value:
    #   direction - optimization direction, as entered
    #   x.count - number of variables in objective function
    #   objective - vector of objective function coefficients, as entered
    #   const.count - number of constraints entered
    #   constraints - constraint matrix, as entered (not returned by 
    #       lp.assign or lp.transport)
    #   int.count - number of integer variables
    #   int.vec - vector of integer variables' indices, as entered
    #   objval - value of objective function at optimum}
    #   solution - vector of optimal coefficients
    #   status - numeric indicator: 0 = success, 2 = no feasible solution

    # Note:
    #   This function requires to load the contributed R package
    #   lpSolve explicitely!

    # Example:
    #   Data = 100*as.timeSeries(data(LPP2005REC))[, 1:6]
    #   Spec = portfolioSpec();setType(Spec)="CVaR"; setSolver(Spec)="solveRlp"
    #   Constraints = portfolioConstraints(Data, Spec)
    #   tangencyPortfolio(Data, Spec)
    
    # FUNCTION:

    # Load lpSolve from CRAN Server:
    if (!require(lpSolve)) {
        cat("\n\nlpSolve Package missing")
        cat("\nPlease install Package 'lpSolve' from CRAN Server\n")
    }
    
    # Transform Data and Constraints:
    data = portfolioData(data, spec)
    if (class(constraints) == "fPFOLIOCON")
        constraints = constraints@stringConstraints

    # Trace:
    trace = getTrace(spec)
    if(trace) cat("\nPortfolio Optimiziation:\n Using RlpSolve ...\n\n")

    # Get Specifications:
    mu = getMu(data)
    Sigma = getSigma(data)
    nAssets = getNAssets(data)

    # Extracting data from spec:
    targetReturn = getTargetReturn(spec)
    stopifnot(is.numeric(targetReturn))

    # Get quantile measure alpha:
    targetAlpha = getAlpha(spec)

    # Scenarios:
    Data = getSeries(data)
    colNames = colnames(Data)
    rowNames = rownames(Data)
    assets = dim(Data)
    m = assets[1]
    w = assets[2]

    if (nAssets == 2) {

        ### # Two Assets Portfolio:
        ### # YC: test might failed because of numerical errors, hence 'round'
        ### stopifnot(round(targetReturn, 6) >= round(min(mu), 6))
        ### stopifnot(round(targetReturn, 6) <= round(max(mu), 6))

        stopifnot(targetReturn >= min(mu))
        stopifnot(targetReturn <= max(mu))

        names(targetReturn) <- spec@model$estimator[1]
        weights = (targetReturn-mu[2]) / (mu[1]-mu[2])
        weights = c(weights, 1- weights)
        CVaR = -.cvarRisk(Data, weights, targetAlpha)
        ans = list(
            weights = weights,
            VaR = .varRisk(Data, weights, targetAlpha),
            solution = .varRisk(Data, weights, targetAlpha),
            CVaR = CVaR,
            objval = .cvarRisk(Data, weights, targetAlpha),
            ierr = 0,
            status = 0,
            solver = "twoAssetsCVaR",
            targetAlpha = targetAlpha,
            objective = CVaR)
    } else {
        # Compose objective function:
        Names = c("VaR", paste("e", 1:m, sep = ""), colNames)
        f.obj = c(-1, rep(-1/(targetAlpha*m), m), rep(0, w))
        names(f.obj) = Names

        # Info on constraints - Constraint matrix:
        #   Example m=8 Data Records, and w=4 Assets
        #
        #   VaR  es            weights          exposure
        #   x1   x2  ...  x9   x10 ... x13
        #
        #    0    0       0    mu1     mu4      >= Mu
        #    0    0       0    1       1        == 1
        #
        #   -1    1       0    r1.1    r4.1     >= 0
        #   -1    0  1    0    r1.2    r4.2     >= 0
        #
        #   -1    0    1  0    r1.8    r4.8     >= 0
        #   -1    0       1    r1.9    r4.9     >= 0
        #
        #   x2   >= 0    ...   x9   >= 0
        #   x10  >= 0    ...   x13  >= 0

        # Compose Constraint Matrix:
        nX = 1 + m + w
        nY = 2 + m
        f.con = matrix(rep(0, nX*nY), ncol = nX)
        rownames(f.con) = c("Budget", "Return", rowNames)
        colnames(f.con) = c("VaR", paste("e", 1:m, sep = ""), colNames)
        f.con[1, (2+m):(2+m+w-1)] = as.numeric(mu)
        f.con[2, (2+m):(2+m+w-1)] = 1
        f.con[3:(m+2), 1] = 1
        f.con[3:(m+2), 2:(m+1)] = diag(m)
        f.con[3:(m+2), (2+m):(2+m+w-1)] = series(Data)

        # Box and Group Constraints:
        tmpConstraints = .setConstraints(data, spec, constraints)
        nConstraints = dim(tmpConstraints)
        append = cbind(matrix(0, ncol = 1+m, nrow = (nConstraints[1]-2)),
            tmpConstraints[3:nConstraints[1], 1:(nConstraints[2]-1)])
        f.con = rbind(f.con, append)

        # Set Directions:
        f.dir = c("==", "==", rep(">=", m))
        f.dir = c(f.dir, rep(">=", (nConstraints[1]-2)))
        names(f.dir) = rownames(f.con)

        # Compose Right Hand Side Vector:
        f.rhs = c(targetReturn, 1, rep(0, m))
        f.rhs = c(f.rhs, tmpConstraints[3:(nConstraints[1]), nConstraints[2]])
        names(f.rhs) = rownames(f.con)

        # Optimize Portfolio:
        ans = lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs)
        class(ans) <- "list"

        # Prepare Output List:
        ans$weights = ans$solution[(m+2):(m+1+w)]
        for(i in 1:w) {
            if(abs(ans$weights[i]) < sqrt(.Machine$double.eps))
                ans$weights[i] = 0
        }
        attr(ans$weights, "error") <- ans$ierr
        ans$weights[as.logical(ans$ierr)] = NA
        ans$VaR = ans$solution[1]
        ans$CVaR = -ans$objval
        ans$ierr = ans$status
        ans$solver = "lpSolve"
        ans$targetAlpha = targetAlpha
        ans$objective = -ans$objval
    }

    # Return Value:
    ans
}


################################################################################


solveRlpSolve <-
    function(data, spec, constraints)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Linear Solver from R package RlpSolve for Scenario Optimization

    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # Value:

    # Note:
    #   This function requires to load the R package RlpSolve explicitely!

    # FUNCTION:
    
    # Load RlpSolve:
    if (!require(ClpSolve)) {
        cat("\n\nRlpSolve Package missing")
        cat("\nPlease install Package 'RlpSolve' from Rmetrics Server\n")
    }
     
    # Transform Data and Constraints to S4 Objects:
    data = portfolioData(data, spec)
    constraints = portfolioConstraints(data, spec, constraints)

    # Get Portfolio Type and Solver:
    type = getType(spec)
    FUN = paste(".", tolower(type), "Solver", sep = "")
    funSolver = match.fun(FUN)

    # Solve:
    ans = funSolver(data, spec, constraints)
    
    # Return Value:
    ans
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
    function(data, spec, contsraints)
    # Modified by DW from:
    #   function(scenario, forecasts, targetReturn, alpha, group, 
    #   maxGroup, minGroup, maxAsset, minAsset)
{
    # A function implemented by Alexios Ghalanos and adapted by Diethelm Wuertz
    
    # Description:
    #   Conditional Value-at-Risk solver
    
    # Arguments:
    
    # Previous Function Arguments:
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
    
    # Value:
    #   ans = list()
    #   What is returned from the solver, a list named
    #       ans$optim
    #   What we also definitely need: 
    #       ans$solver 
    #       ans$weights 
    #       ans$objective
    #       ans$status
    #       ans$message
    #       ans$targetReturn 
    #       ans$targetRisk  
    
    # Details:
    #   
    
    # Notes:
    #   Custom lpSolve uses:
    #       min: objL * x
    #       subject to: A x <= b
    #       Aeq x == beq
    #       lb[j] <= x[j] <= ub[j] 
    
    # FUNCTION:
    
    # Wrapper - added by DW:
    scenario <- as.matrix(getSeries(data))
    forecasts <- colMeans(scenario)
    targetReturn <- getTargetReturn(spec)
    alpha <- getAlpha(spec)
    group <- NA
    maxGroup <- NA
    minGroup <- NA
    maxAsset <- NA
    minAsset <- NA
    
    # Settings - what follows from AG:
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
    
    # Solve - use lpSolve from KK:
    optim <- rlpSolve(obj = objL,  A = Amat,  b = bvec,  
        Aeq = Aeq, beq = beq,  lb = lb,  ub = ub) 
        
    # Result:
    ans <- list(
        solver = "RlpSolve",
        optim = optim,
        weights = .checkWeights(optim$x[2:(n+1)]), 
        targetReturn = NA,
        targetRisk = NA,
        objective = optim$objective, 
        status = optim$status, 
        message = optim$message)          
        
    # Return Value:
    ans
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
    function(data, spec, contsraints)
    # function(scenario, forecasts, targetReturn, alpha, group, 
    # maxGroup, minGroup, maxAsset, minAsset)
{
    # A function implemented by Alexios Ghalanos adapted by DW
    
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
    
    # Value:
    #   ans = list()
    #   What is returned from the solver, a list named
    #       ans$optim
    #   What we also definitely need: 
    #       ans$solver 
    #       ans$weights 
    #       ans$objective
    #       ans$status
    #       ans$message
    #       ans$targetReturn 
    #       ans$targetRisk  
    
    # Details:
    #   P.31 of Chekhlov, Uryasev & Zabarankin, 
    #   "DRAWDOWN MEASURE IN PORTFOLIO OPTIMIZATION"
    #   provides the linearization of the CDaR function 
    #   using appropriate auxiliary variables
    
    # Notes:
    #   Custom lpSolve uses:
    #       min: objL * x
    #       subject to: A x <= b
    #       Aeq x == beq
    #       lb[j] <= x[j] <= ub[j] 
    
    # FUNCTION:
    
    # Wrapper:
    data = portfolioData(data)
    scenario = as.matrix(getData(data))
    forecasts = getMu(data)
    targetReturn = getTargetReturn(spec)
    alpha = getAlpha(spec)
    group = NA
    maxGroup = NA
    minGroup = NA
    # boxConstrains = .setBoxConstraints(data, constraints)
    maxAsset = NA # boxConstraints$minW
    minAsset = NA # boxConstraints$maxW
    
    # Settings:
    n <- dim(scenario)[2]
    s <- dim(scenario)[1]
    S <- scenario
    colnames(S) <- NULL
    rownames(S) <- NULL
    group <- as.matrix(group)
    g <- dim(group)[1]
    colnames(group) <- NULL
    rownames(group) <- NULL
    maxg <- matrix(maxGroup,ncol = 1)
    ming <- matrix(minGroup,ncol = 1)   
    minw <- as.numeric(minAsset)
    maxw <- as.numeric(maxAsset)
    forecasts<-as.numeric(forecasts)
    targetReturn <- as.numeric(targetReturn)
    alpha <- as.numeric(alpha)
    gbGroup <- rbind(group, -1*group)
    gbCons <- rbind(maxg, -1*ming)
    xm <- as.matrix(-diag(s))
    idx <- which(xm == (-1), arr.ind = TRUE)
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
    bvec = matrix(0,nrow = 2*s, ncol = 1)
    bvec = rbind(bvec, gbCons)
    objL = c(1, rep(0, n), rep(1/((1-alpha)*s), s), rep(0, s))
    lb = rbind(0,matrix(minw, ncol = 1))
    lb = rbind(lb,matrix(0, ncol = 1, nrow = 2*s))
    ub = rbind(1, matrix(maxw, ncol = 1))
    ub = rbind(ub, matrix(100, nrow = 2*s, ncol = 1))
    ub[2+n+s] <- 0
    Aeq = cbind(0,matrix(1, ncol = n, nrow = 1),
        matrix(0, ncol = 2*s, nrow = 1))
    Aeq = rbind(Aeq, cbind(0, matrix(forecasts, ncol = n, nrow = 1),
        matrix(0, ncol = 2*s, nrow = 1)))
    beq = c(1, targetReturn)
    
    # Solve:
    optim <- rlpSolve(obj = objL, A = Amat, b = bvec, Aeq = Aeq, 
        beq = beq, lb = lb, ub = ub) 
    
    # Result:
    ans <- list(
        weights=optim$x[2:(n+1)],
        objective = optim$objective,
        status = optim$status, 
        message = optim$message, 
        sol = optim)
    
    # Return Value:
    ans
}


################################################################################


if (FALSE) {
       
    
    
    Data <- returns(as.timeSeries(read.csv("indices.csv")), method = "discrete")
    colnames(Data) <- abbreviate(colnames(Data))
    
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    #setSolver(Spec) =
    
    Constraints = c(
        "sumW[1:6]   = c(-0.9, 0.9)", # asia
        "sumW[7:25]  = c(-0.9, 0.9)", # greater europe
        "sumW[26:29] = c(-0.9, 0.9)") # americas
        
    Groups = .setGroupMatConstraints(Data, Spec, Constraints)
    group = Groups$Amat
    minGroup = Groups$avec
    maxGroup = Groups$bvec
    
    Box = .setBoxConstraints(Data, Spec, Constraints)
    minAssets = Box$minW
    maxAssets = Box$MaxW
    
    # Dataset contains daily price data on 29 countries for the 
    # period 12/02/96-12/31/98 (m/d/y)
    Data <- returns(as.timeSeries(read.csv("indices.csv")), method = "discrete")
    colnames(Data) <- abbreviate(colnames(Data)) 
    nAssets <- NCOL(Data)

    # Create some sample grouping
    group <- matrix(0, nrow = 3, ncol = nAssets)
    group[1, 1:6] <- 1     # asia
    group[2, 7:25] <- 1    # greater europe
    group[3, 26:29] <- 1   # americas

    # Prepare Inputs and constraints
    scenario <- as.matrix(Data)
    rownames(scenario) <- NULL
    scenario <- as.matrix(scenario[,])
    forecasts <- apply(scenario,2, FUN = function(x) mean(x))
    minAsset <- rep(-0.15, nAssets)
    maxAsset <- rep(0.15, nAssets)
    minGroup <- c(-0.9, -0.9, -0.9)
    maxGroup <- c(0.9, 0.9, 0.9)
    targetReturn <- 0.1/252
    alpha = 0.9
    lambda = 4
    threshold <- 0
    moment = 1
    ## V <- giniVmatrix(scenario, method = "fast")
    
    # Run Optimizers
    cvar.test <- .cvarSolver(scenario, forecasts, targetReturn, alpha,
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

