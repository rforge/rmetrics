
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
# FUNCTION:                  DESCRIPTION:
#  solveRlpSolveAPI           Calls linear programming solver
#  solveRlp                   Synonyme function call
################################################################################


solveRlpSolveAPI <-
    function(data, spec, constraints)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Linear Solver from R package RlpSolveAPI for Scenario Optimization

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


# ------------------------------------------------------------------------------
# Now on CRAN ..


solveRlp <- solveRlpSolveAPI


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
    #           min: objL * x
    #       subject to: 
    #           Aeq x == beq
    #           A x   <= b
    #           lb[j] <= x[j] <= ub[j] 
    
    # FUNCTION:
    
    # Wrapper - added by DW:
    scenario <- as.matrix(getSeries(data))
    forecasts <- colMeans(scenario)
    targetReturn <- getTargetReturn(spec)
    alpha <- getAlpha(spec)
    
    # forecasts <- as.numeric(forecasts)
    # alpha <- as.numeric(alpha)
    
    group <- NA
    maxGroup <- NA
    minGroup <- NA
    
    asset = NA
    maxAsset <- NA
    minAsset <- NA
    
    # Settings - what follows from AG:
    n <- dim(scenario)[2]
    s <- dim(scenario)[1]
    S <- as.matrix(scenario)
    colnames(S) <- NULL
    rownames(S) <- NULL
    
    if (!is.na(group)) {
        group <- as.matrix(group)
        g <- dim(group)[1]
        colnames(group) <- NULL
        rownames(group) <- NULL
        maxg <- matrix(maxGroup, ncol = 1)
        ming <- matrix(minGroup, ncol = 1)
        gbGroup = rbind(group, -1*group)
        gbCons = rbind(maxg, -1*ming)
    }
    
    if (!is.na(asset)) {
        minw <- as.numeric(minAsset)
        maxw <- as.numeric(maxAsset)
    }
    
    # Objective Function:
    objL <- c(1, rep(0, n), rep(1/((alpha)*s), s))
    
    # "<="
    Amat <- cbind(matrix(-1, nrow = s, ncol = 1), -S, -diag(s))
    if (!is.na(group)) Amat <- 
        rbind(Amat, cbind(matrix(0, nrow = 2*g, ncol = 1), 
        gbGroup, matrix(0, nrow = 2*g, ncol = s)))
    bvec <- matrix(0, nrow = s, ncol = 1)
    if (!is.na(group)) bvec <- rbind(bvec, gbCons)
    
    # Lower Box:
    lb <- rbind(-100, matrix(minw, ncol = 1))
    lb <- rbind(lb, matrix(0, ncol = 1, nrow = s)) 
    
    # Upper Box:
    ub <- rbind(100, matrix(maxw, ncol = 1))
    ub <- rbind(ub, matrix(1, nrow = s, ncol = 1)) 
     
    # "=="
    Aeq <- 
        cbind(0, matrix(1, ncol = n, nrow = 1), 
        matrix(0, ncol = s, nrow = 1))
    Aeq <- 
        rbind(Aeq, 
        cbind(0, matrix(forecasts, ncol = n, nrow = 1), 
        matrix(0, ncol = s, nrow = 1))) 
    beq <- c(1, targetReturn)
    
    # Solve - use lpSolve from KK:
    optim <- rlp(obj = objL,  A = Amat,  b = bvec,  
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
    optim <- rlp(obj = objL, A = Amat, b = bvec, Aeq = Aeq, 
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

