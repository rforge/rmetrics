
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
# FUNCTION:                    CONSTRAINTS:
#  portfolioConstraints         Checks Consistency of Constraints Strings
# FUNCTION:                    INTERNAL USAGE ONLY:
#  .setConstraints              Transforms constraint strings into a list value
#  .setBoxGroupConstraints       Utility function called by .setConstraints()
#  .setRiskBudgetsConstraints    Utility function called by .setConstraints()
#  .getConstraints              Transforms a constraint list value into strings
#  .setRdonlp2Constraints       Adds Rdonlp2 conform constraints
################################################################################


portfolioConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly", ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Checks Consistency of Constraints Strings

    # Arguments:
    #   data - a portfolio data object
    #   spec - a portfolio specification object
    #   constraints - a constraints string
    
    # Example:
    #   Data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   Spec = portfolioSpec()
    #   Constraints = c("minW[1:6]=0.1", "maxW[1:6]=0.9", "minsumW[1:6]=0.1", "maxsumW[1:6]=0.9")
    #   portfolioConstraints(Data, Spec, Constraints)
    
    # FUNCTION:

    # Handle NULL:
    if (is.null(constraints)) constraints = "LongOnly"
    
    # Already done ...
    if (class(constraints) == "fPFOLIOCON") return(constraints)
    
    # Constraints is a string (vector) ...
    
    # Vector of Valid Strings:
    validStrings = c(
        "LongOnly", "Short",    # LongOnly and Short Notification
        "minW", "maxW",         # Box Constraints
        "sumW",                 # Two Sided Group Constraints
        "minsumW", "maxsumW",   # left and right Sided Group Constraints
        "minB", "maxB",         # Covariance Risk Budgets
        "rdonlp2")              # alt Rdonlp2 Constraints

    # Check Constraints Strings:
    usedStrings = unique(sort(sub("\\[.*", "", constraints)))
    checkStrings = usedStrings %in% validStrings
    check = (sum(!checkStrings) == 0)
    if (check) {
        check = "valid"
    } else {
        stop("Unvalid Constraints String(s)")
    }
    stringConstraints = constraints
    attr(stringConstraints, "control") = check

    # Set Box Constraints:
    boxConstraints = .setBoxConstraints(data, spec, constraints)
        
    # Set Group Equal Constraints:
    groupEqConstraints = .setGroupEqConstraints(data, spec, constraints)
        
    # Set Group Matrix Constraints:
    groupMatConstraints = .setGroupMatConstraints(data, spec, constraints)
            
    # Set BoxGroup Constraints:
    boxgroupConstraints =
        .setConstraints(data, spec, constraints, type = "BoxGroup")

    # Set RiskBudget Constraints:
    riskbudgetConstraints =
        .setConstraints(data, spec, constraints, type = "RiskBudget")
        
    # Alternative Constraints Settings:
    #   This allows to set directly constraints for the rdonlp2 solver.
    #   Note, arguments are passed through the dots argument.
    if (constraints[1] == "rdonlp2") {
        rdonlp2Constraints = .setRdonlp2Constraints(data, spec, ...)
    } else {
        rdonlp2Constraints = list()
    }

    # Return Value:
    new("fPFOLIOCON",
        stringConstraints = stringConstraints,
        boxConstraints = boxConstraints,
        groupEqConstraints = groupEqConstraints,
        groupMatConstraints = groupMatConstraints,
        boxgroupConstraints = boxgroupConstraints,
        riskbudgetConstraints = riskbudgetConstraints,
        altConstraints = rdonlp2Constraints)
}


# ------------------------------------------------------------------------------


.setConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly",
    type = c("BoxGroup", "RiskBudget"))
{
    # A function implemented by Rmetrics

    # Description:
    #   Transforms constraint strings into a list value

    # Arguments:
    #   data - a portfolio data object
    #   spec - a portfolio specification object
    #   constraints - a constraints string
    #   type - type of constraints

    # FUNCTION:

    # Check Arguments:
    type = match.arg(type)

    # Check Data:
    data = portfolioData(data, spec)

    # Short Selling:
    ## DW: I'm not sure if we need this
    ## if (any(constraints == "Short")) constraints = NULL

    # check if user want to use Risk Budget with wrong solver
    # currently only Rdonlp2 supports Risk Budget constraints
    if (length(grep("minB|maxB", constraints)) &&
        getSolver(spec) != "solveRdonlp2")
        warning("Risk budget used with wrong solver")

    # Constraints:
    if (type == "BoxGroup") {
            ans = .setBoxGroupConstraints(data, spec, constraints)
    } else if (type == "RiskBudget") {
            ans = .setRiskBudgetsConstraints(data, spec, constraints)
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.setBoxGroupConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Rmetrics

    # Description:
    #   Transforms constraint strings into a list value

    # Arguments:
    #   data - a portfolio data object
    #   spec - a portfolio specification object
    #   constraints - a constraints string
    #   type - type of constraints

    # FUNCTION:

    # Get Statistics:
    data = portfolioData(data, spec)

    # Get Specifications:
    mu = getMu(data)
    Sigma = getSigma(data)
    N = nAssets = getNAssets(data)
    nameAssets <- getNames(data)

    # Target Return:
    targetReturn = getTargetReturn(spec)
    weights = getWeights(spec)
    if(is.null(targetReturn) & is.null(weights)) {
        weights = rep(1/N, N)
        # warning("Equal Weights Portfolio in use")
    }
    if(is.null(targetReturn)) {
        targetReturn = (weights %*% Sigma %*% weights)[1, 1]
    }

    # Compose Matrix A:
    A = matrix(c(rep(1, times = N), mu), byrow = TRUE, ncol = N)
    A = rbind(A, diag(1, N), diag(-1, N))
    # colnames(A) = paste("A", 1:N, sep = "")
    colnames(A) <- nameAssets
    rownames(A) = c("Budget", "Return", paste("minW", 1:N, sep = ""),
        paste("maxW", 1:N, sep = ""))

    # Compose vector b0:
    minW = rep(0, N)
    maxW = rep(1, N)
    names(minW) <- names(maxW) <- nameAssets
    b0 = matrix(c(1, targetReturn, minW, -maxW), ncol = 1)
    colnames(b0) = "b0"
    if (!is.null(constraints)) {
        nC = length(constraints)
        what = substr(constraints, 1, 4)
        for (i in 1:nC) {
            if (what[i] == "minW" | what[i] == "maxW") {
                eval(parse(text = constraints[i]))
            }
        }
        b0 = matrix(c(1, targetReturn, minW, -maxW), ncol = 1)
        what = substr(constraints, 1, 7)
        for (i in 1:nC) {
            if (what[i] == "minsumW")  {
                minsumW = rep(0, times = N)
                names(minsumW) <- nameAssets
                eval(parse(text = constraints[i]))
                A = rbind(A, minsumW = sign(minsumW))
                b = strsplit(constraints[i], "=")[[1]][2]
                b0 = rbind(b0, as.numeric(b))
            }
        }
        for (i in 1:nC) {
            if (what[i] == "maxsumW")  {
                maxsumW = rep(0, times = N)
                names(maxsumW) <- nameAssets
                eval(parse(text = constraints[i]))
                A = rbind(A, maxsumW = -sign(maxsumW))
                b = strsplit(constraints[i], "=")[[1]][2]
                b0 = rbind(b0, -as.numeric(b))
            }
        }
    }
    rownames(b0) = rownames(A)

    # Bind Results:
    ans = cbind(A = A, b = b0)
    colnames(ans) = c(colnames(A), "Exposure")
    class(ans) = "matrix"

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.setBoxConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Rmetrics

    # Description:
    #   Transforms constraint strings into minW and maxW vectors

    # Arguments:
    #   data - a portfolio data object
    #   spec - a portfolio specification object
    #   constraints - a constraints string
    #   type - type of constraints

    # FUNCTION:

    # Get Statistics:
    data = portfolioData(data, spec)

    # Get Specifications:
    N = nAssets = getNAssets(data)
    nameAssets <- getNames(data)

    # Compose vectors avec and bvec:
    minW = rep(0, N)
    maxW = rep(1, N)
    names(minW) <- names(maxW) <- nameAssets
    if (!is.null(constraints)) {
        nC = length(constraints)
        what = substr(constraints, 1, 4)
        for (i in 1:nC) {
            if (what[i] == "minW" | what[i] == "maxW") {
                eval(parse(text = constraints[i]))
            }
        }
    }

    # Return Value:
    list(minW = minW, maxW = maxW)
}


# ------------------------------------------------------------------------------


.setGroupEqConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Rmetrics

    # Description:
    #   Transforms constraint strings into a list value

    # Arguments:
    #   data - a portfolio data object
    #   spec - a portfolio specification object
    #   constraints - a constraints string
    #   type - type of constraints

    # FUNCTION:

    # Get Statistics:
    data = portfolioData(data, spec)
    targetReturn = 
    if(is.null(getTargetReturn(spec))) targetReturn = NA
    else targetReturn = getTargetReturn(spec)
       
    # Get Specifications:
    mu = getMu(data)
    N = nAssets = getNAssets(data)
    nameAssets <- getNames(data)  
    
    # Target Return: 
    Aeq = matrix(mu, byrow = TRUE, ncol = N)
    # Full Investment:
    Aeq = rbind(Aeq, rep(1, N))
    # Dimension Names:
    colnames(Aeq) <- nameAssets
    rownames(Aeq) = c("Return", "Budget")
    ceq = c(Return = targetReturn, Budget = 1)
    
    # Return Value:
    list(Aeq = Aeq, ceq = ceq)
}


# ------------------------------------------------------------------------------
    
    
.setGroupMatConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Rmetrics

    # Description:
    #   Transforms constraint strings into a list value

    # Arguments:
    #   data - a portfolio data object
    #   spec - a portfolio specification object
    #   constraints - a constraints string
    #   type - type of constraints
    
    # Example:
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   spec = portfolioSpec()
    #   constraints = c("minsumW[3:5]=0.1", "maxsumW[1:3]=0.8", "sumW[4]=c(0.2,0.7)")
    

    # FUNCTION:

    # Get Statistics:
    data = portfolioData(data, spec)     
    
    # Get Specifications:
    N = nAssets = getNAssets(data)
    nameAssets <- getNames(data)          

    # Compose matrix Amat and vectors avec and bvec:
    Amat = matrix(rep(1, N), nrow = 1)
    avec = matrix(-Inf)
    bvec = matrix(Inf)
    what4 = substr(constraints, 1, 4)
    what7 = substr(constraints, 1, 7)
    count = 0
    if (!is.null(constraints)) {
        nC = length(constraints)
        for (i in 1:nC) {
            if (what7[i] == "minsumW")  {
                count = count + 1
                minsumW = rep(0, times = N)
                names(minsumW) <- nameAssets
                eval(parse(text = constraints[i]))
                Amat = rbind(Amat, minsumW = sign(minsumW))
                a = strsplit(constraints[i], "=")[[1]][2]
                avec = rbind(avec, as.numeric(a))
                bvec = rbind(bvec, +Inf)
            }
        }
        for (i in 1:nC) {
            if (what7[i] == "maxsumW")  {
                count = count + 1
                maxsumW = rep(0, times = N)
                names(maxsumW) <- nameAssets
                eval(parse(text = constraints[i]))
                Amat = rbind(Amat, maxsumW = sign(maxsumW))
                b = strsplit(constraints[i], "=")[[1]][2]
                avec = rbind(avec, -Inf)
                bvec = rbind(bvec, as.numeric(b))
            }
        }
        for (i in 1:nC) {
            if (what4[i] == "sumW")  {
                count = count + 1
                sumW = rep(0, times = N)
                names(sumW) <- nameAssets
                A = paste(strsplit(constraints[i], "=")[[1]][1], "=1")
                eval(parse(text = A))
                Amat = rbind(Amat, sumW = sign(sumW))
                b = paste("MinMax =", strsplit(constraints[i], "=")[[1]][2])
                eval(parse(text = b))
                avec = rbind(avec, MinMax[1])
                bvec = rbind(bvec, MinMax[2])
            }
        }
    }
    if (count > 0) {
        Amat = Amat[-1, ]
        avec = matrix(avec[-1, ])
        bvec = matrix(bvec[-1, ])
    }
    rownames(avec) <- rownames(Amat)
    rownames(bvec) <- rownames(Amat)
    colnames(avec) <- "lower"
    colnames(bvec) <- "upper"

    # Return Value:
    list(Amat = Amat, avec = avec, bvec = bvec)
}


# ------------------------------------------------------------------------------


.setRiskBudgetsConstraints <-
    function(data, spec = NULL, constraints = "LongOnly")
{
    # A function implemented by Rmetrics

    # Description:
    #   Transforms constraint strings into a list value

    # Arguments:

    # Example:
    #   Constraints = c("minB[3:4]=0.1","maxB[1:3]=0.3","maxB[c(4,6)]=0.4")
    #   setRiskBudgetsConstraints(8,  constraints = Constraints)

    # FUNCTION:

    # Create Data Object:
    data = portfolioData(data, spec)

    # Get Specifications:
    mu = getMu(data)
    Sigma = getSigma(data)
    N = nAssets = getNAssets(data)
    nameAssets <- getNames(data)

    # Compose Risk Budgets:
    minB = rep(0, N)
    maxB = rep(1, N)
    names(minB) <- names(maxB) <- nameAssets
    if (!is.null(constraints)) {
        nC = length(constraints)
        what = substr(constraints, 1, 4)
        for (i in 1:nC) {
            if (what[i] == "minB" | what[i] == "maxB") {
                eval(parse(text = constraints[i]))
            }
        }
    }
    ans = rbind(minB = minB, maxB = maxB)
    # colnames(ans) = paste("A", 1:N, sep = "")

    # Return Value:
    ans
}


################################################################################


.getConstraints <-
    function(object)
{
    # A function implemented by Rmetrics

    # Description:
    #   Transforms a constraint list value into strings

    # Arguments:
    #   object - the "constraintMatrix", a list with two named elements,
    #       the constrainded Matrix A and the constrained vector b, satisfying
    #       A * w >= b, where b is the exosure.

    # Value:
    #   A one column matrix with constraint strings.

    # FUNCTION:

    # Extract Matrix A and Vector b0:
    M = dim(object)[2]
    A = object[, -M]
    b0 = t(t(object[, M]))

    # Matrix Dimension:
    L = dim(A)[1]
    N = dim(A)[2]

    # Start with Box Constraints:
    const1 = paste("minW[", 1:N, "] = ", b0[3:(2+N),], sep = "")
    const2 = paste("maxW[", 1:N, "] = ", -b0[(3+N):(2+2*N),], sep = "")
    constraints = matrix(c(const1, const2), ncol = 1)

    # Add Sector Constraints:
    if((3+2*N) <= L) {
        for (i in (3+2*N):L) {
            index = paste ((1:N)[abs(A[i, ]) != 0], collapse = ",")
            addConstraintString = paste(rownames(A)[i], "[c(", index, ")] = ",
                abs(b0[i]), sep = "")
            constraints = rbind(constraints, addConstraintString)
        }
    }

    # Add Names:
    colnames(constraints) = "Constraints"
    rownames(constraints) = 1:(L-2)

    # Return Value:
    constraints
}


# ------------------------------------------------------------------------------


.setRdonlp2Constraints <- 
    function(data, spec, 
    par.lower = NULL, par.upper = NULL,
    A = NULL, lin.lower = NULL, lin.upper = NULL,
    nlin = list(), nlin.lower = NULL, nlin.upper = NULL)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds Rdonlp2 conform constraints
    
    # Arguments:
    #   Here we use the same arguments as in the original Rdonlp2
    #   package ...
   
    # Defaults for "LongOnly" 
    if (is.null(par.lower)) par.lower = rep(0, times = NCOL(data))
    if (is.null(par.upper)) par.upper = rep(1, times = NCOL(data))
    
    # Return Value:
    list(
        name = "solveRdonlp2",
        par.lower = par.lower, par.upper = par.upper,
        A = A, lin.lower = lin.lower, lin.upper = lin.upper,
        nlin = nlin, nlin.lower = nlin.lower, nlin.upper = nlin.upper)
}


################################################################################


.setRquadprogConstraints <-
    function(data, spec, constraints)
{
    # FUNCTION:
    
    # Setting the constraints matrix and vector:
    data = portfolioData(data, spec)
    if (class(constraints) == "fPFOLIOCON")
        constraints = constraints@stringConstraints
    tmpConstraints = .setConstraints(data, spec, constraints)
    nAssets = getNAssets(data)
    Sigma = getSigma(data)
    
    # Arguments:
    Dmat = Sigma
    dvec = rep(0, nAssets)
    Amat = t(tmpConstraints[, -(nAssets+1)])
    bvec = tmpConstraints[, (nAssets+1)]
    meq = 2
    
    # Return Value"
    list(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
}


# ------------------------------------------------------------------------------