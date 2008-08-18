
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


################################################################################
# FUNCTION:                     DESCRIPTION:
#  portfolioConstraints          Returns an object of class fPFOLIOCON
# FUNCTION:                     DESCRIPTION:
#  setBoxConstraints             Returns a list with box constraints
#  setGroupEqConstraints         Returns a list with group equal constraints
#  setGroupMatConstraints        Returns a list with group matrix constraints
#  setRiskBudgetConstraints      Returns a list with risk-budget constraints
#  setBoxGroupConstraints        Returns a matrix of box/group constraints
################################################################################


# DW: Thes functions are made to convert constraint strings into matrixes
# and vectors to simplify the input for the many different portfolio 
# solvers.


if(FALSE) {
portfolioConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly", ...)
{
    # Description:
    #   Returns an object of class fPFOLIOCON

    # Arguments:
    #   data - a timeSeries or a fPFOLIODATA object
    #   spec - a fPFOLIOSPEC object
    #   constraints - a constraints string
    #       validStrings = c(
    #           "LongOnly", "Short",    # LongOnly and Short Notification
    #           "minW", "maxW",         # Box Constraints
    #           "minsumW", "maxsumW",   # left and right Sided Group Constraints
    #           "minB", "maxB")         # Covariance Risk Budgets
    
    # Details:
    #   This function takes the constraints strings and converts them to
    #   constraints vectors and matrices of the following form:
    #   1. boxConstraints           W_min <= W <= W_max
    #   2. groupEqConstraints       A_eq W = c_eq
    #   3. groupMatConstraints      a_vec <= A_mat W <= b_vec
    #   4. riskBudgetConstraints    a <= RisBudget <= b
    #   These values are returned as list in four slots.
    
    # Example:
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]; spec = portfolioSpec() 
    #   portfolioConstraints(data, spec, "LongOnly")
    #   constraints = c("minW[1:3]=0.1", "maxW[4:6]=0.9", "minsumW[c(2,5)]=0.2", "maxsumW[c(1,4)]=0.9")
    #   portfolioConstraints(data, spec, constraints)
    
    # FUNCTION:

    # Already done ...
    if (class(constraints) == "fPFOLIOCON") 
        constraints = constraints@stringConstraints
    
    # Handle NULL - A NULL  :
    if (is.null(constraints)) 
        constraints = "LongOnly"
    
    # Chweck Vector of Valid Strings - these are strings ...
    validStrings = c(
        "LongOnly", "Short",    # LongOnly and Short Notification
        "minW", "maxW",         # Box Constraints
        "minsumW", "maxsumW",   # left and right Sided Group Constraints
        "minB", "maxB")         # Covariance Risk Budgets
    if (any(constraints == "Short")) setSolver(spec) = "solveRshortExact"
    usedStrings = unique(sort(sub("\\[.*", "", constraints)))
    checkStrings = usedStrings %in% validStrings
    check = (sum(!checkStrings) == 0)
    if (check) check = "valid" else stop("Invalid Constraints String(s)")
    stringConstraints = constraints
    attr(stringConstraints, "control") = check

    # Set Box Constraints:
    boxConstraints <-
        setBoxConstraints(data, spec, constraints)
        
    # Set Group Equal Constraints:
    groupEqConstraints <- 
        setGroupEqConstraints(data, spec, constraints)
        
    # Set Group Matrix Constraints:
    groupMatConstraints <- 
        setGroupMatConstraints(data, spec, constraints)
          
    # Set RiskBudget Constraints:
    riskBudgetConstraints <- 
        setRiskBudgetConstraints(data, spec, constraints)
        
    # Set Box/Group Constraints:
    boxGroupConstraints <- 
        setBoxGroupConstraints(data, spec, constraints)

    # Return Value:
    new("fPFOLIOCON",
        stringConstraints = stringConstraints,
        boxConstraints = boxConstraints,
        groupEqConstraints = groupEqConstraints,
        groupMatConstraints = groupMatConstraints,
        riskBudgetConstraints = riskBudgetConstraints,
        boxGroupConstraints = boxGroupConstraints)
}
}

################################################################################


setBoxConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # Description:
    #   Returns a list with box constraints vectors
    
    # Details:
    #   Takes care of "minW" and "maxW" strings
    #   W_min <= W <= W_max

    # Arguments:
    #   data - a timeSeries or a fPFOLIODATA object
    #   spec - a fPFOLIOSPEC object
    #   constraints - a constraints string
    
    # Example:
    #   setBoxConstraints(as.timeSeries(data(LPP2005REC))[,1:6])
    #   $minW
    #   SBI SPI SII LMI MPI ALT 
    #     0   0   0   0   0   0 
    #   $maxW
    #   SBI SPI SII LMI MPI ALT 
    #     1   1   1   1   1   1 

    # FUNCTION:

    # Get Statistics:
    data = portfolioData(data, spec)

    # Get Specifications:
    N = nAssets = getNAssets(data)
    Assets <- getNames(data)
    
    # Consider LongOnly:
    if("LongOnly" %in% constraints) {
        minW = rep(0, nAssets)
        maxW = rep(1, nAssets)
        names(minW) = names(maxW) = Assets
        return(list(minW = minW, maxW = maxW))
    }
    
    # Consider Unlimited Short:
    if("Short" %in% constraints) {
        minW = rep(-Inf, nAssets)
        maxW = rep( Inf, nAssets)
        names(minW) = names(maxW) = Assets
        return(list(minW = minW, maxW = maxW))
    }

    # Extract and Compose Vectors a_vec and b_vec:
    minW = rep(0, nAssets)
    maxW = rep(1, nAssets)
    names(minW) <- names(maxW) <- Assets
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


setGroupEqConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # Description:
    #   Returns a list with group equal matrix and vectors constraints

    # Details:
    #   Takes care of "eqsumW" strings
    #   A_eq W = c_eq
    
    # Arguments:
    #   data - a timeSeries or a fPFOLIODATA object
    #   spec - a fPFOLIOSPEC object
    #   constraints - a constraints string
    
    # Example:
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   setGroupEqConstraints(data)
    #   $Aeq
    #                   SBI          SPI          SII 
    #   Return 4.066340e-07 0.0008417544 0.0002389356 
    #   Budget 1.000000e+00 1.0000000000 1.0000000000 
    #   $ceq
    #   Return Budget 
    #       NA      1

    # FUNCTION:

    # Get Statistics:
    data = portfolioData(data, spec)
    targetReturn = getTargetReturn(spec)[1]
    if (is.null(targetReturn)) targetReturn = NA
       
    # Get Specifications:
    mu <- getMu(data)
    nAssets <- getNAssets(data)
    Assets <- getNames(data)  
    
    # Target Return: 
    Aeq <- matrix(mu, byrow = TRUE, ncol = nAssets)
    
    # Full Investment:
    Aeq <- rbind(Aeq, rep(1, nAssets))
    
    # Dimension Names:
    colnames(Aeq) <- Assets
    rownames(Aeq) <- c("Return", "Budget")
    ceq <- c(Return = targetReturn, Budget = 1)
    
    # Extract and Compose Matrix and Vector:
    what6 = substr(constraints, 1, 6)
    if (!is.null(constraints)) {
        nC = length(constraints)
        for (i in 1:nC) {
            if (what6[i] == "eqsumW")  {
                eqsumW = rep(0, times = nAssets)
                names(eqsumW) <- Assets
                eval(parse(text = constraints[i]))
                Aeq = rbind(Aeq, eqsumW = sign(eqsumW))
                a = strsplit(constraints[i], "=")[[1]][2]
                ceq = c(ceq, eqsumW = as.numeric(a))
            }
        }
    }
    
    # Return Value:
    list(Aeq = Aeq, ceq = ceq)
}


# ------------------------------------------------------------------------------
   
    
setGroupMatConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # Description:
    #   Returns a list with group matrix and vectors constraints

    # Arguments:
    #   data - a timeSeries or a fPFOLIODATA object
    #   spec - a fPFOLIOSPEC object
    #   constraints - a constraints string$
    
    # Details:
    #   Takes care of "minsumW" and "maxsumW" strings
    #   a_vec <= A_mat W <= b_vec
    
    # Example:
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   spec = portfolioSpec()
    #   constraints = "LongOnly"
    #   setGroupMatConstraints(data, spec, constraints)
    #   constraints = c("minsumW[3:5]=0.1", "maxsumW[c(1,4)]=0.7", "maxsumW[1:3]=0.8", "maxsumW[c(2,5)]=0.8")
    #   setGroupMatConstraints(data, spec, constraints)
     
    # FUNCTION:
        
    # Get Statistics:
    data = portfolioData(data, spec)     
    
    # Get Specifications:
    nAssets = getNAssets(data)
    assetNames <- getNames(data)          

    # Extrac and Compose Matrix and Vectors:
    what7 = substr(constraints, 1, 7)
    
    if (!is.null(constraints)) {
        nC = length(constraints)
        
        count = 0
        Amat = NULL
        avec = NULL
        for (i in 1:nC) {
            if (what7[i] == "minsumW")  {
                count = count + 1
                minsumW = rep(0, times = nAssets)
                names(minsumW) <- assetNames
                eval(parse(text = constraints[i]))
                Amat = rbind(Amat, minsumW = sign(minsumW))
                a = strsplit(constraints[i], "=")[[1]][2]
                avec = c(avec, as.numeric(a))
            }
        }
        if (!is.null(Amat)){
            colnames(Amat) = assetNames
            names(avec) = rep("lower", count)
        }
        
        count = 0
        Bmat = NULL
        bvec = NULL
        for (i in 1:nC) {
            if (what7[i] == "maxsumW")  {
                count = count + 1
                maxsumW = rep(0, times = nAssets)
                names(maxsumW) <- assetNames
                eval(parse(text = constraints[i]))
                Bmat = rbind(Bmat, maxsumW = sign(maxsumW))
                b = strsplit(constraints[i], "=")[[1]][2]
                bvec = c(bvec, as.numeric(b))
            }
        }
        if (!is.null(Bmat)) {
            colnames(Bmat) = assetNames
            names(bvec) = rep("upper", count)
        }
    }

    # Return Value:
    list(Amat = Amat, Bmat = Bmat, avec = avec, bvec = bvec)
}


# ------------------------------------------------------------------------------
 

setRiskBudgetConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Rmetrics

    # Description:
    #   Returns a list with risk budget constraints vectors

    # Arguments:
    #   data - a timeSeries or a fPFOLIODATA object
    #   spec - a fPFOLIOSPEC object
    #   constraints - a constraints string
    
    # Details:
    #   Takes care of "minB" and "maxB" strings
    #   minB <= RiskBudget <= maxB

    # Example:
    #   Constraints = c("minB[3:4]=0.1","maxB[1:3]=0.3","maxB[c(4,6)]=0.4")
    #   setRiskBudgetConstraints(8,  constraints = Constraints)

    # FUNCTION:

    # Create Data Object:
    data = portfolioData(data, spec)

    # Get Specifications:
    mu = getMu(data)
    Sigma = getSigma(data)
    N = nAssets = getNAssets(data)
    nameAssets <- getNames(data)

    # Extract and Compose Risk Budgets:
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
    ans = list(minB = minB, maxB = maxB)
    # Return Value:
    ans
}


################################################################################


setBoxGroupConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # Description:
    #   Returns a matrix of box/group constraints

    # Arguments:
    #   data - a portfolio data object
    #   spec - a portfolio specification object
    #   constraints - a constraints string
    
    # Details:
    #   A W >= b0 such that cbind(A,b0) will be returned
    
    # Example:
    #   data = 100 * as.timeSeries(data(LPP2005REC))[, 1:6]
    #   spec = portfolioSpec(); setTargetReturn(spec) = mean(data)
    #   constraints = c("minW[3:4]=0.1", "maxW[5:6]=0.8", "minsumW[1:3]=0.2", "maxsumW[c(2,4)]=0.8")
    #   setBoxGroupConstraints(data, spec, constraints)

    # FUNCTION:

    # Get Statistics:
    Data = portfolioData(data, spec)

    # Get Specifications:
    mu = getMu(Data)
    Sigma = getSigma(Data)
    N = nAssets = getNAssets(Data)
    nameAssets <- getNames(Data)

    # Target Return:
    targetReturn = getTargetReturn(spec)[1]
    weights = getWeights(spec)
    if(is.null(targetReturn) & is.null(weights)) {
        weights = rep(1/N, N)
        # warning("Equal Weights Portfolio in use")
    }
    if(is.null(targetReturn)) {
        targetReturn = (weights %*% Sigma %*% weights)[1, 1]
    }

    # Compose Matrix A:
    A = matrix(c(mu, rep(1, times = N)), byrow = TRUE, ncol = N)
    ## A = matrix(c(rep(1, times = N), mu), byrow = TRUE, ncol = N)
    A = rbind(A, diag(1, N), diag(-1, N))
    # colnames(A) = paste("A", 1:N, sep = "")
    colnames(A) <- nameAssets
    rownames(A) = c("Return", "Budget", paste("minW", 1:N, sep = ""),
        paste("maxW", 1:N, sep = ""))

    # Compose vector b0:
    minW = rep(0, N)
    maxW = rep(1, N)
    names(minW) <- names(maxW) <- nameAssets
    b0 = matrix(c(targetReturn, 1, minW, -maxW), ncol = 1)
    colnames(b0) = "b0"
    if (!is.null(constraints)) {
        nC = length(constraints)
        what = substr(constraints, 1, 4)
        for (i in 1:nC) {
            if (what[i] == "minW" | what[i] == "maxW") {
                eval(parse(text = constraints[i]))
            }
        }
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


################################################################################

 