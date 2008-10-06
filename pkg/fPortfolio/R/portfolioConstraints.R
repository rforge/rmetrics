
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
# FUNCTION:               DESCRIPTION:
#  portfolioConstraints    Returns an object of class fPFOLIOCON
#  minWConstraints         Returns vector with min box constraints
#  maxWConstraints         Returns vector with max box constraints
#  eqsumWConstraints       Returns list with group equal vec/matrix constraints
#  minsumWConstraints      Returns list with group min vec/matrix constraints
#  maxsumWConstraints      Returns list with group max vec/matrix constraints
#  minBConstraints         Returns vector with min cov risk budget constraints
#  maxBConstraints         Returns vector with max cov risk budget constraints
#  minFConstraints         Returns vector with min nonlin functions constraints
#  maxFConstraints         Returns vector with max nonlin functions constraints
################################################################################


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
    #           "LongOnly", "Short",      # LongOnly and Short Notification
    #           "minW", "maxW",           # Box Constraints
    #           "minsumW", "maxsumW",     # left/right Sided Group Constraints
    #           "minB", "maxB",           # Covariance Risk Budgets
    #           "listF", "minF", "maxF")  # Nonlinear Functions Constraints
    
    # Details:
    #   This function takes the constraints strings and converts them to
    #   constraints vectors and matrices of the following form:
    #   1. boxConstraints           W_min <= W <= W_max
    #   2. groupEqConstraints       A_eq W = c_eq
    #   3. groupMatConstraints      a_vec <= A_mat W <= b_vec
    #   4. riskBudgetConstraints    a <= RiskBudget <= b
    #   These values are returned as list in four slots.
    
    # Example:
    #   data = .lppData; spec = .mvSpec 
    #   portfolioConstraints(data, spec, "LongOnly")
    #   constraints = c("minW[1:3]=0.1", "maxW[4:6]=0.9", "minsumW[c(2,5)]=0.2", "maxsumW[c(1,4)]=0.9")
    #   portfolioConstraints(data, spec, constraints)
    
    # FUNCTION:

    # Already done ...
    if (class(constraints) == "fPFOLIOCON") return(constraints)
    
    # Handle NULL - A NULL  :
    if (is.null(constraints)) constraints = "LongOnly"
    
    # Check Vector of Valid Strings - these are strings ...
    validStrings = c(
        "LongOnly", "Short",      # LongOnly and Short Notification
        "minW", "maxW",           # Box Constraints
        "minsumW", "maxsumW",     # left and right Sided Group Constraints
        "minB", "maxB",           # Covariance Risk Budgets
        "listF", "minF", "maxF")  # Nonlinear Functions Constraints
    if (any(constraints == "Short")) setSolver(spec) = "solveRshortExact"
    # usedStrings = unique(sort(sub("\\[.*", "", constraints)))
    # checkStrings = usedStrings %in% validStrings
    # check = (sum(!checkStrings) == 0)
    # if (check) check = "valid" else stop("Invalid Constraints String(s)")
    stringConstraints = constraints
    # attr(stringConstraints, "control") = check
    
    minW = minWConstraints(data, spec, constraints)
    maxW = maxWConstraints(data, spec, constraints)
    eqsumW = eqsumWConstraints(data, spec, constraints)
    minsumW = minsumWConstraints(data, spec, constraints)
    maxsumW = maxsumWConstraints(data, spec, constraints)
    minB = minBConstraints(data, spec, constraints)
    maxB = maxBConstraints(data, spec, constraints)
    listF = listFConstraints(data, spec, constraints)
    minF = minFConstraints(data, spec, constraints)
    maxF = maxFConstraints(data, spec, constraints)
    
    if(is.null(minW)) minW = numeric()
    if(is.null(maxW)) maxW = numeric()
    if(is.null(eqsumW)) eqsumW = matrix(NA)
    if(is.null(minsumW)) minsumW = matrix(NA)
    if(is.null(maxsumW)) maxsumW = matrix(NA)
    if(is.null(minB)) minB = numeric()
    if(is.null(maxB)) maxB = numeric()
    if(is.null(maxsumW)) maxsumW = matrix(NA)
    if(is.null(minF)) minF = numeric()
    if(is.null(maxF)) maxF = numeric()

    # Return Value:
    new("fPFOLIOCON",
        stringConstraints = stringConstraints,
        minWConstraints = minW,
        maxWConstraints = maxW,
        eqsumWConstraints = eqsumW,
        minsumWConstraints = minsumW,
        maxsumWConstraints = maxsumW,
        minBConstraints = minB, 
        maxBConstraints = maxB,
        listFConstraints = listF,
        minFConstraints = minF, 
        maxFConstraints = maxF)
}
            

################################################################################
    
 
minWConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # Description:
    #   Returns a vector with min box constraints

    # Details:
    #   Takes care of "minW" strings, i.e. lower blounds
    #   W >= c
    
    # Arguments:
    #   data - a timeSeries or a fPFOLIODATA object
    #   spec - a fPFOLIOSPEC object
    #   constraints - a constraints string
    
    # Example:
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   spec = portfolioSpec()
    #   constraints = c("minW[3:4]=0.1", "maxW[5:6]=0.8")
    #   minWConstraints(data, spec, constraints)
    
    # FUNCTION:
    
    # Settings:
    Data = portfolioData(data, spec)
    nAssets = getNAssets(Data)
    assetNames <- getNames(Data)
    
    # Consider LongOnly:
    if("LongOnly" %in% constraints) {
        minW = rep(0, nAssets)
        names(minW) = assetNames
        return(minW)
    }
    
    # Consider Unlimited Short:
    if("Short" %in% constraints) {
        minW = rep(-Inf, nAssets)
        names(minW) = assetNames
        return(minW)
    }

    # Extract and Compose Vectors a_vec and b_vec:
    minW = rep(0, nAssets)
    if (!is.null(constraints)) {
        nC = length(constraints)
        what = substr(constraints, 1, 4)
        for (i in 1:nC) {
            if (what[i] == "minW") eval(parse(text = constraints[i]))
        }
    }
    names(minW) = assetNames
    return(minW)

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


maxWConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # Description:
    #   Returns a vector with max box constraints

    # Details:
    #   Takes care of "maxW" strings, i.e. upper bounds
    #   W >= c
    
    # Arguments:
    #   data - a timeSeries or a fPFOLIODATA object
    #   spec - a fPFOLIOSPEC object
    #   constraints - a constraints string
    
    # Example:
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   spec = portfolioSpec()
    #   constraints = c("minW[3:4]=0.1", "maxW[5:6]=0.8")
    #   maxWConstraints(data, spec, constraints)
    
    # FUNCTION:
    
    # Settings:
    Data = portfolioData(data, spec)
    nAssets = getNAssets(Data)
    assetNames <- getNames(Data)
    
    # Consider LongOnly:
    if("LongOnly" %in% constraints) {
        maxW = rep(1, nAssets)
        names(maxW) = assetNames
        return(maxW)
    }
    
    # Consider Unlimited Short:
    if("Short" %in% constraints) {
        maxW = rep(Inf, nAssets)
        names(maxW) = assetNames
        return(maxW)
    }

    # Extract and Compose Vectors a_vec and b_vec:
    maxW = rep(1, nAssets)
    if (!is.null(constraints)) {
        nC = length(constraints)
        what = substr(constraints, 1, 4)
        for (i in 1:nC) {
            if (what[i] == "maxW") eval(parse(text = constraints[i]))
        }
    }
    names(maxW) = assetNames
    return(maxW)

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


eqsumWConstraints <-
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
    #   spec = portfolioSpec(); setTargetReturn(spec) = mean(data)
    #   constraints = "eqsumW[1:6]=1"
    #   eqsumWConstraints(data, spec, constraints)
    #   eqsumWConstraints(data, spec, constraints = "LongOnly")
    #   eqsumWConstraints(data, spec, constraints = c("LongOnly","Partial"))

    # FUNCTION:

    # Get Statistics:
    data = portfolioData(data, spec)
    targetReturn = getTargetReturn(spec)[1]
    if (is.null(targetReturn)) {
        targetReturn = NA
        stop("Target Return is Missing")
    }
       
    # Get Specifications:
    mu <- getMu(data)
    nAssets <- getNAssets(data)
    assetsNames <- getNames(data)  
    
    # Target Return: 
    Aeq <- matrix(mu, byrow = TRUE, ncol = nAssets)
    
    # Full or partial Investment?
    if ("partial" %in% tolower(constraints)) 
        fullInvest = FALSE else fullInvest = TRUE
    
    # Full Investment:
    #   - negative to handle better partial Investment in Rquadprog:
    if (fullInvest) Aeq <- rbind(Aeq, -rep(1, nAssets))
    
    # Dimension Names:
    colnames(Aeq) <- assetsNames
    if (fullInvest) 
        rownames(Aeq) <- c("Return", "Budget") 
    else 
        rownames(Aeq) <- "Return"
        
    # RHS Vector:
    if (fullInvest) 
        ceq <- c(Return = targetReturn, Budget = -1) 
    else 
        ceq <- c(Return = targetReturn)
    
    # Extract and Compose Matrix and Vector:
    what6 = substr(constraints, 1, 6)
    if (!is.null(constraints)) {
        nC = length(constraints)
        for (i in 1:nC) {
            if (what6[i] == "eqsumW")  {
                eqsumW = rep(0, times = nAssets)
                names(eqsumW) <- assetsNames
                eval(parse(text = constraints[i]))
                Aeq = rbind(Aeq, eqsumW = sign(eqsumW))
                a = strsplit(constraints[i], "=")[[1]][2]
                ceq = c(ceq, eqsumW = as.numeric(a))
            }
        }
    }
    
    # Return Value:
    cbind(ceq, Aeq)
}


# ------------------------------------------------------------------------------

   
minsumWConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # Description:
    #   Returns a list with group matrix and vectors constraints

    # Arguments:
    #   data - a timeSeries or a fPFOLIODATA object
    #   spec - a fPFOLIOSPEC object
    #   constraints - a constraints string$
    
    # Details:
    #   Takes care of "minsumW" strings
    #   a_vec <= A_mat W
    
    # Example:
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   spec = portfolioSpec(); setTargetReturn(spec) = mean(data)
    #   constraints = c("minsumW[2:3]=0.2", "minsumW[c(1,4:6)]=0.2")
    #   minsumWConstraints(data, spec, constraints)   
    #   minsumWConstraints(data, spec)
    
    # FUNCTION:
        
    # Get Statistics:
    data = portfolioData(data, spec)     
    
    # Get Specifications:
    mu = getMu(data)
    nAssets = getNAssets(data)
    assetNames <- getNames(data)          

    # Extrac and Compose Matrix and Vectors:
    what7 = substr(constraints, 1, 7)
    
    if (!is.null(constraints)) {
        nC = length(constraints)
        
        count = 0
        Amat = NULL
        avec = NULL
        
        # Partial Investment:
        if ("partial" %in% tolower(constraints)) {
            Amat <- rbind(Amat, rep(1, times = nAssets))
            avec <- c(avec, 0)
        }
 
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
        
    }

    # Return Value:
    cbind(avec = avec, Amat = Amat)
}


# ------------------------------------------------------------------------------

   
maxsumWConstraints <-
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
    #   spec = portfolioSpec(); setTargetReturn(spec) = mean(data)
    #   constraints = c("maxsumW[2:3]=0.7", "maxsumW[c(1,4:6)]=0.8")
    #   maxsumWConstraints(data, spec, constraints) 
    #   maxsumWConstraints(data, spec)      
    
    # FUNCTION:
        
    # Get Statistics:
    data = portfolioData(data, spec)     
    
    # Get Specifications:
    mu = getMu(data)
    nAssets = getNAssets(data)
    assetNames <- getNames(data)          

    # Extract and Compose Matrix and Vectors:
    what7 = substr(constraints, 1, 7)
    
    if (!is.null(constraints)) {
        nC = length(constraints)
        
        count = 0
        Amat = NULL
        avec = NULL
        
        # Partial Investment:
        if ("partial" %in% tolower(constraints)) {
            Amat <- rbind(Amat, rep(1, times = nAssets))
            avec <- c(avec, 1)
        }
        
        for (i in 1:nC) {
            if (what7[i] == "maxsumW")  {
                count = count + 1
                maxsumW = rep(0, times = nAssets)
                names(maxsumW) <- assetNames
                eval(parse(text = constraints[i]))
                Amat = rbind(Amat, maxsumW = sign(maxsumW))
                a = strsplit(constraints[i], "=")[[1]][2]
                avec = c(avec, as.numeric(a))
            }
        }
        if (!is.null(Amat)) {
            colnames(Amat) = assetNames
            names(avec) = rep("upper", count)
        }
    }

    # Return Value:
    cbind(avec = avec, Amat = Amat)
}


# ------------------------------------------------------------------------------


minBConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Rmetrics

    # Description:
    #   Returns a list with min risk budget constraints vectors

    # Arguments:
    #   constraints - a constraints string

    # Example:
    #   data = as.timeSeries(data(LPP2005REC))[, 1:6]
    #   spec = portfolioSpec()
    #   constraints = c("minB[3:4]=0.1","maxB[1:3]=0.3","maxB[c(4,6)]=0.4")
    #   minBConstraints(data, spec, constraints)
    #   minBConstraints(data, spec)

    # FUNCTION:

    # Create Data Object:
    Data = portfolioData(data, spec)

    # Get Specifications:
    nAssets = getNAssets(Data)
    assetsNames <- getNames(Data)

    # Extract and Compose Risk Budgets:
    minB = rep(-Inf, nAssets)
    names(minB) <- assetsNames
    if (!is.null(constraints)) {
        nC = length(constraints)
        what = substr(constraints, 1, 4)
        for (i in 1:nC) {
            if (what[i] == "minB") eval(parse(text = constraints[i]))
        }
    }
    
    # Return Value:
    minB
}


# ------------------------------------------------------------------------------


maxBConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Rmetrics

    # Description:
    #   Returns a list with max risk budget constraints vectors

    # Arguments:
    #   constraints - a constraints string

    # Example:
    #   data = as.timeSeries(data(LPP2005REC))[,1:6]
    #   spec = portfolioSpec()
    #   constraints = c("minB[3:4]=0.1","maxB[1:3]=0.3","maxB[c(4,6)]=0.4")
    #   maxBConstraints(data, spec, constraints)
    #   maxBConstraints(data, spec)
    
    # FUNCTION:

    # Create Data Object:
    Data = portfolioData(data, spec)

    # Get Specifications:
    N = nAssets = getNAssets(Data)
    assetsNames <- getNames(Data)

    # Extract and Compose Risk Budgets:
    maxB = rep(1, N)
    names(maxB) = assetsNames
    if (!is.null(constraints)) {
        nC = length(constraints)
        what = substr(constraints, 1, 4)
        for (i in 1:nC) {
            if (what[i] == "maxB") eval(parse(text = constraints[i]))
        }
    }
    
    # Return Value:
    maxB
}


# ------------------------------------------------------------------------------


listFConstraints <-  
function(data, spec = portfolioSpec(), constraints = "LongOnly") 
{
    # Description:
    #   Nonlinear Constraints
    
    # Example: listFConstraints(c("minF=-0.04", "listF(maxdd)"))
    
    # FUNCTION:
    
    nlin = list()
    matched = pmatch("listF" , constraints)
    if(!is.na(matched)) {
        Constraints = paste("nlin = ", constraints[matched])
        Constraints = sub("listF", "list", Constraints)
        eval(parse(text = Constraints))
    }
    return(nlin)
}   
        

minFConstraints <-  
function(data, spec = portfolioSpec(), constraints = "LongOnly") 
{
    # Description:
    #   Nonlinear Constraints
    
    # Example: minFConstraints("minF=-0.04")
    
    # FUNCTION:
    
    minF = NULL
    matched = pmatch("minF" , constraints)
    if(!is.na(matched)) eval(parse(text = constraints[matched]))
    return(minF)
}

maxFConstraints <- 
function(data, spec = portfolioSpec(), constraints = "LongOnly") 
{
    # Description:
    #   Nonlinear Constraints
    
    # Example: maxFConstraints(c("LongOnly", "maxF=0")
    
    # FUNCTION:
    
    maxF = NULL
    matched = pmatch("maxF" , constraints)
    if(!is.na(matched)) eval(parse(text = constraints[matched]))
    return(maxF)
}


################################################################################

