
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
################################################################################


portfolioConstraints <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly")
{
    # A function implemented by Rmetrics

    # Description:
    #   Checks Consistency of Constraints Strings

    # Arguments
    #   data - a portfolio data object
    #   spec - a portfolio specification object
    #   constraints - a constraints string

    # FUNCTION:

    # Vector of Valid Strings:
    validStrings = c(
        "LongOnly", "Short",    # LongOnly and Short Notification
        "minW", "maxW",         # Box Constraints
        "minsumW", "maxsumW",   # Group Constraints:
        "minB", "maxB")         # Covariance Risk Budgets
                                # ... Tail Risk Budgets

    # Check Constraints Strings:
    usedStrings = unique(sort(sub("\\[.*", "", constraints)))
    checkStrings = usedStrings %in% validStrings
    check = (sum(!checkStrings) == 0)
    if (check) check = "valid"
    stringConstraints = constraints
    attr(stringConstraints, "control") = check

    # Set BoxGroup Constraints
    boxgroupConstraints =
        .setConstraints(data, spec, constraints, type = "BoxGroup")

    # Set RiskBudget Constraints
    riskbudgetConstraints =
        .setConstraints(data, spec, constraints, type = "RiskBudget")

    # Return Value:
    new("fPFOLIOCON",
        stringConstraints = stringConstraints,
        boxgroupConstraints = boxgroupConstraints,
        riskbudgetConstraints = riskbudgetConstraints)
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
    #   data -
    #   spec -
    #   constraints -
    #   type -

    # FUNCTION:

    # Check Arguments:
    type = match.arg(type)

    # Check Data:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)

    # Short Selling:
    if (length(constraints) > 0) {
        # 'constraints' must be at least of length 1, to be checked ...
        if (constraints[1] == "Short") constraints = NULL
    }

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
    #   data -
    #   spec -
    #   constraints -
    #   type -

    # FUNCTION:

    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)

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
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)

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


################################################################################

