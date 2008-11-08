
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
#  show.fPORTFOLIO               S4 Print method for 'fPPORTFOLIO' objects
#  show.fPFOLIODATA              Print method for 'fPFOLIODATA' objects
#  show.fPFOLIOSPEC              Print method for 'fPFOLIOSPEC' objects
#  show.fPFOLIOCON               Print method for 'fPFOLIOCON' objects

################################################################################


setMethod("show", "fPORTFOLIO",
    function(object)
{
    # Description:
    #   S4 Print Method for an object of class "fPORTFOLIO"

    # Arguments:
    #   object - an object of class "fPORTFOLIO"

    # FUNCTION:

    # Print Title:
    cat("\nTitle:\n ")
        cat(getType(object),  getTitle(object),     "\n")
        cat(" Estimator:   ", getEstimator(object), "\n")
        cat(" Solver:      ", getSolver(object),    "\n")
        cat(" Optimize:    ", getOptimize(object),  "\n")
        cat(" Constraints: ", getConstraintsTypes(object), "\n")
    if(getType(object) == "CVaR")
        cat(" VaR Alpha:   ", getAlpha(object),     "\n")
        #at(" Objective:   ", getObjective(object), "\n")

    # Print Call:
    # cat("\nCall:\n ")
    # print.default(getCall(object))

    # Print Target Weights:
    cat("\nPortfolio Weights:\n")
    weights = data.frame(round(getWeights(object), digits = 4))
    if (NROW(weights) == 1) rownames(weights) = ""
    print(weights)

    # Print Covariance Risk Budgets:
    cat("\nCovariance Risk Budgets:\n")
    covRiskBudgets = data.frame(round(getCovRiskBudgets(object), digits = 4))
    if (NROW(covRiskBudgets) == 1) rownames(covRiskBudgets) = ""
    print(covRiskBudgets)

    # Print Tail Risk Budgets:
    if (FALSE) {
        if (!is.na(getTailRiskBudgets(object))) {
            cat("\nRiskBudget(s):\n")
            riskBudgets = round(getTailRiskBudgets(object), digits = 4)
            print.table(riskBudgets)
        }
    }

    # Print Target Return and Risks:
    cat("\nTarget Return and Risks:\n")
    targetReturn = getTargetReturn(object)
    targetRisk = getTargetRisk(object)
    target = data.frame(targetReturn, targetRisk)
    if (NROW(target) == 1) rownames(target) = ""
    print(round(target, digits = 4))

    # Print Description:
    cat("\nDescription:\n ")
    cat(getDescription(object), "\n")

    # Return Value:
    invisible(object)
})


# ------------------------------------------------------------------------------


setMethod("show", "fPFOLIODATA",
    function(object)
{
    # Description:
    #   S4 Print Method for an object of class "fPFOLIODATA"

    # Arguments:
    #   object - an object of class "fPFOLIOSPEC"

    # FUNCTION:

    # Series:
    cat("\nSeries Data:\n\n")
    print(object@data$series)

    # Statistics:
    cat("\nStatistics:\n\n")
    print(object@statistics)

    # Tailrisk:
    # NYI

    # Return Value:
    invisible(object)
})


# ------------------------------------------------------------------------------


setMethod("show", "fPFOLIOSPEC",
    function(object)
{
    # Description:
    #   S4 Print Method for an object of class "fPFOLIOSPEC"

    # Arguments:
    #   object - an object of class "fPFOLIOSPEC"

    # FUNCTION:

    # Model:
        cat("\nPortfolio Specification:\t")
        cat("\n Portfolio Type:           ",
            object@model$type)
        cat("\n Optimize:                 ",
            object@model$optimize)
        cat("\n Covariance Estimator:     ",
            object@model$estimator)

    # Portfolio:
    if (!is.null(object@portfolio$weights)) {
        cat("\n Portfolio Weights:    ",
            object@portfolio$weights)
    }
    if (!is.null(object@portfolio$targetReturn)) {
        cat("\n Target Return:            ",
            object@portfolio$targetReturn)
    }
    if (!is.null(object@portfolio$targetAlpha)) {
        cat("\n Target Alpha:             ",
        as.character(object@portfolio$targetAlpha))
    }
    if (!is.null(object@portfolio$riskFreeRate)) {
        cat("\n Portfolio Risk-Free Rate: ",
        as.character(object@portfolio$riskFreeRate))
    }
    if (!is.null(object@portfolio$nFrontierPoints)) {
        cat("\n Number of Frontier Points:",
        as.character(object@portfolio$nFrontierPoints))
    }

    # Optimization:
        cat("\n Optimizer:                ",
            object@optim$solver, "\n")

    # Return Value:
    invisible(object)
})


# ------------------------------------------------------------------------------

if(FALSE) {
setMethod("show", "fPFOLIOCON",
    function(object)
{
    # Description:
    #   S4 Print Method for an object of class "fPFOLIODATA"

    # Arguments:
    #   object - an object of class "fPFOLIOSPEC"

    # FUNCTION:

    # Print Title:
    cat("\nTitle:\n ")
    cat("Portfolio Constraints\n")

    cat("\nConstraint String:\n")
    print(object@stringConstraints)

    cat("\nBox Constraints:\n")
    print(object@boxConstraints)

    cat("Group-Equal Constraints:\n")
    print(object@groupEqConstraints)

    cat("Group-Matrix Constraints:\n")
    print(object@groupMatConstraints)

    cat("Cov Risk Budget Constraints:\n")
    print(object@riskBudgetConstraints)
    
    cat("Box/Group Constraints:\n")
    print(object@boxGroupConstraints)

    # Return Value:
    invisible(object)
})
}


# ------------------------------------------------------------------------------


setMethod("show", "fPFOLIOCON",
    function(object)
{
    # Description:
    #   S4 Print Method for an object of class "fPFOLIODATA"

    # Arguments:
    #   object - an object of class "fPFOLIOSPEC"

    # FUNCTION:

    # Print Title:
    cat("\nTitle:\n ")
    cat("Portfolio Constraints\n")

    minmaxW = rbind(object@minWConstraints, object@maxWConstraints)
    if (length(minmaxW)) {
        cat("\nLower/Upper Bounds:\n")
        print(minmaxW)
    }

    eqsumW = object@eqsumWConstraints
    if (sum(dim(eqsumW)) > 2) {
        cat("\nEqual Matrix Constraints:\n")
        print(eqsumW)
    }
    
    minsumW = object@minsumWConstraints
    if (sum(dim(minsumW)) > 2) {
        cat("\nLower Matrix Constraints:\n")
        print(minsumW)
    }
    
    maxsumW = object@maxsumWConstraints
    if (sum(dim(minsumW)) > 2) {
        cat("\nUpper Matrix Constraints:\n")
        print(maxsumW)
    }
    
    minmaxB = rbind(object@minBConstraints, object@maxBConstraints)
    if (length(minmaxB) > 0) {
        cat("\nLower/Upper Cov Risk Budget Bounds:\n")
        rownames(minmaxB) = c("Lower", "Upper")
        print(minmaxB)
    }
    
    # Return Value:
    invisible(object)
})


################################################################################

