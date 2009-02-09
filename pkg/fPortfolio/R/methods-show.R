
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

    nAssets = getNAssets(object)
    Names = getNames(object)
    
    # Print Target Weights:
    cat("\nPortfolio Weights:\n")
    table = matrix(round(getWeights(object@portfolio), digits = 4), ncol = nAssets)
    colnames(table) = Names
    rownames(table) = 1:NROW(table)
    print.table(table)

    # Print Covariance Risk Budgets:
    cat("\nCovariance Risk Budgets:\n")
    table = matrix(round(getCovRiskBudgets(object@portfolio), digits = 4), ncol = nAssets)
    colnames(table) = Names
    rownames(table) = 1:NROW(table)
    print.table(table)
    
    # Print Tail Risk Budgets:
    # to do ...

    # Print Target Return and Risks:
    # DW: Note obgect@targetR* is a list do not use getTargetR*() 
    cat("\nTarget Return and Risks:\n")
    targetReturn = matrix(getTargetReturn(object@portfolio), ncol = 2)
    targetRisk = matrix(getTargetRisk(object@portfolio), ncol = 4)
    target = round(cbind(targetReturn, targetRisk), digits = 4)
    colnames(target) = c("mean", "mu", "Cov", "Sigma", "CVaR", "VaR")
    rownames(target) = 1:NROW(target) 
    print.table(target)

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
    cat("\nHead/Tail Series Data:\n\n")
    print(head(object@data$series, n = 3))
    print(tail(object@data$series, n = 3))

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
    
    cat("\nModel List:\t")
       
    cat("\n Type:                     ",
        object@model$type)
        
    cat("\n Optimize:                 ",
        object@model$optimize)
        
    cat("\n Estimator:                ",
        object@model$estimator)
        
    if (length(object@model$tailRisk) > 0) {
        cat("\n Tail Risk:                ",
            object@model$tailRisk)
    } else {
        cat("\n Tail Risk:                ", "list()")
    }
    
    cat("\n Params:                   ",
        paste(names(unlist(object@model$params)), "=", 
            unlist(object@model$params)))
    
    # Portfolio:
    
    cat("\n\nPortfolio List:\t")
    
    if (!is.null(object@portfolio$weights)) {
        cat("\n Portfolio Weights:        ",
            object@portfolio$weights)
    } else {
        cat("\n Target Weights:           ", "NULL")
    }
    
    if (!is.null(object@portfolio$targetReturn)) {
        cat("\n Target Return:            ",
            object@portfolio$targetReturn)
    } else {
        cat("\n Target Return:            ", "NULL")
    }
    
    if (!is.null(object@portfolio$targetRisk)) {
        cat("\n Target Risk:              ",
            object@portfolio$targetRisk)
    } else {
        cat("\n Target Risk:              ", "NULL")
    }
    
    if (!is.null(object@portfolio$riskFreeRate)) {
        cat("\n Risk-Free Rate:           ",
        as.character(object@portfolio$riskFreeRate))
    }
    
    if (!is.null(object@portfolio$nFrontierPoints)) {
        cat("\n Number of Frontier Points:",
        as.character(object@portfolio$nFrontierPoints))
    }
    
    cat("\n Status:                   ",
        as.character(object@portfolio$status))

        
    # Optimization:
    
    cat("\n\nOptim List:\t")
    
    cat("\n Solver:                   ",
        object@optim$solver)
        
    if (!is.null(object@optim$objective)) {
        cat("\n Objective:                ",
        as.character(object@optim$objective))
    } else {
        cat("\n Objective:                ", "list()" )
    }
    
    cat("\n Options:                  ",
        paste(names(unlist(object@optim$options)), "=", 
            unlist(object@optim$options)))
    
    if (length(object@optim$control) > 0) {
        cat("\n Control:                  ",
        as.character(object@optim$control))
    } else {
        cat("\n Control:                  ", "list()")
    }  
    
    
    cat("\n Trace:                    ",
        object@optim$trace)
    
                 
    # Messages:
    
    cat("\n\nMessage List:\t")
    
    if (!is.null(object@messages$list)) {
        cat("\n List:                     ",
            object@messages$list)
    } else {
        cat("\n List:                     ", "NULL")
    }
    
        
    cat("\n")
            
            
    # Return Value:
    invisible(object)
})


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
    rownames(minmaxW) = c("Lower", "Upper")
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
    if (sum(dim(maxsumW)) > 2) {
        cat("\nUpper Matrix Constraints:\n")
        print(maxsumW)
    }
    
    minmaxB = rbind(object@minBConstraints, object@maxBConstraints)
    if (length(minmaxB) > 0) {
        cat("\nLower/Upper Cov Risk Budget Bounds:\n")
        rownames(minmaxB) = c("Lower", "Upper")
        print(minmaxB)
    }
    
    listF = object@listFConstraints
    minF = object@minFConstraints
    maxF = object@maxFConstraints
    if (length(listF) > 0) {
        cat("\nNon-Linear Function Constraints:\n")
        minmaxF = rbind(minF, maxF)
        colnames(minmaxF) = names(listF)
        rownames(minmaxF) = c("Lower", "Upper")
        print(minmaxF)
    }  
        
        
    # Return Value:
    invisible(object)
})


################################################################################

