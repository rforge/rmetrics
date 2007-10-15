
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                     PRINT AND PLOT METHODS:           
#  show.fPORTFOLIO               S4 Print method for 'fPPORTFOLIO' objects
################################################################################


show.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   S4 Print Method for an object of class "fPORTFOLIO"
    
    # Arguments:
    #   object - an object of class "fPORTFOLIO"
    
    # FUNCTION:
     
    # Title:
    cat("\nTitle:\n ")
    cat(getTitle(object), "\n")
    
    # Call:
    cat("\nCall:\n ")
    print.default(getCall(object))
    
    # Target Weights:
    cat("\nPortfolio Weight(s):\n")
    weights = round(getWeights(object), digits = 4)
    if (length(weights) == 1) {
        cat(" ", weights, "\n")
    } else {
        print.table(weights)
    }
    
    # Covariance Risk Budgets:
    cat("\nRiskBudget(s):\n")
    riskBudgets = round(getCovRiskBudgets(object), digits = 4)
    if (length(riskBudgets) == 1) {
        cat(" ", riskBudgets, "\n")
    } else {
        print.table(riskBudgets)
    }
    
    # Tail Risk Budgets:
    if (FALSE) {
        if (!is.na(getTailRiskBudgets(object))) {
             cat("\nRiskBudget(s):\n")
            riskBudgets = round(getTailRiskBudgets(object), digits = 4)
            if (length(riskBudgets) == 1) {
                cat(" ", riskBudgets, "\n")
            } else {
                print.table(riskBudgets)
            }   
        }  
    }
  
    # Target Returns:   
    # cat("\nTarget Return(s):\n")
    targetReturn = object@portfolio$targetReturn # getTargetReturn(object)
    # print(targetReturn)
 
    # Target Risk:
    # cat("\nTarget Risk(s):\n")
    targetRisk = object@portfolio$targetRisk # getTargetRisk(object) 
    # print(targetRisk)
    
    ##
    spec = getSpec(object)
    cat("\nTarget Risk(s) and Return(s):\n")
    if (is.null(dim(targetReturn))) {
        targetReturn = matrix(targetReturn, nrow = 1)
        colnames(targetReturn) = getEstimator(spec)[1]
    }
    if (is.null(dim(targetRisk))) {
        targetRisk = matrix(targetRisk, nrow = 1)
        colnames(targetRisk) = getEstimator(spec)[2]
    }
    target = cbind(targetReturn, targetRisk)
    colnames(target) = c(colnames(targetReturn), colnames(targetRisk))    
    if (nrow(target) == 1) {
        print(target[1, ])
    } else {
        print(target)
    }
       
    # Description:
    cat("\nDescription:\n ")
    cat(getDescription(object), "\n")
        
    # Return Value: 
    invisible(object)
}


# ------------------------------------------------------------------------------


setMethod("show", "fPORTFOLIO", show.fPORTFOLIO)


################################################################################

