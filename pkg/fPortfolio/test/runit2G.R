
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

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
# FUNCTION:                    DESCRIPTION:  
#  solveRQuadprog               Calls Goldfarb and Idnani's QP solver
#  solveRDonlp2                 Calls Spelucci's donlp2 solver
# FUNCTION:                    DESCRIPTION:
#  setSolver                    Sets the desired solver
#  setSolver<-                  Sets the desired solver                    
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioSolver, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.solverRQuadprog =
function()
{ 
    # Install "Rdonlp2" from - http://arumat.net/Rdonlp2/
    require(Rdonlp2)
    
    # Direct Access:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(as.matrix(Data))
    
    # Default Constraints:
    Constraints = "LongOnly"
    
    # Quadprog:
    solveRQuadprog(Data, Spec, Constraints)$solution 
    
    # Check Termination Error:
    round(getWeights(efficientPortfolio(Data, Spec, Constraints)), 2)
    round(getWeights(efficientPortfolio(10*Data, Spec, Constraints)), 2) 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solverRDonlp2 =
function()
{ 
    # Install "Rdonlp2" from - http://arumat.net/Rdonlp2/
    require(Rdonlp2)
    
    # Direct Access:   
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(as.matrix(Data))
    
    # Long Only Constraints:
    Constraints = NULL
    
    # Quadprog:
    setSolver(Spec) = "RQuadprog"
    round(solveRQuadprog(Data, Spec, Constraints)$solution, 3)
    round(getWeights(efficientPortfolio(Data, Spec, Constraints)), 3)
    
    # Donlp2:
    setSolver(Spec) = "RDonlp2"
    round(solveRDonlp2(Data, Spec, Constraints)$solution, 3)
    round(getWeights(efficientPortfolio(Data, Spec, Constraints)), 3)
    
    # Add Budget Constraints:
    Constraints = c("minW[1:4]=0", "maxB[1:4]=1")
    solveRDonlp2(Data, Spec, Constraints)$solution
    round(getWeights(efficientPortfolio(Data, Spec, Constraints)), 2)
    
    # PART II:
    
    # Scale Returns - You should get the same Weights:
    Data2 = 10* Data
    setTargetReturn(Spec) = mean(as.matrix(Data2))
    
    # Long Only Constraints:
    Constraints = NULL
    
    # Scaled Quadprog:
    setSolver(Spec) = "RQuadprog"
    round(solveRQuadprog(Data2, Spec, Constraints)$solution, 3)
    round(getWeights(efficientPortfolio(Data2, Spec, Constraints)), 3)
    
    # Scaled Donlp2:
    setSolver(Spec) = "RDonlp2"
    round(solveRDonlp2(Data2, Spec, Constraints)$solution, 3)
    round(getWeights(efficientPortfolio(Data2, Spec, Constraints)), 3)
    
    # Scaled Donlp2 - Add Budget Constraints:
    Constraints = c("minW[1:4]=0", "maxB[1:4]=1")
    solveRDonlp2(Data2, Spec, Constraints)$solution
    myPf = efficientPortfolio(Data2, Spec, Constraints)
    round(getWeights(myPf), 2)
    getRiskBudgets(myPf)
    
    # Scaled Donlp2 - Add Budget Constraints:
    Constraints = c("minW[1:4]=0", "maxB[1:4]=0.3")
    solveRDonlp2(Data2, Spec, Constraints)$solution
    myPf = efficientPortfolio(Data2, Spec, Constraints)
    round(getWeights(myPf), 2)
    getRiskBudgets(myPf)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2G.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

