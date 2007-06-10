
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
# FUNCTION:                    CONSTRAINTS:
#  portfolioConstraints         Checks Consistency of Constraints Strings
#  .setConstraints              Transforms constraint strings into a list value
#  .setBoxGroupConstraints       Utility function called by .setConstraints()
#  .setRiskBudgetsConstraints    Utility function called by .setConstraints()
#  .getConstraints              Transforms a constraint list value into strings                
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioConstraints, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.portfolioConstraints =
function()
{ 
   # Load Data:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
   # Set Default Specifications:
   Spec = portfolioSpec() 
   
   # Set Portfolio Constraints:
   Constraints = NULL
   portfolioConstraints(Data, Spec, Constraints)
   
   # Set Portfolio Constraints:
   Constraints = "LongOnly"
   portfolioConstraints(Data, Spec, Constraints)
   
   # Set Portfolio Constraints:
   Constraints = "Short"
   portfolioConstraints(Data, Spec, Constraints)
   
   # Set Portfolio Constraints:
   Constraints = c("minW[1:4]=0", "maxW[1:4]=1")
   portfolioConstraints(Data, Spec, Constraints)
   
   # Return Value:
   return()
}


# ------------------------------------------------------------------------------


test.setConstraints =
function()
{ 
    # Arguments:
    # .setConstraints(data, spec = portfolioSpec(), constraints = NULL, 
    #   type = c("BoxGroup", "RiskBudget"))
    
    # Data, Specification and Constraints:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Spec = portfolioSpec()
    Constraints = NULL
    
    # Set Default Box-Group Constraints:
    # These are: 0 <= W[1:nAssets] <= 1, no Group Constraints ...
    .setConstraints(Data, Spec, Constraints)
    
    # Set Default Covariance Risk-Budget Constraints:
    # These are: 0 <= RiskBudgets[1:nAssets] <= 1, 
    #   no RiskBudget Group Constraints ...
    # Note,  Risk-Budget Constraints have to been added explicitely!
    .setConstraints(Data, Spec, Constraints, type = "RiskBudget")  
    
    # Short:
    Constraints = "Short"
    .setConstraints(Data, portfolioSpec(), Constraints)
    
    # Long Only:
    Constraints = "LongOnly"
    .setConstraints(Data, portfolioSpec(), Constraints)
    
    # minW, maxW:
    Constraints = 
        c("minW[1:nAssets]=0.09", "maxW[1:nAssets]=rep(c(0.6, 0.4),4)")
    .setConstraints(Data, portfolioSpec(), Constraints)
    
    # minsumW, maxsumW:
    Constraints = c("minsumW[c(2,4)]=0.20", "maxsumW[4:6]=0.80")
    .setConstraints(Data, portfolioSpec(), Constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setRiskBudgetsConstraints =
function()
{
    # .setBoxGroupConstraints()       
    #   Utility function called by .setConstraints()

    # Todo:
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setRiskBudgetsConstraints =
function()
{ 
    # .setRiskBudgetsConstraints()    
    #   Utility function called by .setConstraints()
    
    # Todo:
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getConstraints =
function()
{ 
    # Arguments:
    # getConstraints(data, spec = portfolioSpec(), constraints = NULL, 
    #   type = c("BoxGroup", "RiskBudget"))
    
    # Data, Specification and Constraints:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Spec = portfolioSpec()
    Constraints = NULL
    
    # Set Default Box-Group Constraints:
    # These are: 0 <= W[1:nAssets] <= 1, no Group Constraints ...
    ans = .setConstraints(Data, Spec, Constraints)
    .getConstraints(ans)
    
    # Set Default Covariance Risk-Budget Constraints:
    ans = .setConstraints(Data, Spec, Constraints, type = "RiskBudget")  
    .getConstraints(ans)
    
    # Short:
    Constraints = "Short"
    ans = .setConstraints(Data, portfolioSpec(), Constraints)
    .getConstraints(ans)
    
    # Long Only:
    Constraints = "LongOnly"
    ans = .setConstraints(Data, portfolioSpec(), Constraints)
    .getConstraints(ans)
    
    # minW, maxW:
    Constraints = 
        c("minW[1:nAssets]=0.09", "maxW[1:nAssets]=rep(c(0.6, 0.4),4)")
    ans = .setConstraints(Data, portfolioSpec(), Constraints)
    .getConstraints(ans)
    
    # minsumW, maxsumW:
    Constraints = c("minsumW[c(2,4)]=0.20", "maxsumW[4:6]=0.80")
    ans = .setConstraints(Data, portfolioSpec(), Constraints)
    .getConstraints(ans)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2D.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

