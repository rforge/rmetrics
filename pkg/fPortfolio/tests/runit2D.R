
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
   data = as.timeSeries(data(smallcap.ts))
   data = data[, c("BKE", "GG", "GYMB", "KRON")]
   head(data)
   
   # Set Default Specifications:
   spec = portfolioSpec() 
   spec
   
   # Constraints:
   constraints = NULL
   constraints
   
   # Set Portfolio Constraints - Returns NULL:
   portfolioConstraints(data, spec, constraints)
   
   # Return Value:
   return()
}


# ------------------------------------------------------------------------------


test.portfolioConstraints.LongOnly =
function()
{    
   # Load Data:
   data = as.timeSeries(data(smallcap.ts))
   data = data[, c("BKE", "GG", "GYMB", "KRON")]
   head(data)
   
   # Set Default Specifications:
   spec = portfolioSpec() 
   spec
   
   # Constraints:
   constraints = "LongOnly"
   constraints
   
   # Set Portfolio Constraints:
   portfolioConstraints(data, spec, constraints)
   
   # Return Value:
   return()
}
     
   
# ------------------------------------------------------------------------------


test.portfolioConstraints.Short =
function()
{    
   # Load Data:
   data = as.timeSeries(data(smallcap.ts))
   data = data[, c("BKE", "GG", "GYMB", "KRON")]
   head(data)
   
   # Set Default Specifications:
   spec = portfolioSpec() 
   spec
   
   # Constraints:
   constraints = "Short"
   constraints
   
   # Set Portfolio Constraints:
   portfolioConstraints(data, spec, constraints)
   
   # Return Value:
   return()
}


# ------------------------------------------------------------------------------


test.portfolioConstraints.tailored =
function()
{    
   # Load Data:
   data = as.timeSeries(data(smallcap.ts))
   data = data[, c("BKE", "GG", "GYMB", "KRON")]
   head(data)
   
   # Set Default Specifications:
   spec = portfolioSpec() 
   spec
   
   # Constraints:
   constraints = c("minW[1:4]=0", "maxW[1:4]=1")
   constraints
   
   # Set Portfolio Constraints:
   portfolioConstraints(data, spec, constraints)
   
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
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    spec = portfolioSpec()
    constraints = NULL
    
    # Set Default Box-Group Constraints:
    # These are: 0 <= W[1:nAssets] <= 1, no Group Constraints ...
    .setConstraints(data, spec, constraints)
    
    # Set Default Covariance Risk-Budget Constraints:
    # These are: 0 <= RiskBudgets[1:nAssets] <= 1, 
    #   no RiskBudget Group Constraints ...
    # Note,  Risk-Budget Constraints have to been added explicitely!
    .setConstraints(data, spec, constraints, type = "RiskBudget")  
    
    # Short:
    constraints = "Short"
    .setConstraints(data, portfolioSpec(), constraints)
    
    # Long Only:
    constraints = "LongOnly"
    .setConstraints(data, portfolioSpec(), constraints)
    
    # Tailored - minW, maxW:
    constraints = 
        c("minW[1:nAssets]=0.09", "maxW[1:nAssets]=rep(c(0.6, 0.4),2)")
    .setConstraints(data, portfolioSpec(), constraints)
    
    # Tailored - minsumW, maxsumW:
    constraints = c("minsumW[c(2,4)]=0.20", "maxsumW[3:4]=0.80")
    .setConstraints(data, portfolioSpec(), constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setBoxGroupConstraints = 
function()
{
    # Data, Specification and Constraints:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    spec = portfolioSpec()
    constraints = c(
        "minW[1:4]=runif(4, 0, 0.2)", "maxW[1:4]=0.9",
        "minsumW[1:2]=0.2", "maxsumW[3:4]=0.8")
    
    # Set Constraints:
    .setBoxGroupConstraints(data, spec, constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setRiskBudgetsConstraints =
function()
{
    # Data, Specification and Constraints:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    spec = portfolioSpec()
    constraints = c("minB[1:4]=runif(4, 0, 0.2)", "maxB[1:4]=0.9")
    
    # Set Constraints:
    .setRiskBudgetsConstraints(data, spec, constraints)
    
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
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    spec = portfolioSpec()
    constraints = NULL
    
    # Set Default Box-Group Constraints:
    # These are: 0 <= W[1:nAssets] <= 1, no Group Constraints ...
    ans = .setConstraints(data, spec, constraints)
    .getConstraints(ans)
    
    # Set Default Covariance Risk-Budget Constraints:
    ans = .setConstraints(data, spec, constraints, type = "RiskBudget")  
    # .getConstraints(ans)                                       # CHECK !!!
    
    # Short:
    constraints = "Short"
    ans = .setConstraints(data, portfolioSpec(), constraints)    # CHECK !!!
    .getConstraints(ans)
    
    # Long Only:
    constraints = "LongOnly"
    ans = .setConstraints(data, portfolioSpec(), constraints)
    .getConstraints(ans)
    
    # minW, maxW:
    constraints = 
        c("minW[1:nAssets]=0.09", "maxW[1:nAssets]=rep(c(0.6, 0.4),2)")
    ans = .setConstraints(data, portfolioSpec(), constraints)    # CHECK !!!
    .getConstraints(ans)
    
    # minsumW, maxsumW:
    constraints = c("minsumW[c(2,4)]=0.20", "maxsumW[3:4]=0.80")
    ans = .setConstraints(data, portfolioSpec(), constraints)    # CHECK !!!
    .getConstraints(ans)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/tests/runit2D.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

