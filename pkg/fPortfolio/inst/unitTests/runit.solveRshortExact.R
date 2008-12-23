
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                     
#  test.solveRshortExact.MV.Short.MinRisk
#  test.solveRshortExact.MV.Short.MaxReturn               
################################################################################


test.solveRshortExact.MV.Short.MinRisk <- 
    function()
{ 
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(as.matrix(data))
    setSolver(spec) = "solveRshortExact"
    setTrace(spec) = TRUE
    print(spec)
    
    # Default Constraints:
    constraints = "Short"
    print(constraints)
 
    # Optimization:
    portfolio = solveRshortExact(data, spec, constraints)
    print(portfolio)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasiblePortfolio.MV.Short <- 
    function()
{ 
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Specification:
    spec = portfolioSpec()
    setWeights(spec) = rep(1/4, 4)
    setSolver(spec) = "solveRshortExact"
    print(spec)
    
    # Default Constraints:
    constraints = "Short"
    print(constraints)
 
    # Optimization:
    portfolio = feasiblePortfolio(data, spec, constraints)
    print(portfolio)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyPortfolio.MV.Short <- 
    function()
{  
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Specification:
    spec = portfolioSpec()
    setSolver(spec) = "solveRshortExact"
    print(spec)
    
    # Default Constraints:
    constraints = "Short"
    print(constraints)
 
    # Optimization:
    portfolio = tangencyPortfolio(data, spec, constraints)
    print(portfolio)
    
    
    # TWO ASSETS TEST:
    
    
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "KRON")]
    print(head(data))
    
    # Specification:
    spec = portfolioSpec()
    setSolver(spec) = "solveRshortExact"
    print(spec)
    
    # Default Constraints:
    constraints = "Short"
    print(constraints)
 
    # Optimization:
    portfolio = tangencyPortfolio(data, spec, constraints)
    print(portfolio)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio.MV.Short <- 
    function()
{    
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Specification:
    spec = portfolioSpec()
    setSolver(spec) = "solveRshortExact"
    print(spec)
    
    # Default Constraints:
    constraints = "Short"
    print(constraints)
 
    # Optimization:
    portfolio = minvariancePortfolio(data, spec, constraints)
    print(portfolio)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solveRshortExact.MV.Short.MaxReturn <- 
    function()
{ 
    # Given Target Risk Maximize Return Return
    
    # Not yet implemented:
    NA
    
    # Return Value:
    return()
}


################################################################################

