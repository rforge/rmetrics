
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
#  test.portfolioConstraints.LongOnly
#  test.portfolioConstraints.Short   
#  test.portfolioConstraints.boxConstraints   
#  test.portfolioConstraints.boxgroupConstraints 
#  test.portfolioConstraints.riskBudgetsConstraints 
#  test.portfolioConstraints.allTypes   
################################################################################


test.portfolioConstraints.LongOnly <- 
    function()
{ 
    # Load Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Set Default Specifications:
    spec = portfolioSpec() 
    setTargetReturn(spec) = mean(data)
    print(spec)
    
    # Constraints:
    constraints = "LongOnly"
    print(constraints)
    
    # Set Portfolio Constraints:
    Constraints = portfolioConstraints(data, spec, constraints)
    Constraints
    
    # Return Value:
    return()
}
     
   
# ------------------------------------------------------------------------------


test.portfolioConstraints.Short <- 
    function()
{    
    # Load Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Set Default Specifications:
    spec = portfolioSpec() 
    setTargetReturn(spec) = mean(data)
    print(spec)
    
    # Constraints:
    constraints = "Short"
    print(constraints)
    
    # Set Portfolio Constraints:
    Constraints = portfolioConstraints(data, spec, constraints)
    Constraints
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstraints.boxConstraints <- 
    function()
{    
    # Load Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications:
    spec = portfolioSpec() 
    setTargetReturn(spec) = mean(data)
    spec
   
    # Constraints:
    constraints = c("minW[1:4]=0.1", "maxW[1:4]=c(rep(0.8, 3), 0.9)")
    constraints
   
    # Set Portfolio Constraints:
    Constraints = portfolioConstraints(data, spec, constraints)
    Constraints
   
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstraints.boxgroupConstraints <- 
    function()
{
    # Data, Specification and Constraints:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Set Default Specifications:
    spec = portfolioSpec() 
    setTargetReturn(spec) = mean(data)
    spec
    
    # Constraints:
    constraints = c(
        "minW[1:4]=runif(4, 0, 0.2)", "maxW[1:4]=0.9",
        "minsumW[1:2]=0.2", "maxsumW[3:4]=0.8")
    
    # Set Portfolio Constraints:
    Constraints = portfolioConstraints(data, spec, constraints)
    Constraints
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstraints.riskBudgetsConstraints <- 
    function()
{
    # Data, Specification and Constraints:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Set Default Specifications:
    spec = portfolioSpec() 
    setTargetReturn(spec) = mean(data)
    spec
    
    # Constraints:
    constraints = c("minB[1:4]=runif(4, 0, 0.2)", "maxB[1:4]=0.9")
    
    # Set Portfolio Constraints:
    Constraints = portfolioConstraints(data, spec, constraints)
    Constraints
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstraints.allTypes <- 
    function()
{
    # Data, Specification and Constraints:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Set Default Specifications:
    spec = portfolioSpec() 
    setTargetReturn(spec) = mean(data)
    spec
    
    # Constraints:
    constraints = c(
        "minW[1:4]=runif(4, 0, 0.2)", "maxW[1:4]=0.9",
        "minsumW[1:2]=0.2", "maxsumW[3:4]=0.8",
        "minB[1:4]=runif(4, 0, 0.2)", "maxB[1:4]=0.9")
    
    # Set Portfolio Constraints:
    Constraints = portfolioConstraints(data, spec, constraints)
    Constraints
    
    # Return Value:
    return()
}


################################################################################

