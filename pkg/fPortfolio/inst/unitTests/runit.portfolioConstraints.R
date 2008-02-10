
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
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                    CONSTRAINTS:
#  portfolioConstraints         Checks Consistency of Constraints Strings               
################################################################################


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


test.portfolioConstraints.boxConstraints =
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
    constraints = c("minW[1:4]=0.1", "maxW[1:4]=c(rep(0.8, 3), 0.9)")
    constraints
   
    # Set Portfolio Constraints:
    portfolioConstraints(data, spec, constraints)
   
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstraints.boxgroupConstraints <- 
    function()
{
    # Data, Specification and Constraints:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Set Default Specifications:
    spec = portfolioSpec() 
    spec
    
    # Constraints:
    constraints = c(
        "minW[1:4]=runif(4, 0, 0.2)", "maxW[1:4]=0.9",
        "minsumW[1:2]=0.2", "maxsumW[3:4]=0.8")
    
    # Set Portfolio Constraints:
    portfolioConstraints(data, spec, constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstraints.riskBudgetsConstraints <- 
    function()
{
    # Data, Specification and Constraints:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Set Default Specifications:
    spec = portfolioSpec() 
    spec
    
    # Constraints:
    constraints = c("minB[1:4]=runif(4, 0, 0.2)", "maxB[1:4]=0.9")
    
    # Set Portfolio Constraints:
    portfolioConstraints(data, spec, constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstraints.allTypes <- 
    function()
{
    # Data, Specification and Constraints:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Set Default Specifications:
    spec = portfolioSpec() 
    spec
    
    # Constraints:
    constraints = c(
        "minW[1:4]=runif(4, 0, 0.2)", "maxW[1:4]=0.9",
        "minsumW[1:2]=0.2", "maxsumW[3:4]=0.8",
        "minB[1:4]=runif(4, 0, 0.2)", "maxB[1:4]=0.9")
    
    # Set Portfolio Constraints:
    portfolioConstraints(data, spec, constraints)
    
    # Return Value:
    return()
}


################################################################################

