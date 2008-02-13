

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
# FUNCTION:                    DESCRIPTION:   
#  solveRQuadprog               Calls Goldfarb and Idnani's QP solver                  
################################################################################

    
test.solveRquadprog <- 
    function()
{ 
    # Quadratic Programmming - Mean-Variance Portfolio:
    #   The target return is fixed, we minimize the risk!

    # Direct Access:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(as.matrix(data))
    spec
    
    # Default Constraints:
    constraints = "LongOnly"
    constraints
    
    # Quadprog:
    solveRquadprog(data, spec, constraints)
    
    # Should give the same results ...
    setTargetReturn(spec) = 10*getTargetReturn(spec)
    solveRquadprog(10*data, spec, constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solveRquadprog.twoAssets <- 
    function()
{ 
    # Solved Analytically
    #   Speeds up the two-assets forntier significantly!

    # Direct Access:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(as.matrix(data))   
    spec
    
    # Default Constraints:
    constraints = "LongOnly"
    constraints
    
    # Quadprog:
    solveRquadprog(data, spec, constraints) 
    
    # Return Value:
    return()
}


################################################################################

