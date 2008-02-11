
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Founbdattion, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:
################################################################################


test.feasiblePortfolio.ConstrainedMV = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
 
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Feasible Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
   
 
test.feasiblePortfolio.RandomWeights.LongOnly = 
function()
{   
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification + Random Weights:
    spec = portfolioSpec()
    nAssets = ncol(data)
    Weights = runif(nAssets, 0, 1)
    Weights = Weights/sum(Weights)
    setWeights(spec) <- Weights
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Feasible Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasiblePortfolio.MV.Short = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification - Equal Weights Portfolio:
    spec = portfolioSpec()
    spec
    
    # Constraints are ignored:
    constraints = "Short"
    constraints
    
    # Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasibleShortMVPortfolio.Short = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification - Equal Weights Portfolio:
    spec = portfolioSpec()
    spec
    
    # Constraints can natursally also defined as:
    constraints = "Short"
    constraints
    
    # Portfolio:
    Portfolio = .feasibleShortMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}




################################################################################





test.feasiblePortfolio.CVaR.LongOnly.Alpha =
function()
{   
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setWeights(spec) = rep(1/4, 4)
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}

