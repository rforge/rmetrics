
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
# FUNCTION:                          SINGLE PORTFOLIOS:
#  .feasibleConstrainedMVPortfolio    Returns a constrained feasible MV-PF
#  .efficientConstrainedMVPortfolio   Returns a constrained frontier MV-PF
#  .cmlConstrainedMVPortfolio         Returns constrained CML-Portfolio
#  .tangencyConstrainedMVPortfolio    Returns constrained tangency MV-PF
#  .minvarianceConstrainedMVPortfolio Returns constrained min-Variance-PF
# FUNCTION:                          PORTFOLIO FRONTIER:
#  .portfolioConstrainedMVFrontier    Returns the EF of a constrained MV-PF                    
################################################################################


test.efficientConstrainedMVPortfolio.LongOnly = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(seriesData(data))
    spec
    
    # Efficient Portfolio:
    Portfolio = .efficientConstrainedMVPortfolio(data, spec, "LongOnly")
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio.Tailored = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(seriesData(data))
    spec
    
    # Consgtraints:
    constraints = "maxW[1:nAssets]=0.6"
    constraints
    
    # Efficient Portfolio:
    Portfolio = .efficientConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio.Rdonlp2 = 
function()
{
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setTargetReturn(spec) <- mean(seriesData(data))
        setSolver(spec) = "Rdonlp2"
        spec
        
        # Constraints:
        constraints = "maxW[1:nAssets]=0.6"
        constraints
        
        # Efficient Portfolio:
        Portfolio = .efficientConstrainedMVPortfolio(data, spec, constraints)
        Portfolio
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio.twoAssets = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(data@Data)
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Efficient Portfolio:
    Portfolio = .efficientConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio.RDonlp2 = 
function()
{
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setTargetReturn(spec) = mean(seriesData(data))
        setSolver(spec) = "Rdonlp2"
        spec
        
        # Tangency Portfolio:
        .efficientConstrainedMVPortfolio(data, spec, NULL)
        .efficientConstrainedMVPortfolio(data, spec, "LongOnly")
        .efficientConstrainedMVPortfolio(data, spec, "maxW[1:nAssets]=0.6")
    }
    
    # Return Value:
    return()
}


################################################################################


test.portfolioConstrainedMVFrontier =
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 5
    spec
    
    # Constraints: 
    constraints = NULL
    constraints
    
    # Portfolio Frontier:
    Frontier = .portfolioConstrainedMVFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedMVFrontier.RDonlp2 =
function()
{   
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setSolver(spec) = "Rdonlp2"
        setNFrontierPoints(spec) = 5
        spec
        
        # Constraints: 
        constraints = NULL                        
        constraints
        
        # Portfolio Frontier:
        Frontier = .portfolioConstrainedMVFrontier(data, spec, constraints)
        Frontier 
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedMVFrontier.RiskBudgets =
function()
{    
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setSolver(spec) = "Rdonlp2"
        setNFrontierPoints(spec) = 10
        spec
        
        # Add Risk Budgets:
        constraints = c("minW[1:4]=0", "maxB[1:4]=0.8")
        constraints
           
        # Frontier:
        Frontier = .portfolioConstrainedMVFrontier(data, spec, constraints)
        Frontier
    
    }
      
    # Return Value:
    return()
}


################################################################################

