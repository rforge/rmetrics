
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
#  test.getSpec.getModel  
#  test.getSpec.getPortfolio
#  test.getSpec.getOptim
################################################################################


test.getSpec.getModel <-  
    function()
{  
    # FUNCTION:            PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION:
    #  getModel             Extract model slot
    #   getType             Extract portfolio type from specification 
    #   getOptimize         Extract what to optimize from specification
    #   getEstimator        Extract type of covariance estimator
    #   getTailRisk         Extract list of tail dependency risk matrixes
    #   getParams           Extract parameters from specification
    #   getAlpha            Extracts target VaR-alpha specification

    # Specification:
    spec = portfolioSpec()
    
    # Get Slot:
    print(getModel(spec))
    print(names(getModel(spec)))
    
    # Get Model Entries:
    print(getType(spec))
    print(getOptimize(spec))
    print(getEstimator(spec))
    print(getTailRisk(spec))
    print(getParams(spec))
        
    # Model$Params:
    print(getAlpha(spec))
    # print(getA(spec)) ... is not yet implemented
 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getSpec.getPortfolio <-  
    function()
{  
    # FUNCTION:            PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION:
    #  getPortfolio         Extract portfolio slot
    #   getWeights          Extracts weights from a portfolio object
    #   getTargetReturn     Extracts target return from specification
    #   getTargetRisk       Extracts target riks from specification
    #   getRiskFreeRate     Extracts risk free rate from specification 
    #   getNFrontierPoints  Extracts number of frontier points 
    #   getStatus           Extracts portfolio status information

    # Specification:
    spec = portfolioSpec()
    
    # Get Slot:
    print(getPortfolio(spec))
    print(names(getPortfolio(spec)))
    
    # Get Portfolio Entries:                                
    print(getWeights(spec))            
    print(getTargetReturn(spec))       
    print(getTargetRisk(spec))      
    print(getRiskFreeRate(spec))       
    print(getNFrontierPoints(spec))    
    print(getStatus(spec))
 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getSpec.getOptim <-  
    function()
{  
    # FUNCTION:            PORTFOLIO S4 EXTRACTORS FROM SPECIFICATION:
    #  getOptim             Extract optim slot
    #   getSolver           Extracts solver from specification
    #   getTrace            Extracts solver's trace flag

    # Specification:
    spec = portfolioSpec()
    
    # Get Slot:
    print(getOptim(spec))
    print(names(getOptim(spec)))
                                    
    # Get Optim Entries:
    print(getSolver(spec))            
    print(getTrace(spec))       
 
    # Return Value:
    return()
}


################################################################################

