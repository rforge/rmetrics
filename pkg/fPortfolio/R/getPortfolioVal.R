
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA


################################################################################
# FUNCTION:
#  getPortfolio 
# FUNCTION:
#  getWeights 
#  getCovRiskBudgets 
#  getTargetReturn 
#  getTargetRisk 
#  getAlpha 
#  getRiskFreeRate 
#  getNFrontierPoints 
#  getStatus 
################################################################################
   
    
getPortfolio.fPFOLIOVAL <- 
    function(object) object@portfolio

getWeights.fPFOLIOVAL <- 
    function(object) object@portfolio$weights
getCovRiskBudgets.fPFOLIOVAL <- 
    function(object) object@portfolio$covRiskBudgets
getTargetReturn.fPFOLIOVAL <- 
    function(object) object@portfolio$targetReturn
getTargetRisk.fPFOLIOVAL <- 
    function(object) object@portfolio$targetRisk

getAlpha.fPFOLIOVAL <- 
    function(object) object@portfolio$targetAlpha

getRiskFreeRate.fPFOLIOVAL <- 
    function(object) object@Portfolio$riskFreeRate
getNFrontierPoints.fPFOLIOVAL <- 
    function(object) object@portfolio$nFrontierPoints

getStatus.fPFOLIOVAL <- 
    function(object) object@portfolio$status


################################################################################

