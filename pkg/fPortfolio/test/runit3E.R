
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
# FUNCTION:                              SINGLE PORTFOLIOS:
#  .feasibleConstrainedCVaRPortfolio      Returns a feasible M-CVaR PF
#  .cmlConstrainedCVaRPortfolio           Returns a CML M-CVaR PF
#  .tangencyConstrainedCVaRPortfolio      Returns the tangency M-CVaR PF
#  .minvarianceConstrainedCVaRPortfolio   Returns the minimum var M-CVaR PF
#  .frontierConstrainedCVaRPortfolio      Returns a frontier M-CVaR PF
# FUNCTION:                              PORTFOLIO FRONTIER:
#  .portfolioConstrainedCVaRFrontier      Returns the EF of a  M-CVaR PF
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ConstrainedCVaRPortfolio, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.CVaRPortfolio = 
function()
{  
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = mean(colAvgs(Data))
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # CVaR Portfolio Optimization:
    ans = efficientPortfolio(Data, Spec)
    weights = round(getWeights(ans), 3)
    weights
    
    # Compare with:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    Spec
    ans = efficientPortfolio(Data, Spec)
    weights = round(getWeights(ans), 3)
    weights
    
    # Return Value:
    return()    
}
  

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit3E.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################