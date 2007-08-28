
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
# FUNCTION:                         GUMBEL COPULA:
#  .rgumbelCopula
#  .dgumbelCopula
#  .pgumbelCopula
# FUNCTION:                         MIXED GUMBEL-SURVIVALGUMBEL-NORMAL COPULA:
#  .rgsgnormCopula
#  .dgsgnormCopula
#  .gsgnormCopulaFit
# FUNCTION:                         NON-PARAMETRIC TAIL DEPENDECY ESTIMATOR:
#  .cfgTDE
# FUNCTION:                         COPULA FIT WITH NIG MARGINALS:
#  .nigDependencyFit                 
################################################################################



################################################################################


test.mixedCopula =
function()
{
    require(fCopulae)
    
    # Simulated data:
    x = .rnormCopula(1000, rho = 0.7)
    .gsgnormCopulaFit(x, trace = TRUE)
    
    # LPP Portfolio:
    require(fPortfolio)
    data(LPP2005REC)
    x = 100 * as.timeSeries(LPP2005REC)[, 1:6]
    head(x)
    
    # Tail Dependency Estimation:
    par(mfrow = c(2,2), cex = 0.7)   
    ans = .nigDependencyFit(x) 
    ans 
    
    #         Lower Upper
    # SBI SPI 0     0 
    # SBI SII 0.055 0 
    # SBI LMI 0.064 0.069 
    # SBI MPI 0     0 
    # SBI ALT 0     0 
    # SPI SII 0     0.064 
    # SPI LMI 0     0.072 
    # SPI MPI 0.352 0.214 
    # SPI ALT 0.273 0.048 
    # SII LMI 0.075 0 
    # SII MPI 0     0.164 
    # SII ALT 0     0.152 
    # LMI MPI 0     0 
    # LMI ALT 0     0 
    # MPI ALT 0.124 0.012 
    # 
    # $lower
    #            SBI       SPI        SII        LMI       MPI       ALT
    # SBI 0.00000000 0.0000000 0.05524575 0.06369211 0.0000000 0.0000000
    # SPI 0.00000000 0.0000000 0.00000000 0.00000000 0.3517273 0.2728653
    # SII 0.05524575 0.0000000 0.00000000 0.07541669 0.0000000 0.0000000
    # LMI 0.06369211 0.0000000 0.07541669 0.00000000 0.0000000 0.0000000
    # MPI 0.00000000 0.3517273 0.00000000 0.00000000 0.0000000 0.1236074
    # ALT 0.00000000 0.2728653 0.00000000 0.00000000 0.1236074 0.0000000
    # 
    # $upper
    #            SBI        SPI       SII        LMI        MPI        ALT
    # SBI 0.00000000 0.00000000 0.0000000 0.06935723 0.00000000 0.00000000
    # SPI 0.00000000 0.00000000 0.0638653 0.07169038 0.21421052 0.04785965
    # SII 0.00000000 0.06386530 0.0000000 0.00000000 0.16401986 0.15228270
    # LMI 0.06935723 0.07169038 0.0000000 0.00000000 0.00000000 0.00000000
    # MPI 0.00000000 0.21421052 0.1640199 0.00000000 0.00000000 0.01209013
    # ALT 0.00000000 0.04785965 0.1522827 0.00000000 0.01209013 0.00000000

    par(mfrow = c(1,1))
    .assetsStarPlot(ans$lower, main = "Lower Tail Relations")
    .assetsStarPlot(ans$upper, main = "Lower Tail Relations")

    # Return Value:
    return()
}


################################################################################


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCopulae/tests/runit6A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
 
  
################################################################################

