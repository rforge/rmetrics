
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
# FUNCTION:             SIMULATION AND PARAMETER ESTIMATION:
#  'fASSETS'             Class representation for "fASSETS" Objects
#  assetsSim             Simulates a set of artificial assets
#  assetsFit             Estimates the parameters of set of assets
#   method = "norm"       assuming a multivariate Normal distribution
#   method = "snorm"      assuming a multivariate skew-Normal distribution
#   method = "st"         assuming a multivariate skew-Student-t 
# FUNCTION:             PRINT, PLOT AND SUMMARY METHOD: 
#  show.fASSETS          S4: Print method for an object of class fASSETS
#  plot.fASSETS          S3: Plot method for an object of class fASSETS
#  summary.fASSETS       S3: Summary method for an object of class fASSETS
# FUNCTION:             REQUIRED UTILITY FUNCTION:
#  .msn.quantities       Function from R package sn [part of fMultivar]                         
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(AssetsModelling, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.assetsSim =
function()
{ 
    # assetsSim(n, dim=2, model = 
    #   list(mu=rep(0, dim), Omega=diag(dim), alpha=rep(0, dim), df=Inf), 
    #   assetNames = NULL) 
    
    # Normel Assets:
    assetsSim(n = 10, dim = 3)
    assetsSim(n = 10, dim = 3,
        list(mu=rep(0, 3), Omega=diag(3), alpha=rep(0, 3), df=Inf))
    
    # Skew Normal Assets:
    assetsSim(n = 10, dim = 3, model = 
        list(mu=rep(0, 3), Omega=diag(3), alpha=rep(0.1, 3), df=Inf))
        
    # Student-t Assets:
    assetsSim(n = 10, dim = 3, model = 
        list(mu=rep(0, 3), Omega=diag(3), alpha=rep(0, 3), df=4))
        
    # Skew Student-t Assets:
    assetsSim(n = 10, dim = 3, model = 
        list(mu=rep(0, 3), Omega=diag(3), alpha=rep(0.1, 3), df=4))
        
    # Add Asset Names:
    assetsSim(n = 10, dim = 3, assetNames = c("A", "B", "C"))

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsFit =
function()
{ 
    # function (x, method = c("st", "snorm", "norm"), title = NULL, 
    #   description = NULL, fixed.df = NA, ...) 

    # Normal Assets:
    x = assetsSim(n = 1000, dim = 3)
    fit = assetsFit(x, "norm")
    fit
    
    # Skew Normal Assets:
    x = assetsSim(n = 1000, dim = 3, model = 
        list(mu=rep(0, 3), Omega=diag(3), alpha=c(-0.5, 0 , 0.5), df=Inf))
    fit = assetsFit(x, "snorm")
    fit
    
    # Student-t Assets:
    x = assetsSim(n = 1000, dim = 3, model = 
        list(mu=rep(0, 3), Omega=diag(3), alpha=rep(0, 3), df=4))    
    fit = assetsFit(x, "st")
    fit
    
    # Student-t Assets - Fixed df:
    x = assetsSim(n = 1000, dim = 3, model = 
        list(mu=rep(0, 3), Omega=diag(3), alpha=rep(0, 3), df=4))    
    fit = assetsFit(x, "st", fixed.df = 4)
    fit
    
    par(ask = FALSE)
    class(fit)
    print(fit)
    plot(fit)                                                    # CHECK X-Label
    summary(fit, doplot = FALSE)                            # CHECK - add doplot
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit1A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

