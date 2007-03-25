
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                POT SERIES SIMULATION:
#  potSim                   Peaks over a threshold from arbitrary series
# FUNCTION:                POT PARAMETER ESTIMATION:
#  'fPOTFIT'                S4 Class Representation
#  potFit                   Fits with POT method
#   print.fPOTFIT            Print Method for object of class "fPOT"
#   plot.fPOTFIT             Print Method for object of class "fPOT"
#   summary.fPOTFIT          Summary Method for object of class "fPOT"
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(DaylightSavingTime, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.potSim = 
function()
{
    # potSim:
    X = as.timeSeries(data(danishClaims))
    x = potSim(X, u = quantile(x, 0.95), run = 1)
    print(class(x))
    par(mfrow = c(2,2), cex = 0.7)
    seriesPlot(x)
    
    # Decluster the Process:
    # ... deCluster(x, run = 20, doplot = TRUE) 
    x = potSim(X, u = quantile(x, 0.95), run = 5)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.fPOTFIT = 
function()
{
    # Slot Names:
    slotNames("fGPDFIT")
    # [1] "call"        "method"      "parameter"   "data"        "fit"        
    # [6] "residuals"   "title"       "description"

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.potFit = 
function()
{
    # Load Data:
    x = as.timeSeries(data(danishClaims))
    
    # Parameter Estimation:
    # potFit(x, u = quantile(x,0.95), run = 1, title = NULL, description = NULL)
    fit = potFit(x)
    print(fit)
    
    # Decluster:
    fit = potFit(x, run = 10)
    print(fit)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.plot = 
function()
{
    # Load Data:
    x = as.timeSeries(data(danishClaims))
    
    # Parameter Estimation with Declustering:
    # potFit(x, u = quantile(x,0.95), run = 1, title = NULL, description = NULL)
    fit = potFit(x, u = 10, run = 10)
    print(fit)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    plot(fit, which = 1:4)
    plot(fit, which = 5:7)
    
    # Try Interactive:
    # plot(fit)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.summary = 
function()
{
    # Summary Report:
    # summary(object, doplot = TRUE, which = "all", ...)
    
    # Load Data:
    x = as.timeSeries(data(danishClaims))
    
    # Parameter Estimation with Declustering:
    # potFit(x, u = quantile(x,0.95), run = 1, title = NULL, description = NULL)
    fit = potFit(x, u = 10)
    print(fit)
    
    # Summary:
    summary(fit, doplot = FALSE)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fExtremes/test/runit4B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}


################################################################################

