
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file 


################################################################################
# FUNCTION:             DESCRIPTION:
#  acfPlot               Displays autocorrelations function plot
#  pacfPlot              Displays partial autocorrelation function plot
#  ccfPlot               Cross correlation function plot
#  teffectPlot           Estimates and plots the Taylor effect
#  lmacfPlot             Estimates and plots the long memory ACF
#  lacfPlot              Plots lagged autocorrelations
#  logpdfPlot            Returns a pdf plot on logarithmic scale(s)
#  qqgaussPlot           Returns a Gaussian quantile-quantile plot
#  scalinglawPlot        Evaluates and plots scaling law behavior
################################################################################
    

test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(StylizedFacts); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.plotLabels = 
function()
{
    # data - 
    data(DowJones30)
    IBM = DowJones30[, "IBM"]
    JPM = DowJones30[, "JPM"]
    IBM.RET = diff(log(IBM))
    JPM.RET = diff(log(JPM))
    checkIdentical(
        target = class(IBM),
        current = "numeric")
    
    # LABELS = TRUE
    
    # acfPlot -
    par(mfrow = c(4, 3), cex = 0.5)
    acfPlot(x = IBM.RET)
    
    # pacfPlot -
    pacfPlot(x = IBM.RET)
    
    # ccfPlot -
    ccfPlot(x = IBM.RET, y = JPM.RET)
    
    # teffectPlot -
    teffectPlot(x = IBM.RET)
    
    # lmacfPlot -
    lmacfPlot(x = abs(IBM.RET))
    # ... CHECK ACF OF RETURNS
    
    # lmacfPlot -
    lacfPlot(x = IBM)

    # logpdfPlot -
    logpdfPlot(x = IBM.RET)
    logpdfPlot(x = IBM.RET, type = "log-log")
    # ... CHECK WARNINGS
    # ... CHECK COLORS
    
    # qqgaussPlot -
    qqgaussPlot(x = IBM.RET)
    
    # scalinglawPlot -
    scalinglawPlot(x = IBM.RET)
    # ... CHECK COLORS
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plotNoLabels = 
function()
{
    # data - 
    data(DowJones30)
    IBM = DowJones30[, "IBM"]
    JPM = DowJones30[, "JPM"]
    IBM.RET = diff(log(IBM))
    JPM.RET = diff(log(JPM))
    checkIdentical(
        target = class(IBM),
        current = "numeric")
    
    # LABELS = FALSE
    
    # acfPlot -
    par(mfrow = c(4, 3), cex = 0.5)
    acfPlot(x = IBM.RET, labels = FALSE)
    
    # pacfPlot -
    pacfPlot(x = IBM.RET, labels = FALSE)
    
    # ccfPlot -
    ccfPlot(x = IBM.RET, y = JPM.RET, labels = FALSE)
    
    # teffectPlot -
    teffectPlot(x = IBM.RET, labels = FALSE)
    
    # lmacfPlot -
    lmacfPlot(x = abs(IBM.RET), type = "acf", labels = FALSE)
    lmacfPlot(x = abs(IBM.RET), type = "hurst", labels = FALSE)
    # ... CHECK ACF OF RETURNS
    
    # lmacfPlot -
    lacfPlot(x = IBM, labels = FALSE)

    # logpdfPlot -
    logpdfPlot(x = IBM.RET, labels = FALSE)
    logpdfPlot(x = IBM.RET, type = "log-log", labels = FALSE)
    # ... CHECK WARNINGS
    # ... CHECK COLORS
    
    # qqgaussPlot -
    qqgaussPlot(x = IBM.RET, labels = FALSE)
    
    # scalinglawPlot -
    scalinglawPlot(x = IBM.RET, labels = FALSE)
    # ... CHECK COLORS
    
    # Return Value:
    return()
}  


# ------------------------------------------------------------------------------  
    

if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fBasics/test/runit014A.R")
    printTextProtocol(testResult)
}


################################################################################

