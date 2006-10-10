
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
    # MSFT Opening Prices:
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    SRC = "ZivotWang/data/msft.dat.csv"
    DATA = paste(URL, SRC, sep = "") 
    download.file(DATA, destfile = "msft.dat.csv")
    msft.dat = readSeries("msft.dat.csv")
    msft = msft.dat[, 1]
    checkSum = 15349.9344
    checkEqualsNumeric(sum(msft@Data), checkSum)
    
    # MSFT Volume in Million Units:
    msft.vol = msft.dat[ , 5]/10^6
    checkSum = 10742.7319
    checkEqualsNumeric(sum(msft.vol@Data), checkSum)
    
    # MSFT Opening Returns:
    msft.ret = returnSeries(msft)
    checkSum = -0.2359905
    checkEqualsNumeric(sum(msft.ret@Data), checkSum)
    
    # LABELS = TRUE
    par(mfrow = c(4, 3), cex = 0.7)
    
    # acfPlot -
    acfPlot(x = msft.ret)
    
    # pacfPlot -
    pacfPlot(x = msft.ret)
    
    # ccfPlot -
    ccfPlot(x = msft.ret, y = msft.vol)
    
    # teffectPlot -
    teffectPlot(x = msft.ret)
    
    # lmacfPlot -
    lmacfPlot(x = abs(msft.ret), type = "acf")
    lmacfPlot(x = abs(msft.ret), type = "hurst")
    # ... CHECK ACF OF RETURNS
    
    # lmacfPlot -
    lacfPlot(x = msft, n = 4)

    # logpdfPlot -
    logpdfPlot(x = msft.ret, labels = FALSE)
    logpdfPlot(x = msft.ret, type = "log-log")
    # ... CHECK WARNINGS
    # ... CHECK COLORS
    
    # qqgaussPlot -
    qqgaussPlot(x = msft.ret)
    
    # scalinglawPlot -
    scalinglawPlot(x = msft.ret, span = 4)
    # ... CHECK COLORS
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plotNoLabels = 
function()
{ 
    # MSFT Opening Prices:
    # URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    # SRC = "ZivotWang/data/msft.dat.csv"
    # DATA = paste(URL, SRC, sep = "") 
    # download.file(DATA, destfile = "msft.dat.csv")
    # msft.dat = readSeries("msft.dat.csv")
    # ... or load it from Package fEcofin
    data(msft.dat)
    msft.dat = as.timeSeries(msft.dat)
    msft = msft.dat[, 1]
    checkSum = 15349.9344
    checkEqualsNumeric(sum(msft@Data), checkSum)
    
    # MSFT Volume in Million Units:
    msft.vol = msft.dat[ , 5]/10^6
    checkSum = 10742.7319
    checkEqualsNumeric(sum(msft.vol@Data), checkSum)
    
    # MSFT Opening Returns:
    msft.ret = returnSeries(msft)
    checkSum = -0.2359905
    checkEqualsNumeric(sum(msft.ret@Data), checkSum)
    
    # LABELS = FALSE
    par(mfrow = c(4, 3), cex = 0.7)
    
    # acfPlot -
    acfPlot(x = msft.ret, labels = FALSE)
    
    # pacfPlot -
    pacfPlot(x = msft.ret, labels = FALSE)
    
    # ccfPlot -
    ccfPlot(x = msft.ret, y = msft.vol, labels = FALSE)
    
    # teffectPlot -
    teffectPlot(x = msft.ret, labels = FALSE)
    
    # lmacfPlot -
    lmacfPlot(x = abs(msft.ret), type = "acf", labels = FALSE)
    lmacfPlot(x = abs(msft.ret), type = "hurst", labels = FALSE)
    # ... CHECK ACF OF RETURNS
    
    # lmacfPlot -
    lacfPlot(x = msft, n = 4, labels = FALSE)

    # logpdfPlot -
    logpdfPlot(x = msft.ret, labels = FALSE)
    logpdfPlot(x = msft.ret, type = "log-log", labels = FALSE)
    # ... CHECK WARNINGS
    # ... CHECK COLORS
    
    # qqgaussPlot -
    qqgaussPlot(x = msft.ret, labels = FALSE)
    
    # scalinglawPlot -
    scalinglawPlot(x = msft.ret, span = 4, labels = FALSE)
    # ... CHECK COLORS
    
    # Return Value:
    return()
}  


# ------------------------------------------------------------------------------  
    

if (FALSE) {
    require(RUnits)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit3B.R")
    printTextProtocol(testResult)
}


################################################################################

