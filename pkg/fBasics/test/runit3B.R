
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
    # Artificial Data Set:
    set.seed(4711)
    S = sample(1:12)
    V = sample(rep(1:3, 4)) * 10000
    matX = cbind(Open = S, High = S+1, Low = S-1, Close = S, Volume = V )
    X = timeSeries(matX, timeCalendar())
    print(X)
    
    # X Opening Prices:
    x = X[, 1]
    sum(x@Data)
    checkSum = 78
    checkEqualsNumeric(sum(x@Data), checksum)
    
    # X Volume in Thousand Units:
    volX = X[, 5]/1000
    sum(volX@Data)
    checkSum = 240
    checkEqualsNumeric(sum(volX@Data), checkSum)
    
    # MSFT Opening Returns:
    retX = returnSeries(X[, 1])
    sum(retX@Data)
    checkSum = -1.099
    checkEqualsNumeric(sum(retX@Data), checkSum)
    
    # LABELS = TRUE
    
    # acfPlot -
    ans = acfPlot(x = retX)
    checkSum = 0.5 
    checkEqualsNumeric(sum(ans$acf), checkSum)
    
    # pacfPlot -
    pacfPlot(x = retX)
    
    # ccfPlot -
    ans = ccfPlot(x = retX, y = volX[-1])
    checkSum = 0 
    checkEqualsNumeric(sum(ans$acf), checkSum)
    
    # teffectPlot -
    ans = teffectPlot(x = retX)
    checkSum = -7.5
    checkEqualsNumeric(sum(ans), checkSum)
    
    # lmacfPlot -
    # ans = lmacfPlot(x = abs(retX))
    # ans
    
    # lmacfPlot -
    # ans = lacfPlot(x = retX, n = 4)
    # ans

    # logpdfPlot -
    # ans = logpdfPlot(x = retX)
    # ans
    # ans = logpdfPlot(x = retX, type = "log-log")
    # ans
    # ... CHECK WARNINGS
    # ... CHECK COLORS
    
    # qqgaussPlot -
    ans = qqgaussPlot(x = retX)
    ans
    
    # scalinglawPlot -
    # ans = scalinglawPlot(x = retX, span = 4)
    # ans
    # ... CHECK COLORS
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plotNoLabels = 
function()
{
    
    # MSFT Opening Prices:
    msft = as.timeSeries(MSFT)[, 1]
    checkSum = 15349.9344
    checkEqualsNumeric(sum(msft@Data), checkSum)
    
    # MSFT Volume in Million Units:
    msft.vol = as.timeSeries(MSFT)[, 5]/10^6
    checkSum = 10742.7319
    checkEqualsNumeric(sum(msft.vol@Data), checkSum)
    
    # MSFT Opening Returns:
    msft.ret = returnSeries(msft)
    checkSum = -0.2359905
    checkEqualsNumeric(sum(msft.ret@Data), checkSum)
    
    # LABELS = FALSE
    
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

