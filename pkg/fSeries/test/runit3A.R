
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
# PART I: 
# FUNCTIONS:          FRACTIONAL BROWNIAN MOTION:
#  fbmSim              Generates fractional Brownian motion
#  fgnSim              Generates fractional Gaussian noise
#  farimaSim           Generates FARIMA time series process
################################################################################


################################################################################
# PART II: Reimplemented functions from Beran's SPlus Scripts
# FUNCTIONS:          DESCRIPTION:
#  whittleFit          Whittle estimator
################################################################################


################################################################################
# PART III: Reimplemented functions from Taqqu M.S, Teverovsky V, Willinger W.
# FUNCTIONS:          HURST EXPONENT:
#  fHURST              S4 Class Representation
#   print.fHURST        S3 Print Method
#   plot.fHURST         S3 Plot Method
#  aggvarFit           3.1 Aggregated variance method
#  diffvarFit          3.2 Differenced aggregated variance method
#  absvalFit           3.3 Absolute values (moments) method
#  higuchiFit          3.4 Higuchi's method
#  pengFit             3.5 peng's or Residuals of Regression method
#  rsFit               3.6 R/S method
#  perFit              3.7 Periodogram and cumulated periodogram method
#  boxperFit           3.8 Boxed (modified) peridogram method
#  whittleFit          3.9 Whittle estimator -> PART II
#  hurstSlider         Hurst Slider [1-7]
################################################################################


################################################################################
# PART IV: Wavelet Estimator
# FUNCTIONS:          DESCRIPTION:
#  waveletFit          Wavelet Estimator
################################################################################


################################################################################
# PART V: Statistical Tests and Slider
# FUNCTIONS:          DESCRIPTION:
#  .beranTest          Not yet ready for usage ...
#  .rsTest             Not yet ready for usage ...
#  .vsTest             Not yet ready for usage ...
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(LongRangeDependence, ask = FALSE); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.fbmSim = 
function()
{  
    # Simulation:
    
    # Try: 
    #   .fbmSimSlider()
    
    # fbmSim(
    #   n = 100, H = 0.7, 
    #   method = c("mvn", "chol", "lev", "circ", "wave"), 
    #   waveJ = 7, 
    #   doplot = TRUE, fgn = FALSE) 

    # Graphics Frame:
    par(mfrow = c(3, 2), cex = 0.7)
    
    x = fbmSim(n = 50, method = "mvn",  seed = 4711)
    print(x)
    target = round(mean(x), 2)
    print(target)
    checkSum = +0.08
    checkEqualsNumeric(target, checkSum)
    
    x = fbmSim(n = 50, method = "chol", seed = 4711)
    print(x)
    target = round(mean(x), 2)
    print(target)
    checkSum = +0.31
    checkEqualsNumeric(target, checkSum)
    
    x = fbmSim(n = 50, method = "lev",  seed = 4711)
    print(x)
    target = round(mean(x), 2)
    print(target)
    checkSum = -0.01
    checkEqualsNumeric(target, checkSum)
    
    x = fbmSim(n = 50, method = "circ", seed = 4711)
    print(x)
    target = round(mean(x), 2)
    print(target)
    checkSum = +0.94
    checkEqualsNumeric(target, checkSum)
    
    x = fbmSim(n = 50, method = "wave", seed = 4711)
    print(x)
    target = round(mean(x), 2)
    print(target)
    checkSum = -0.32
    checkEqualsNumeric(target, checkSum)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.fgnSim = 
function()
{  
    # Simulation:
    
    # Try: 
    #   .fgnSimSlider()
    
    # fbmSim(
    #   n = 100, H = 0.7, 
    #   method = c("mvn", "chol", "lev", "circ", "wave"), 
    #   waveJ = 7, doplot = TRUE, fgn = FALSE) 

    # Graphics Frame:
    par(mfrow = c(3, 2), cex = 0.7)
    
    x = fbmSim(n = 50, method = "mvn",  seed = 4711, fgn = TRUE)
    print(x)
    target = round(var(x), 2)
    print(target)
    checkSum = 0.01
    checkEquals(target, checkSum)
    
    x = fbmSim(n = 50, method = "chol", seed = 4711, fgn = TRUE)
    print(x)
    target = round(var(x), 2)
    print(target)
    checkSum = 0.0
    checkEquals(target, checkSum)
    
    x = fbmSim(n = 50, method = "lev",  seed = 4711, fgn = TRUE)
    print(x)
    target = round(var(x), 2)
    print(target)
    checkSum = 0.0
    checkEquals(target, checkSum)
    
    x = fbmSim(n = 50, method = "circ", seed = 4711, fgn = TRUE)
    print(x)
    target = round(var(x), 2)
    print(target)
    checkSum = +0.94
    checkEquals(target, checkSum)
    
    x = fbmSim(n = 50, method = "wave", seed = 4711, fgn = TRUE)
    print(x)
    target = round(var(x), 2)
    print(target)
    checkSum = 0.0
    checkEquals(target, checkSum)
    
    # Return Value:
    return()       
}


# ------------------------------------------------------------------------------


test.farimaSim = 
function()
{  
    # Simulation:
    
    # Try: 
    #   .farimaSimSlider()
    
    # farimaSim(
    #   n = 1000, 
    #   model = list(ar = c(0.5, -0.5), d = 0.3, ma = 0.1), 
    #   method = c("freq", "time"), 
    #   ...) 

    # Frequency Method:
    set.seed(4711)
    x = farimaSim(n = 50, method = "freq")
    print(x)
    target = round(var(x), 2)
    print(target)
    checkSum = 1.11
    checkEquals(target, current = checkSum)
    
    # Time Methhod:
    set.seed(4711)
    x = farimaSim(n = 50, method = "time")
    print(x)
    target = round(var(x), 2)
    print(target)
    checkSum = 1.11
    checkEquals(target, current = checkSum)
    
    # Return Value:
    return()   
}


# ------------------------------------------------------------------------------


test.whittleFit = 
function()
{
    # whittleFit(
    #   x, order = c(1, 1), subseries = 1, 
    #   method = c("fgn", "farma"), 
    #   trace = FALSE, spec = FALSE, title = NULL, description = NULL) 


    # Beran - Simulate:
    set.seed(1985)
    x = fgnSim(n = 100, H = 0.7, method = "beran")
    
    # Fit:
    Hurst = whittleFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    # Durbin - Simulate:
    set.seed(1985)
    x = fgnSim(n = 100, H = 0.7, method = "durbin")
    
    # Fit:
    Hurst = whittleFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    # Paxson - Simulate:
    set.seed(1985)
    x = fgnSim(n = 100, H = 0.7, method = "paxson")
    
    # Fit:
    Hurst = whittleFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    # Return Value:
    return()   
}


# ------------------------------------------------------------------------------


test.hurstFit = 
function()
{  
    # Try:
    #   hurstSlider(x = fbmSim(n = 1000, H = 0.7, fgn = TRUE))
    
    # Methods:
    #   method = 1: aggvarFit      
    #   method = 2: diffvarFit 
    #   method = 3: absvalFit 
    #   method = 4: higuchiFit 
    #   method = 5: pengFit 
    #   method = 6: rsFit 
    #   method = 7: perFit 
    
    # Simulate:
    set.seed(1953)
    x = fbmSim(n = 1000, H = 0.7, method = "mvn", fgn = TRUE)
    
    Hurst = aggvarFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    Hurst = diffvarFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    Hurst = absvalFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    Hurst = higuchiFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    Hurst = pengFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    Hurst = rsFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    Hurst = perFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEqualsNumeric(target, checkValue, tol = 0.2)
    
    # More Estimators:
    
    
    # Return Value:
    return()   
}


# ------------------------------------------------------------------------------


test.waveletFit = 
function()
{
    # waveletFit(
    #   x, length = NULL, order = 2, octave = c(2, 8), 
    #   doplot = FALSE, title = NULL, description = NULL) 

    # Simulate:
    set.seed(1953)
    x = fbmSim(n = 1000, H = 0.7, method = "mvn", fgn = TRUE)
    
    # Fit:
    Hurst = waveletFit(x)@hurst$H
    print(Hurst)
    target = round(Hurst, 1)
    print(target)
    checkValue = 0.7
    checkEquals(target, checkValue, tol = 0.2)
    
    # Return Value:
    return()   
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fSeries/test/runit3A.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
