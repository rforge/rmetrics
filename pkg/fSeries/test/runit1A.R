
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
# FUNCTION:               DESCRIPTION:
#  'fARMA'                 S4 Class representation for "fARMA" objects
#  armaSim                 Simulates an ARIMA time series process
#  armaFit                 Fits parameters for ARMA Time Series process
#  .arFit                   Internal function called by armaFit
#  .arimaFit                Internal function called by armaFit
#  .arfimaFit               Internal function called by armaFit
# S3 METHOD:              PREDICTION:
#  predict.fARMA           S3: Predicts from an ARMA time series prrocess 
#  .arPpredict             Internal function called by predict.fARMA
#  .arimaPpredict          Internal function called by predict.fARMA
#  .arfimaPredict          Not yet implemented
# S3 METHOD:              PRINT - SUMMARY - PLOT:
#  print.fARMA             S3: Prints a fitted ARMA time series object
#  plot.fARMA              S3: Plots stylized facts of a fitted ARMA object
#  summary.fARMA           S3: Summarizes a fitted ARMA time series object
# S3 METHOD:              ADDON:
#  fitted.fARMA            S3: Returns fitted values from a fitted ARMA object
#  coef.fARMA              S3: Returns coefficidents from a fitted ARMA object
#  coefficients.fARMA      S3: Synonyme for coef.fARMA
#  residuals.fARMA         S3: Returns residuals from a fitted ARMA object
################################################################################

################################################################################
# FUNCTION:               DESCRIPTION:
#  armaTrueacf             Returns True ARMA autocorrelation function
#  armaRoots               Returns Roots of the ARMA characteristic polynomial
################################################################################


################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ArmaModelling); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.fARMA = 
function()
{   
    # Slot Names:
    Names = getSlots("fARMA")
    target =  names(Names)
    current = c(
        "call",             
        "formula", 
        "method",     
        "parameter",       
        "data",             
        "fit",     
        "residuals",  
        "fitted.values",   
        "predicted.values", 
        "title",   
        "description")
    checkIdentical(target, current)
   
    # Return Value:
    return()    
}    


# ------------------------------------------------------------------------------


test.ar2Fit = 
function()
{   
    # Note, internally R's base function ar() is used
    
    # Simulate:
    set.seed(1953)
    x = armaSim(model = list(ar = c(0.5, -0.5)), n = 1000)
    
    # method = c("mle", "ols")
    
    # Fit:
    object = armaFit(formula = x ~ ar(2), method = "ols")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)

    # Fit:
    object = armaFit(formula = x ~ ar(2), method = "mle")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)

    # Note, also other methods can be used supported by ar():
    
    # Fit:
    object = armaFit(formula = x ~ ar(2), method = "yw")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)

    # Fit:
    object = armaFit(formula = x ~ ar(2), method = "burg1")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)

    # Fit:
    object = armaFit(formula = x ~ ar(2), method = "burg2")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)

    # Note, also arma() or arima() formulas can be applied:
    
    # Fit:
    object = armaFit(formula = x ~ arima(2, 0, 0), method = "CSS-ML")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)

    # Fit:
    object = armaFit(formula = x ~ arima(2, 0, 0), method = "CSS")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)

    # Fit:
    object = armaFit(formula = x ~ arima(2, 0, 0), method = "ML")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)
   
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.ar2Report = 
function()
{    
    # Simulate:
    set.seed(4711)
    x = armaSim(model = list(ar = c(0.5, -0.5)), n = 1000)
    
    # Fit:
    object = armaFit(formula = x ~ ar(2), method = "mle")
    
    # Report:
    print(object)
    par(mfrow = c(2, 2), cex = 0.7)
    plot(object, which = "all")
    summary(object, doplot = FALSE)
    
    # Get Values:
    coefficients(object)
    coef(object)
    fitted(object)[1:10]
    residuals(object)[1:10]
    
    # Predict:
    predict(object)
   
    # Return Value:
    return()    
}

  
# ------------------------------------------------------------------------------


test.ma2Fit = 
function()
{   
    # Simulate:
    set.seed(4711)
    x = armaSim(model = list(d = 0, ma = c(0.5, -0.5)), n = 5000)
    
    # To Fit a MA Model use ma(q), arma(0,q) or arima(0, 0, q):
    object = armaFit(formula = x ~ ma(2))
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)
    
    # Note, also arma() or arima() formulas can be applied:
    
    # Fit:
    object = armaFit(formula = x ~ arima(0, 0, 2), method = "CSS-ML")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)
    
    # Fit:
    object = armaFit(formula = x ~ arima(0, 0, 2), method = "CSS")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)
    
    # fit:
    object = armaFit(formula = x ~ arima(0, 0, 2), method = "ML")
    print(object)
    target = as.vector(round(coef(object), 1))
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.ma2Report = 
function()
{        
    # Simulate:
    set.seed(4711)
    x = armaSim(model = list(d = 0, ma = c(0.5, -0.5)), n = 5000)
    
    # To Fit a MA Model use ma(q), arma(0,q) or arima(0, 0, q):
    object = armaFit(formula = x ~ ma(2))
    
    # Report:
    print(object)
    par(mfrow = c(2, 2), cex = 0.7)
    plot(object, which = "all")
    summary(object, doplot = FALSE)
    
    # Get Values:
    coefficients(object)
    coef(object)
    fitted(object)[1:10]
    residuals(object)[1:10]
    
    # Predict:
    predict(object)

    # Return Value:
    return()    
}

  
# ------------------------------------------------------------------------------


test.arma21Fit = 
function()
{
    # Simulate:
    set.seed(1985)
    x = armaSim(model = list(ar = c(0.5, -0.5), ma = 0.1), n = 1000)
    
    # Fit:
    object = armaFit(formula = x ~ arima(2, 0, 1), method = "mle")
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.5, -0.5, 0.1, 0)
    checkEqualsNumeric(target, current)
    
    # Fit:
    object = armaFit(formula = x ~ arima(2, 0, 1), method = "CSS-ML")
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.5, -0.5, 0.1, 0)
    checkEqualsNumeric(target, current)
    
    # Fit:
    object = armaFit(formula = x ~ arima(2, 0, 1), method = "CSS")
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.5, -0.5, 0.1, 0)
    checkEqualsNumeric(target, current)
    
    # Fit:
    object = armaFit(formula = x ~ arima(2, 0, 1), method = "ML")
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.5, -0.5, 0.1, 0)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.arma21Report = 
function()
{            
    # Simulate:
    x = armaSim(model = list(ar = c(0.5, -0.5), ma = 0.1), n = 1000)
    
    # Fit:
    object = armaFit(formula = x ~ arima(2, 0, 1), method = "CSS-ML")
     
    # Report:
    print(object)
    par(mfrow = c(2, 2), cex = 0.7)
    plot(object, which = "all")
    summary(object, doplot = FALSE)
    
    # Get Values:
    coefficients(object)
    coef(object)
    fitted(object)[1:10]
    residuals(object)[1:10]
    
    # Predict:
    predict(object)

    # Return Value:
    return()       
}

  
# ------------------------------------------------------------------------------


test.arima211Fit = 
function()
{
    # Simulate:
    set.seed(4711)
    x = armaSim(model = list(ar = c(0.5, -0.5), d = 1, ma = 0.1), n = 1000)
    
    # Fit:
    object = armaFit(formula = x ~ arima(2, 1, 1), method = "CSS-ML")
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)
    
    # Fit:
    object = armaFit(formula = x ~ arima(2, 1, 1), method = "CSS")
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)
    
    # Fit:
    object = armaFit(formula = x ~ arima(2, 1, 1), method = "ML")
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.5, -0.5, 0)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.arima211Report = 
function()
{               
    # Simulate:
    set.seed(4711)
    x = armaSim(model = list(ar = c(0.5, -0.5), d = 1, ma = 0.1), n = 1000)
    
    # Integrated ARMA Fit:
    object = armaFit(formula = x ~ arima(2, 1, 1), method = "CSS-ML")
    
    # Report:
    print(object)
    par(mfrow = c(2, 2), cex = 0.7)
    plot(object, which = "all")
    summary(object, doplot = FALSE)
    
    # Get Values:
    coefficients(object)
    coef(object)
    fitted(object)[1:10]
    residuals(object)[1:10]
    
    # Predict:
    predict(object)[1:10]           # CHECK
    
    # Return Value:
    return()    
}

  
# ------------------------------------------------------------------------------


test.arfimaFit = 
function()
{
    # Simulate:
    set.seed(4711)
    x = armaSim(model = list(d = 0.3), n = 1000)
    
    # Fit:
    object = armaFit(formula = x ~ arfima(0, 0))    
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = 0.3
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.arfimaReport = 
function()
{               
    # Simulate:
    set.seed(4711)
    x = armaSim(model = list(d = 0.3), n = 1000)
    
    # Fit:
    object = armaFit(formula = x ~ arfima(0, 0))    
    
    # Report:
    print(object)
    plot(object, which = "all")         # not yet implemented       
    summary(object)                     # uses always doplot=FALSE
    
    # Get Values:
    coefficients(object)
    coef(object)
    fitted(object)[1:10]                # not yet implemented 
    residuals(object)[1:10]             # not yet implemented 
    
    # Predict:
    predict(object)
      
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.armaUtils = 
function()
{ 
    
    # armaTrueacf           Returns True ARMA autocorrelation function
    # armaRoots             Returns Roots of the ARMA characteristic polynomial

    # armaTrueacf(model, lag.max = 20, type = "correlation", doplot = TRUE)
    model = list(ar = c(0.5, -0.5))
    armaTrueacf(model, lag.max = 10)
    
    # armaRoots(coefficients, n.plot = 400, digits = 4, ...)
    coefficients = c(-0.5, 0.9, -0.1, -0.5)
    ans = armaRoots(coefficients)
    target = round(sum(ans), 2)
    checkSum = 4.58
    checkEqualsNumeric(target, checkSum)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fSeries/test/runit1A.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
