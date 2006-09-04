
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


################################################################################
#  *                       Asterisked Functions are in ArmaModelling.R
# FUNCTION:               DESCRIPTION:
#  *'fARMA'                S4 Class representation for "fARMA" objects
#  *armaSim                Simulates a time series process from the ARIMA family
#  arfimaOxFit             Fits parameters for AR(FI)MA time series processes
# S3 METHOD:              PREDICTION:
#  *predict.fARMA          S3: Predicts from an ARMA time series prrocess 
#  .arfimaOxPredict            Internal function called by predict.fARMA
#  *predictPlot            S3: Use method
#  *predictPlot.fARMA      S3: Plots from an ARMA time series prediction
# S3 METHOD:              PRINT - SUMMARY - PLOT:
#  *print.fARMA            S3: Prints a fitted ARMA time series object
#  *plot.fARMA             S3: Plots stylized facts of a fitted ARMA object
#  *summary.fARMA          S3: Summarizes a fitted ARMA time series object
#  *fitted.fARMA           S3: Returns fitted values from a fitted ARMA object
#  *residuals.fARMA        S3: Returns residuals from a fitted ARMA object
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ArfimaOxInterface); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.arfimaFit = 
function()
{
    # OX-ARFIMA(2,1) - WARNING: MA Coefficients have opposite sign!
    
    # Set Path:
    OXPATH <<- "C:\\Ox\\Ox3"
    
    # Simulate:
    set.seed(1985)
    x = armaSim(model = list(ar = c(0.5, - 0.5), d = 0.3, ma = 0.1), n = 1000)
    
    # MLE Fit - method="mle":
    object = arfimaOxFit(formula = ~arfima(2,1), data = x)
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.3, 0.5, -0.5, -0.1)
    checkEqualsNumeric(target, current)
    
    # MLE Fit - method="nls":
    object = arfimaOxFit(formula = ~ arfima(2, 1), data = x, method = "nls")
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.3, 0.5, -0.5, -0.1)
    checkEqualsNumeric(target, current)
    
    # MLE Fit - method="mpl":
    object = arfimaOxFit(formula = ~ arfima(2, 1), data = x, method = "mpl")
    print(object)
    target = as.vector(round(coef(object), 1))
    print(target)
    current = c(0.3, 0.5, -0.5, -0.1)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.arfimaReport = 
function()
{    
    # Set Path:
    OXPATH <<- "C:\\Ox\\Ox3"
    
    # Simulate:
    set.seed(1985)
    x = armaSim(model = list(ar = c(0.5, - 0.5), d = 0.3, ma = 0.1), n = 1000)
    
    # Fit:
    object = arfimaOxFit(formula = ~ arfima(2, 1), data = x)
    
    # Report:
    print(object)
    
    # Plot:
    # par(mfrow = c(2,2), cex = 0.7)
    # plot(object, which = "all")           # Not yet implemented
    
    # Summary:
    summary(object)
    
    # Get Values:
    coefficients(object)
    coef(object)
    fitted(object)[1:10]      
    residuals(object)[1:10]       
      
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------



test.arfimaPredict = 
function()
{    
    # Set Path:
    OXPATH <<- "C:\\Ox\\Ox3"
    
    # Simulate:
    set.seed(1985)
    x = armaSim(model = list(ar = 0.1, d = 0.2, ma = c(0.5, -0.5)), n = 1000)
    
    # Fit:
    object = arfimaOxFit(formula = ~ arfima(1, 2), data = x)
    print(object)
   
    # Predict:
    # predict.fARMA(object, n.ahead = 10, n.back = 50, conf = c(80, 95), 
    #   doplot = TRUE, ...) 
    # plot for predict method ignored, not yet available
    predict(object)[1:10]   
    predict(object, doplot = FALSE)[1:10]                                # CHECK     
    
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.noTrace = 
function()
{    
    # Set Path:
    OXPATH <<- "C:\\Ox\\Ox3"
    
    # Simulate and Fit:
    set.seed(1985)
    x = armaSim(model = list(ar = 0.5, d = 0.3, ma = 0.5), n = 1000)
    object = arfimaOxFit(formula = ~ arfima(1, 1), data = x, trace = FALSE)      
    object
    
    # Simulate and Fit:
    set.seed(1985)
    x = armaSim(model = list(ar = 0, d = 0.3, ma = 0), n = 1000)
    object = arfimaOxFit(formula = ~ arfima(1, 1), data = x)      
    object
    
    # Simulate and Fit:
    set.seed(1985)
    x = armaSim(model = list(ar = 0, d = 0.3, ma = 0), n = 1000)
    object = arfimaOxFit(formula = ~ arfima(0, 0), data = x)      
    object
    
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fSeries/test/runit1B.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
