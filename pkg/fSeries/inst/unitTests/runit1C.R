
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
# FUNCTION:               DESCRIPTION:
#  * fARMA                 Class representation for "fARMA" objects
#  * armaSim               Simulates a time series process from the ARIMA family
#    arfimaOxFit           Fits parameters for AR(FI)MA time series processes
# S3 METHOD:              PREDICTION:
#  * predict.fARMA         S3: Predicts from an ARMA time series prrocess 
#    .arfimaOxPredict          Internal function called by predict.fARMA
# S3 METHOD:              RINT - PLOT - SUMMARY METHODS:
#  * show.fARMA            S4: Prints a fitted ARMA time series object
#  * plot.fARMA            S3: Plots stylized facts of a fitted ARMA object
#  * summary.fARMA         S3: Summarizes a fitted ARMA time series object
# S3 METHOD:              ADDON METHODS:
#  * coef.fARMA            S3: Returns coefficidents from a fitted ARMA object
#  * coefficients.fARMA    S3: Synonyme for coef.fARMA
#  * fitted.fARMA          S3: Returns fitted values from a fitted ARMA object
#  * residuals.fARMA       S3: Returns residuals from a fitted ARMA object
#  *                      Asterisked Functions are in ArmaModelling.R
################################################################################


# MS WINDOWS ONLY !!!


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ArfimaOxInterface, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.arfimaOxFit = 
function()
{
    # OX-ARFIMA(2,1) - IMPORTANT: MA Coefficients have opposite sign!
    
    # Set Path:
    OXPATH <<- "C:\\Ox\\Ox3"
    
    # Simulate:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = armaSim(model = list(ar = c(0.5, - 0.5), d = 0.3, ma = 0.1), n = 1000)
    
    # MLE Fit - Default method="mle":
    object = arfimaOxFit(formula = ~arfima(2,1), data = x)
    print(object)
    target = as.vector(round(coef(object), 3))
    print(target)
    current = c(0.293, 0.396, -0.450, 0.030)
    checkEqualsNumeric(target, current)
       
    # NLS Fit - method="nls":
    object = arfimaOxFit(formula = ~ arfima(2, 1), data = x, method = "nls")
    print(object)
    target = as.vector(round(coef(object), 3))
    print(target)
    current = c(0.302, 0.402, -0.455, 0.014)
    checkEqualsNumeric(target, current)
        
    # MPL Fit - method="mpl":
    object = arfimaOxFit(formula = ~ arfima(2, 1), data = x, method = "mpl")
    print(object)
    target = as.vector(round(coef(object), 3))
    print(target)
    current = c(0.293, 0.396, -0.450, 0.030)
    checkEqualsNumeric(target, current)
       
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.arfimaOxReport = 
function()
{    
    # Set Path:
    OXPATH <<- "C:\\Ox\\Ox3"
    
    # Simulate:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = armaSim(model = list(ar = c(0.5, - 0.5), d = 0.3, ma = 0.1), n = 1000)
    
    # Fit:
    object = arfimaOxFit(formula = ~ arfima(2, 1), data = x)
    
    # Report:
    print(object)
    
    # Plot:
    # 1:   Standardized Residuals
    # 2:   ACF of Residuals
    # 3:   QQ Plot of Residuals
    # 4:   Ljung-Box p Values
    par(mfrow = c(2, 2), cex = 0.7)
    plot(object, which = "all") 
    
    # Try - Interactive Plot:
    # plot(object)
    
    # Summary:
    summary(object, doplot = FALSE)
    
    # Get Values:
    coefficients(object)
    coef(object)
    fitted(object)[1:10]      
    residuals(object)[1:10]       
      
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.arfimaOxPredict = 
function()
{    
    # Set Path:
    OXPATH <<- "C:\\Ox\\Ox3"
    
    # Simulate:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = armaSim(model = list(ar = 0.1, d = 0.2, ma = c(0.5, -0.5)), n = 1000)
    
    # Fit:
    object = arfimaOxFit(formula = ~ arfima(1, 2), data = x)
    print(object)
   
    # Predict:
    predict(object, n.ahead = 10, n.back = 50, conf = c(80, 95), doplot = FALSE) 
    predict(object)$pred[1:5]  
    predict(object)$pred[1:5]   
    predict(object, doplot = TRUE)    
    
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.arfimaOxNoTrace = 
function()
{    
    # Set Ox Path:
    OXPATH <<- "C:\\Ox\\Ox3"
    
    # Simulate:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = armaSim(model = list(ar = 0.65, d = 0.3, ma = 0.65), n = 1000)
    
    # Fit:
    object = arfimaOxFit(formula = x ~ arfima(1, 1), data = x, trace = FALSE)      
    print(object)
    summary(object)
    coef(object)
      
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fSeries/tests/runit1C.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################
    
