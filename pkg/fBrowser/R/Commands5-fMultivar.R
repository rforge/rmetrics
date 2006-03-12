
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA. 

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
# Regression Modelling


.fMultivar.Regression.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # LM - Example Data:
    # Example Data: x = 0.7*x1 + 0.3*x2 + eps
    x1 = rnorm(1000)
    x2 = rnorm(1000)
    y = 0.7 * x1 + 0.3 * x2
    eps = 0.1 * rnorm(1000)
    y = y + eps
    x <<- tkSaveAsX(data = data.frame(x = y, x1 = x1, x2 = x2),
        infoName = "0.7*x1+0.3*x2+eps")
}


.fMultivar.Regression.2 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # GLM / BINOMIAL / LOGIT - Example Data:
    # Example Data: x = 10*sin(x1) + exp(x2) + eps
    x1 = rnorm(1000)
    x2 = rnorm(1000)
    eps = 0.1 * rnorm(1000)
    y = 0.7 * x1 + 0.3 * x2 + eps
    p = 1 / ( 1 + exp(-y) )
    x <<- tkSaveAsX(data = data.frame(x = p, x1 = x1, x2 = x2),
        infoName = "LOGIT(0.7*x1+0.3*x2+eps)")
    # family = binomial(link = logit)
    # glm(formula = x ~ x1 + x2, family = family, data = x)
}


.fMultivar.Regression.3 = 
function() 
{   # A function implemented by Diethelm Wuertz

    
    # GAM - Example Data:
    # Example Data: x = 0.7*sin(x1) + 0.3*exp(x2) + eps
    x1 = rnorm(1000)
    x2 = rnorm(1000)
    y = sin(x1) + exp(x2)
    eps = 0.1 * rnorm(1000, sd = sd(y))
    y = y + eps
    x <<- tkSaveAsX(data = data.frame(x = y, x1 = x1, x2 = x2),
        infoName = "x = 0.7*sin(x1)+0.3*exp(x2)+eps")
    # fit = gam(formula = x ~ s(x1) + s(x2), data = x)
    # plot(fit, pages = 1, scale = 0)   
}


.fMultivar.Regression.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Linear Modelling:
    myFunction = function(formula, family) {
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "LM", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            family = "gaussian"),
        infoName = "LM Modelling",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL) 
}


.fMultivar.Regression.5 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generalized Linear Modelling:
    myFunction = function(formula, family){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "GLM", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            family = "gaussian"),
        infoName = "GLM Modelling",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fMultivar.Regression.6 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generalized Additive Modelling
    myFunction = function(formula){
        object <<- regFit(
            formula = as.formula(formula), 
            family = gaussian(), 
            data = x, method = "GAM", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ s(x1) + s(x2)"), 
        infoName = "GAM Modelling",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fMultivar.Regression.7 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Projection Pursuit Regression:
    myFunction = function(formula){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "PPR", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "Y ~ X1 + X2"),
        infoName = "PPR Modelling",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fMultivar.Regression.8 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # MARS Pursuit Regression:
    myFunction = function(formula){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "MARS", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "Y ~ X1 + X2"), 
        infoName = "MARS Modelling",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fMultivar.Regression.9 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # POLYMARS Pursuit Regression:
    myFunction = function(formula){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "POLYMARS", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "Y ~ X1 + X2"),
        infoName = "POLYMARS Modelling",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )  
}


.fMultivar.Regression.10 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Neural Network Regression:
    myFunction = function(formula){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "NNET", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "Y ~ X1 + X2"), 
        infoName = "NNET Modelling",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fMultivar.Regression.11 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # ... Print Summary
    tkGetSummary(object)
}


# ******************************************************************************
# Regression Tests


.fMultivar.RegressionTests.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: realInvest ~ realGNP + realInterest
    data(Greene4Table131)
    Greene4Table131 = as.timeSeries(Greene4Table131[, c(1, 6:8)])
    colnames(Greene4Table131@Data) = c("x", "x1", "x2")
    x <<- tkSaveAsX(data = Greene4Table131,
        infoName = "Green: x=realInvest | x1=realGNP x2=realInterest")
}


.fMultivar.RegressionTests.2 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Breusch-Godfrey Test:
    myFunction = function(formula, order, type) {
        formula = as.formula(formula)
        order = as.integer(order)
        object <<- bgTest(formula = formula, order = order, 
            type = type, data = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            order = 1,
            type = "Chisq"),
        infoName = "Breusch - Godfrey Test",
        tkoutput = TRUE,
        console = NULL,
        title = "Breusch - Godfrey Test",
        description = NULL )
}


.fMultivar.RegressionTests.3 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Breusch-Pagan Test:
    myFunction = function(formula, studentize) {
        formula = as.formula(formula)
        object <<- bpTest(formula = formula, varformula = NULL, 
            studentize = studentize, data = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            studentize = TRUE),
        infoName = "Breusch - Pagan Test",
        tkoutput = TRUE,
        console = NULL,
        title = "Breusch - Pagan Test",
        description = NULL )
}


.fMultivar.RegressionTests.4 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Durbin-Watson Test:
    myFunction = function(formula, alternative, iterations) {
        formula = as.formula(formula)
        object <<- dwTest(formula = formula, alternative = alternative,
            iterations = iterations, exact = NULL, tol = 1.0e-10, 
            data = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            alternative = "greater",
            iterations = 15),
        infoName = "Durbin - Watson Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fMultivar.RegressionTests.5 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Goldfeld - Quandt Test
    myFunction = function(formula, alternative, iterations) {
        formula = as.formula(formula)
        object <<- dwTest(formula = formula, data = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            alternative = "greater",
            iterations = 15),
        infoName = " Test",
        tkoutput = TRUE,
        console = NULL,
        title = " Test",
        description = NULL )
}


.fMultivar.RegressionTests.6 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Harvey - Collier Test:
    myFunction = function(formula, alternative, iterations) {
        formula = as.formula(formula)
        object <<- dwTest(formula = formula, data = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            alternative = "greater",
            iterations = 15),
        infoName = " Test",
        tkoutput = TRUE,
        console = NULL,
        title = " Test",
        description = NULL ) 
}


.fMultivar.RegressionTests.7 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Harrison - McCabe Test:
    myFunction = function(formula, alternative, iterations) {
        formula = as.formula(formula)
        object <<- dwTest(formula = formula, data = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            alternative = "greater",
            iterations = 15),
        infoName = " Test",
        tkoutput = TRUE,
        console = NULL,
        title = " Test",
        description = NULL )
}


.fMultivar.RegressionTests.8 = 
function()
{
    # Rainbow Test:
    myFunction = function(formula, alternative, iterations) {
        formula = as.formula(formula)
        object <<- dwTest(formula = formula, data = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            alternative = "greater",
            iterations = 15),
        infoName = " Test",
        tkoutput = TRUE,
        console = NULL,
        title = " Test",
        description = NULL )
}


.fMultivar.RegressionTests.9 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Ramsey RESET Test:
    myFunction = function(formula, alternative, iterations) {
        formula = as.formula(formula)
        object <<- dwTest(formula = formula, data = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            alternative = "greater",
            iterations = 15),
        infoName = " Test",
        tkoutput = TRUE,
        console = NULL,
        title = " Test",
        description = NULL ) 
}


# ******************************************************************************
# Equations Modelling


.fMultivar.EquationsModelling.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Equations Modelling:
    tkinsert(txt, "end", "\n\nSorry, not yet implemented!\n")   
}


# ******************************************************************************
# Matrix Addon


.fMultivar.MatrixAddon.1 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Generate Pascal Matrix:
    myFunction = function(n, object2x) {
        n = as.integer(n)
        object <<- pascal(n)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 5,
            object2x = FALSE),
        infoName = "pascal(n)",
        tkoutput = TRUE,
        console = NULL,
        title = "Pascal Matrix",
        description = NULL )
}
    
    
.fMultivar.MatrixAddon.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Return Diagonal Matrix:
    myFunction = function(object2x) {
        object <<- diag(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE),
        infoName = "diag(x)",
        tkoutput = TRUE,
        title = "Diagonal Matrix",
        description = NULL ) 
}
    
    
.fMultivar.MatrixAddon.3 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Lower Triangular Matrix:
    myFunction = function(object2x) {
        object <<- triang(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE),
        infoName = "triang(x)",
        tkoutput = TRUE,
        title = "Lower Triangular Matrix",
        description = NULL ) 
}
    
    
.fMultivar.MatrixAddon.4 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Upper Triangular Matrix:
    myFunction = function(object2x) {
        object <<- Triang(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE),
        infoName = "Triang(x)",
        tkoutput = TRUE,
        title = "Upper Triangular Matrix",
        description = NULL ) 
}
    
    
.fMultivar.MatrixAddon.5 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Determinant:
    myFunction = function(object2x) {
        object <<- det(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE),
        infoName = "det(x)",
        tkoutput = TRUE,
        title = "Determinant",
        description = NULL )
}
    
    
.fMultivar.MatrixAddon.6 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Inverse Matrix:
    myFunction = function(object2x) {
        object <<- inv(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE),
        infoName = "inv(x)",
        tkoutput = TRUE,
        title = "Inverse of Matrix",
        description = NULL ) 
}
    
    
.fMultivar.MatrixAddon.7 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # norm(x, p) p-Norm:
    myFunction = function(p, object2x) {
        p = as.integer(p)
        object <<- norm(x, p)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            p = 2,
            object2x = FALSE),
        infoName = "norm(x)",
        tkoutput = TRUE,
        title = "p-Norm",
        description = NULL )
}
    
    
.fMultivar.MatrixAddon.8 =
function()
{   # A function implemented by Diethelm Wuertz

    # rk(x, method) Rank:
    myFunction = function(method, object2x) {
        object <<- rk(x = x, method = method)
        object }
    tkExecute(
        fun = myFunction, 
        params = list(,
        object2x = FALSE),
        infoName = "rk(x)",
        tkoutput = TRUE,
        title = "Rank",
        description = NULL )
}
    
    
.fMultivar.MatrixAddon.9 =
function()
{   # A function implemented by Diethelm Wuertz

    # t(x) Transposed"
    myFunction = function(method, object2x) {
        object <<- t(x)
        object }
    tkExecute(
        fun = myFunction, 
        params = list(
            object2x = FALSE),
        infoName = "t(x)",
        tkoutput = TRUE,
        title = "Transposed",
        description = NULL )
}
    
    
.fMultivar.MatrixAddon.10 =
function()
{   # A function implemented by Diethelm Wuertz

    # mexp(x, order, method) Exponentiate Square Matrix:
    myFunction = function(order, method) {
        object <<- mexp(x = x, order = order, method = method, object2x)
        object  }
    tkExecute(
        fun = myFunction,
        params = list(
            order = 8, 
            method = "pade",
            object2x = FALSE),
        infoName = "t(x)",
        tkoutput = TRUE,
        title = "Exponentiate",
        description = NULL )
}
    
    
.fMultivar.MatrixAddon.11 =
function()
{   # A function implemented by Diethelm Wuertz

    # chol(x, pivot) Cholesky Factors:
    myFunction = function(pivot, object2x) {
        object <<- chol(x = x, pivot = pivot, LINPACK = pivot) 
        object  }
    tkExecute(
        fun = myFunction,
        params = list(
            pivot = FALSE,
            object2x = FALSE),
        infoName = "t(x)",
        tkoutput = TRUE,
        title = "Cholesky Factors",
        description = NULL )
}
    
    
.fMultivar.MatrixAddon.12 =
function()
{   # A function implemented by Diethelm Wuertz

    # eigen(x) Eigenvalues and Eigevectors:
    what = "eigen(x) Eigenvalues and Eigevectors"
    object <<- eigen(x)
    tkTitle(what)
    tkOutput(capture.output(object))
}
    
    
.fMultivar.MatrixAddon.13 =
function()
{   # A function implemented by Diethelm Wuertz

    # svd(x) Singular Value Decomposition:
    what = "svd(x) Singular Value Decomposition"
    object <<- svd(x)
    tkTitle(what)
    tkOutput(capture.output(object))
}
    
    
.fMultivar.MatrixAddon.14 =
function()
{   # A function implemented by Diethelm Wuertz

    # kappa(x) Condition Number:
    what = "kappa(x) Condition Number"
    object <<- svd(x)
    tkTitle(what)
    tkOutput(capture.output(object))
}
    
    
.fMultivar.MatrixAddon.15 =
function()
{   # A function implemented by Diethelm Wuertz

    # QR Decomposition:
    what = "QR Decomposition"
    object <<- qr(x)
    tkTitle(what)
    tkOutput(capture.output(object))
}


# ******************************************************************************
# Missing Values


.fMultivar.MissingValues.1 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = MSFT|SP500 Returns:
    tkGetData(Data = "msftsp500Monthly", 
        infoName = "Return Series with NA")  
    x <<- x[109:132, ]
    x@Data[1, 2] <<- NA
    x@Data[9, ] <<- c(NA, NA)
    x@Data[10, 2] <<- NA
    x@Data[24, 1] <<- NA
    print(x)
}


.fMultivar.MissingValues.2 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Remove NAs: 
    myFunction = function(series, object2x) {
        x = eval(parse(text = series))
        object <<- removeNA(x = x) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x", 
            object2x = FALSE),
        infoName = "Remove NAs",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )   
}


.fMultivar.MissingValues.3 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Interpolate NAs:
    myFunction = function(series, method, object2x) {
        x = eval(parse(text = series))
        object <<- interpNA(x = x, method = method) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            method = "before", 
            object2x = FALSE),
        infoName = "Interpolate NAs",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )      
}


.fMultivar.MissingValues.4 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # knn Algorithm:
    myFunction = function(series, correlation, object2x) {
        x = eval(parse(text = series))
        object <<- knnNA(x = x, k = max(dim(as.matrix(x))[1] * 0.01, 2), 
            correlation = correlation) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            correlation = FALSE, 
            object2x = FALSE),
        infoName = "knn Algorithm",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )          
}


.fMultivar.MissingValues.5 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = MSFT|SP500 Returns
    X = as.timeSeries(singleIndex.dat, format = "%d-%b-%Y")[108:132, ]
    X = returnSeries(X)
    X@units = paste(X@units, ".RET", sep = "")
    colnames(X@Data) = X@units
    X@FinCenter = "GMT"
    X@Data[1, 2] = NA
    X@Data[9, ] = c(NA, NA)
    X@Data[10, 2] = NA
    X@Data[24, 1] = NA
    x <<- tkSaveAsX(data = X, infoName = "MSFT | SP500 Returns")
}


.fMultivar.MissingValues.6 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Substitute NAs:
    myFunction = function(series, type) {
        x = eval(parse(text = series))
        substituteNA(x = x, type = type) }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            type = "zeros"),
        infoName = "Substitute NAs",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )      
}


.fMultivar.MissingValues.7 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... Save object as x:
    NA
}


# ******************************************************************************
# Technical Analysis


.fMultivar.TechnicalAnalysis.1 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Data: Open-High-Low-Close SP500
    data(spc1970)
    spc1995 = spc1970[-(1:6319),]
    x <<- tkSaveAsX(data = as.timeSeries(spc1995), 
        infoName = "SPC500 Open-High-Low-Close") 
}


.fMultivar.TechnicalAnalysis.2 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # emaTA - Exponential Moving Average
    myFunction = function(select, lag, doplot, col) {
        object <<- .dailyTA(X = x, indicator = "ema", 
            select = select, lag = lag)
        if (doplot) {
            what = "Exponential Moving Average"
            par(mfrow = c(1, 1))
            plot(x[, select], ylab = select, main = select)
            title(paste("\n\n", what, sep = ""))
            lines(object, col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
            col = "red"),
        infoName = "EMA Indicator",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )   
}


.fMultivar.TechnicalAnalysis.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # biasTA - EMA Price Bias
    myFunction = function(select, lag, doplot, col) {
        object <<- .dailyTA(X = x, indicator = "bias", 
            select = select, lag = lag)
        if (doplot) {
            what = "EMA Price Bias"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = "Bias", main = "Bias", col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
            col = "red"),
        infoName = "EMA Price Bias",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )  
}


.fMultivar.TechnicalAnalysis.4 =
function()
{   # A function implemented by Diethelm Wuertz

    # medpriceTA - Median Price
    myFunction = function(select, lag, doplot, col) {
        object <<- .dailyTA(X = x, indicator = "medprice", 
            select = select, lag = lag)
        if (doplot) {
            what = "Median Price"
            par(mfrow = c(1, 1))
            plot(x[, select], ylab = select, main = select)
            title(paste("\n\n", what, sep = ""))
            lines(object, col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
            col = "red"),
        infoName = "Median Price",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )  
}


.fMultivar.TechnicalAnalysis.5 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # typicalpriceTA - Typical Price
    myFunction = function(select, lag, doplot, col) {
        object <<- .dailyTA(X = x, indicator = "typicalprice", 
            select = select, lag = lag)
        if (doplot) {
            what = "Typical Price"
            par(mfrow = c(1, 1))
            plot(x[, select], ylab = select, main = select)
            title(paste("\n\n", what, sep = ""))
            lines(object, col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
            col = "red"),
        infoName = "Typical Price",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )  
}


.fMultivar.TechnicalAnalysis.6 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # wcloseTA - Weighted Close Price
    myFunction = function(select, lag, doplot, col) {
        object <<- .dailyTA(X = x, indicator = "wclose", 
            select = select, lag = lag)
        if (doplot) {
            what = "Weighted Closing Price"
            par(mfrow = c(1, 1))
            plot(x[, select], ylab = select, main = select)
            title(paste("\n\n", what, sep = ""))
            lines(object, col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
            col = "red"),
        infoName = "Weighted Closing Price",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )  
}


.fMultivar.TechnicalAnalysis.7 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # rocTA - Rate of Change
    myFunction = function(select, lag, doplot, col) {
        object <<- .dailyTA(X = x, indicator = "roc", 
            select = select, lag = lag)
        if (doplot) {
            what = "Rate of Change"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
            col = "red"),
        infoName = "Rate of Change",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )  
}


.fMultivar.TechnicalAnalysis.8 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # oscTA - EMA-Oscillator
    myFunction = function(select, lag1, lag2, doplot, col) {
        object <<- .dailyTA(X = x, indicator = "osc", 
            select = select, lag = c(lag1, lag2))
        if (doplot) {
            what = "EMA-Oscillator"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = "Close", 
            lag1 = 9,
            lag2 = 23,
            doplot = TRUE,
            col = "red"),
        infoName = "EMA-Oscillator",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )  
}


.fMultivar.TechnicalAnalysis.9 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # momTA - Momentum Oscillator
    myFunction = function(select, lag, doplot, col) {
        object <<- .dailyTA(X = x, indicator = "mom", 
            select = select, lag = lag)
        if (doplot) {
            what = "Momentum Oscillatore"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
            col = "red"),
        infoName = "Momentum Oscillator",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )  
}


.fMultivar.TechnicalAnalysis.10 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # macdTA -  MACD Oscillator
    myFunction = function(select, lag1, lag2, doplot, col) {
        object <<- .dailyTA(X = x, indicator = "macd", 
            select = select, lag = c(lag1, lag2))
        if (doplot) {
            what = "MACD Oscillator"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = "Close", 
            lag1 = 9,
            lag2 = 23,
            doplot = TRUE,
            col = "red"),
        infoName = "MACD Oscillator",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )  
}


.fMultivar.TechnicalAnalysis.11 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # cdsTA - MACD Signal Line
    myFunction = function(select, ...) {
        object <<- .dailyTA(X = x, indicator = "...", select = select, ...)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.12 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # cdoTA - MACD Oscillator
    myFunction = function(select, ...)
        .dailyTA(X = x, indicator = "...", select = select, ...)
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.13 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # vohlTA - High/Low Volatility
    myFunction = function(select, ...) {
        object <<- .dailyTA(X = x, indicator = "...", select = select, ...)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.14 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # vorTA - Volatility Ratio
    myFunction = function(select, ...) {
        object <<- .dailyTA(X = x, indicator = "...", select = select, ...)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.15 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # fpkTA - Fast %K Stochastics
    myFunction = function(select, ...) {
        object <<- .dailyTA(X = x, indicator = "...", select = select, ...)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.16 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # fpdTA - Fast %D Stochastics
    myFunction = function(select, ...) {
        object <<- .dailyTA(X = x, indicator = "...", select = select, ...)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.17 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # spdTA - Slow %D Stochastics
    myFunction = function(select, ...) {
        object <<- .dailyTA(X = x, indicator = "...", select = select, ...)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.18 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # apdTA - Averaged %D Stochastics
    myFunction = function(select, ...) {
        object <<- .dailyTA(X = x, indicator = "...", select = select, ...)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.19 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # wprTA - Williams %R Stochastics
    myFunction = function(select, ...) {
        object <<- .dailyTA(X = x, indicator = "...", select = select, ...)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.20 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # rsiTA - Relative Strength Index
    myFunction = function(select, ...) {
        object <<- .dailyTA(X = x, indicator = "...", select = select, ...)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            select = ..., 
            lag = ...),
        infoName = "... TA Indicator",
        tkoutput = FALSE )  
    print(start(object))
    print(tail(object))
}


.fMultivar.TechnicalAnalysis.21 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... merge time Series
    x <<- tkSaveAsX(mergeSeries(x, object@Data), "Merged Series & Indicator")
    print(start(x))
    print(tail(x))
}


# ******************************************************************************
# Benchmark Analysis


.fMultivar.BenchmarkAnalysis.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = SP500 Index
    tkGetData(Data = "sp500Index", infoName = "Monthly SP500 Index")
}


.fMultivar.BenchmarkAnalysis.2 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Maximum Draw-Down:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- maxDrawDown(x) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "maxDrawDown",
        tkoutput = TRUE,
        console = NULL,
        title = "Maximum Drawdown",
        description = NULL ) 
}


.fMultivar.BenchmarkAnalysis.3 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Sharpe Ratio:
    myFunction = function(series, r, scale) {
        x = eval(parse(text = series))
        scale = eval(parse(text = scale))
        object <<- sharpeRatio(x = x, r = r, scale = scale) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x", 
            r = 0, 
            scale = "sqrt(12)" ),
        infoName = "Sharpe Ratio",
        tkoutput = TRUE,
        console = NULL,
        title = "Sharpe Ratio",
        description = NULL ) 
}


.fMultivar.BenchmarkAnalysis.4 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Sterling Ratio:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- sterlingRatio(x = x) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Sterling Ratio",
        tkoutput = TRUE,
        console = NULL,
        title = "Sterling Ratio",
        description = NULL ) 
}


# ******************************************************************************
# Rolling Analysis


.fMultivar.RollingAnalysis.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x = SP500 Index
    x = as.timeSeries(data(singleIndex.dat), format = "%d-%b-%Y")[, 2]
    attr(x, "data") <- "singleIndex.dat[,2]"
    x <<- tkSaveAsX(data = x, infoName = "SP500 Index")
}   


.fMultivar.RollingAnalysis.2 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Mean:
    myFunction = function(n, trim, na.rm, doplot) {
        object <<- rollMean(x, n, trim, na.rm)
        if (doplot) {
            par(mfrow = c(1, 1), cex = 0.7)
            plot(x, ylab = "x")
            lines(object, col = "steelblue")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 9, 
            trim = TRUE, 
            na.rm = FALSE,
            doplot = TRUE),
        infoName = "Rolling Mean",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )      
}   


.fMultivar.RollingAnalysis.3 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Variance:
    myFunction = function(n, trim, na.rm, doplot) {
        object <<- rollVar(x, n, trim, na.rm)
        if (doplot) {
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x, ylab = "x")
            plot(object, ylab = "rollVar(x)")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 9, 
            trim = TRUE, 
            na.rm = FALSE,
            doplot = TRUE),
        infoName = "Rolling Variance",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )        
}   


.fMultivar.RollingAnalysis.4 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Minimum:
    myFunction = function(n, trim, na.rm, doplot) {
        object <<- rollMin(x, n, trim, na.rm)
        if (doplot) {
            par(mfrow = c(1, 1), cex = 0.7)
            plot(x, ylab = "x")
            lines(object, col = "steelblue")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 9, 
            trim = TRUE, 
            na.rm = FALSE,
            doplot = TRUE),
        infoName = "Rolling Min",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )    
}   


.fMultivar.RollingAnalysis.5 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Maximum:
    myFunction = function(n, trim, na.rm, doplot) {
        object <<- rollMax(x, n, trim, na.rm)
        if (doplot) {
            par(mfrow = c(1, 1), cex = 0.7)
            plot(x, ylab = "x")
            lines(object, col = "steelblue")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 9, 
            trim = FALSE, 
            na.rm = FALSE,
            doplot = TRUE),
        infoName = "Rolling Max",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )    
}   


.fMultivar.RollingAnalysis.6 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... Save object to x:
    x <<- merge(x, object@Data)
    x <<- tkSaveAsX(data = x, infoName = "x and rolling series merged") 
}


################################################################################

