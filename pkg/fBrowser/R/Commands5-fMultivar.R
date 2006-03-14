
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


.fMultivar.Regression.lmData = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # LM - Example Data:
    # Example Data: x = 0.7*x1 + 0.3*x2 + eps
    tkGetDataFrame("lmData", "lm Demo Data")
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.glmData = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # GLM / BINOMIAL / LOGIT - Example Data:
    # Example Data: x = 10*sin(x1) + exp(x2) + eps
    # family = binomial(link = logit)
    # glm(formula = x ~ x1 + x2, family = family, data = x)
    tkGetDataFrame("glmData", "glm Demo Data")
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.gamData = 
function() 
{   # A function implemented by Diethelm Wuertz

    # GAM - Example Data:
    # Example Data: x = 0.7*sin(x1) + 0.3*exp(x2) + eps
    # fit = gam(formula = x ~ s(x1) + s(x2), data = x)  
    tkGetDataFrame("gamData", "gam Demo Data")   
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.lmFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Linear Modelling:
    myFunction = function(formula, family, object2x, report) {
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "LM", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            family = "gaussian",
            object2x = FALSE,
            report = TRUE ), 
        infoName = "LM Modelling" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.glmFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generalized Linear Modelling:
    myFunction = function(formula, family, object2x, report){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "GLM", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ x1 + x2",
            family = "gaussian",
            object2x = FALSE,
            report = TRUE ), 
        infoName = "GLM Modelling" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.gamFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generalized Additive Modelling
    myFunction = function(formula, object2x, report){
        object <<- regFit(
            formula = as.formula(formula), 
            family = gaussian(), 
            data = x, method = "GAM", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ s(x1) + s(x2)",
            object2x = FALSE,
            report = TRUE ),  
        infoName = "GAM Modelling" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.pprFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Projection Pursuit Regression:
    myFunction = function(formula, object2x, report){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "PPR", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "Y ~ X1 + X2",
            object2x = FALSE,
            report = TRUE ), 
        infoName = "PPR Modelling" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.marsFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # MARS Pursuit Regression:
    myFunction = function(formula, object2x, report){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "MARS", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "Y ~ X1 + X2",
            object2x = FALSE,
            report = TRUE ), 
        infoName = "MARS Modelling" )
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.polymarsFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # POLYMARS Pursuit Regression:
    myFunction = function(formula, object2x, report){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "POLYMARS", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "Y ~ X1 + X2",
            object2x = FALSE,
            report = TRUE ), 
        infoName = "POLYMARS Modelling" )  
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.nnetFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Neural Network Regression:
    myFunction = function(formula, object2x, report){
        object <<- regFit(
            formula = as.formula(formula),
            family = eval(parse(text = "gaussian")), 
            data = x, method = "NNET", nterms = NA, size = NA)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "Y ~ X1 + X2",
            object2x = FALSE,
            report = TRUE ), 
        infoName = "NNET Modelling" ) 
}


################################################################################
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


# ------------------------------------------------------------------------------


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
        infoName = "Breusch - Godfrey Test")
}


# ------------------------------------------------------------------------------


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
            studentize = TRUE ),
        infoName = "Breusch - Pagan Test" )
}


# ------------------------------------------------------------------------------


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
        infoName = "Durbin - Watson Test" )
}


# ------------------------------------------------------------------------------


.fMultivar.RegressionTests.5 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Goldfeld - Quandt Test:
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
        infoName = "Goldfeld - Quandt Test" )
}


# ------------------------------------------------------------------------------


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
        infoName = "Harvey - Collier Test" ) 
}


# ------------------------------------------------------------------------------


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
        infoName = "Harrison - McCabe Test" )
}


# ------------------------------------------------------------------------------


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
        infoName = "Rainbow Test")
}


# ------------------------------------------------------------------------------


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
        infoName = "Ramsey RESET Test" ) 
}


################################################################################
# Equations Modelling


.fMultivar.EquationsModelling.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Equations Modelling:
    tkinsert(txt, "end", "\n\nSorry, not yet implemented!\n")   
}


################################################################################
# Matrix Addon


.fMultivar.MatrixAddon.1 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Generate Pascal Matrix:
    myFunction = function(n, object2x, report) {
        n = as.integer(n)
        object <<- pascal(n)
        colnames(object) = rownames(object) = LETTERS[1:5]
        if (report) tkTitle("Pascal Matrix")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 5,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Pascal Matrix")
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Return Diagonal Matrix:
    myFunction = function(object2x, report) {
        object <<- diag(x)
        if (report) tkTitle("Diagonal Matrix")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE,
            report = TRUE ),
        infoName = "Diagonal Matrix" ) 
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.3 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Lower Triangular Matrix:
    myFunction = function(object2x, report) {
        object <<- triang(x)
        if (report) tkTitle("Lower Triangular Matrix")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE,
            report = TRUE ),
        infoName = "Lower Triangular") 
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.4 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Upper Triangular Matrix:
    myFunction = function(object2x, report) {
        object <<- Triang(x)
        if (report) tkTitle("Upper Triangular Matrix")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE,
            report = TRUE ),
        infoName = "Upper Triangular" ) 
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.5 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Determinant:
    myFunction = function(object2x, report) {
        object <<- det(x)
        if (report) tkTitle("Determinant")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE,
            report = TRUE ),
        infoName = "Determinant" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.6 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Inverse Matrix:
    myFunction = function(object2x, report) {
        object <<- inv(x)
        if (report) tkTitle("Inverse Matrix")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = FALSE,
            report = TRUE ),
        infoName = "Inverse" ) 
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.7 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # norm(x, p) p-Norm:
    myFunction = function(p, object2x, report) {
        p = as.integer(p)
        object <<- norm(x, p)
        if (report) tkTitle("Norm of Matrix")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            p = 2,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Norm" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.8 =
function()
{   # A function implemented by Diethelm Wuertz

    # rk(x, method) Rank:
    myFunction = function(method, object2x, report) {
        object <<- rk(x = x, method = method)
        if (report) tkTitle("Rank of Matrix")
        object }
    tkExecute(
        fun = myFunction, 
        params = list(,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Rank" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.9 =
function()
{   # A function implemented by Diethelm Wuertz

    # t(x) Transposed"
    myFunction = function(method, object2x, report) {
        object <<- t(x)
        if (report) tkTitle("Transpose of Matrix")
        object }
    tkExecute(
        fun = myFunction, 
        params = list(
            object2x = FALSE,
            report = TRUE ),
        infoName = "Transpose" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.10 =
function()
{   # A function implemented by Diethelm Wuertz

    # mexp(x, order, method) Exponentiate Square Matrix:
    myFunction = function(order, method, report) {
        object <<- mexp(x = x, order = order, method = method, object2x)
        object  }
    tkExecute(
        fun = myFunction,
        params = list(
            order = 8, 
            method = "pade",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Exponentiate" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.11 =
function()
{   # A function implemented by Diethelm Wuertz

    # chol(x, pivot) Cholesky Factors:
    myFunction = function(pivot, object2x, report) {
        object <<- chol(x = x, pivot = pivot, LINPACK = pivot) 
        object  }
    tkExecute(
        fun = myFunction,
        params = list(
            pivot = FALSE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Cholesky Factors" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.12 =
function()
{   # A function implemented by Diethelm Wuertz

    # eigen(x) Eigenvalues and Eigevectors:
    myFunction = function(object2x, report) {
        object <<- eigen(x)
        object  }
    tkExecute(
        fun = myFunction,
        params = list(
            pivot = FALSE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Eigenvalues and -vectors" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.13 =
function()
{   # A function implemented by Diethelm Wuertz

    # svd(x) Singular Value Decomposition:
    myFunction = function(object2x, report) {
        object <<- svd(x)
        object  }
    tkExecute(
        fun = myFunction,
        params = list(
            pivot = FALSE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "SV Decomposition" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.14 =
function()
{   # A function implemented by Diethelm Wuertz

    # kappa(x) Condition Number:
    myFunction = function(object2x, report) {
        object <<- kappa(x)
        object  }
    tkExecute(
        fun = myFunction,
        params = list(
            pivot = FALSE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Condition Number" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.15 =
function()
{   # A function implemented by Diethelm Wuertz

    # QR Decomposition:
    myFunction = function(object2x, report) {
        object <<- qr(x)
        object  }
    tkExecute(
        fun = myFunction,
        params = list(
            pivot = FALSE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "QR Decomposition" )
}


################################################################################
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


# ------------------------------------------------------------------------------


.fMultivar.MissingValues.2 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Remove NAs: 
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- removeNA(x = x) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Remove NAs" )   
}


# ------------------------------------------------------------------------------


.fMultivar.MissingValues.3 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Interpolate NAs:
    myFunction = function(series, method, object2x, report) {
        x = tkEval(series)
        object <<- interpNA(x = x, method = method) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            method = "before", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Interpolate NAs" )      
}


# ------------------------------------------------------------------------------


.fMultivar.MissingValues.4 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # knn Algorithm:
    myFunction = function(series, correlation, object2x, report) {
        x = tkEval(series)
        object <<- knnNA(x = x, k = max(dim(as.matrix(x))[1] * 0.01, 2), 
            correlation = correlation) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            correlation = FALSE, 
            object2x = FALSE,
            report = TRUE ),
        infoName = "knn Algorithm" )          
}


# ------------------------------------------------------------------------------


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


# ------------------------------------------------------------------------------


.fMultivar.MissingValues.6 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Substitute NAs:
    myFunction = function(series, type, object2x, report) {
        x = tkEval(series)
        substituteNA(x = x, type = type) }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            type = "zeros", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Substitute NAs" )      
}


################################################################################
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


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.2 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # emaTA - Exponential Moving Average
    myFunction = function(select, lag, doplot, col, object2x, report) {
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
            col = "red", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "EMA Indicator" )   
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # biasTA - EMA Price Bias
    myFunction = function(select, lag, doplot, col, object2x, report) {
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
            col = "red", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "EMA Price Bias" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.4 =
function()
{   # A function implemented by Diethelm Wuertz

    # medpriceTA - Median Price
    myFunction = function(select, lag, doplot, col, object2x, report) {
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
            col = "red", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Median Price" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.5 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # typicalpriceTA - Typical Price
    myFunction = function(select, lag, doplot, col, object2x, report) {
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
            col = "red", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Typical Price" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.6 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # wcloseTA - Weighted Close Price
    myFunction = function(select, lag, doplot, col, object2x, report) {
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
            col = "red", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Weighted Closing Price" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.7 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # rocTA - Rate of Change
    myFunction = function(select, lag, doplot, col, object2x, report) {
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
            col = "red", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Rate of Change")  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.8 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # oscTA - EMA-Oscillator
    myFunction = function(select, lag1, lag2, doplot, col, object2x, report) {
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
            col = "red", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "EMA-Oscillator" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.9 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # momTA - Momentum Oscillator
    myFunction = function(select, lag, doplot, col, object2x, report) {
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
            col = "red", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Momentum Oscillator" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.10 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # macdTA -  MACD Oscillator
    myFunction = function(select, lag1, lag2, doplot, col, object2x, report) {
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
            col = "red", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "MACD Oscillator" )  
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
        infoName = "... TA Indicator" )
}


# ------------------------------------------------------------------------------


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
        infoName = "... TA Indicator" )
}


# ------------------------------------------------------------------------------


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
        infoName = "... TA Indicator" )  
}


# ------------------------------------------------------------------------------


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
        infoName = "... TA Indicator" )  
}


# ------------------------------------------------------------------------------


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
        infoName = "... TA Indicator" )  
}


# ------------------------------------------------------------------------------


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
        infoName = "... TA Indicator" )  
}


# ------------------------------------------------------------------------------


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
        infoName = "... TA Indicator" )  
}


# ------------------------------------------------------------------------------


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
        infoName = "... TA Indicator" )  
}


# ------------------------------------------------------------------------------


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
        infoName = "... TA Indicator" )  
}


# ------------------------------------------------------------------------------


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
        infoName = "... TA Indicator" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.21 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... merge time Series
    x <<- tkSaveAsX(mergeSeries(x, object@Data), "Merged Series & Indicator")
}


################################################################################
# Benchmark Analysis


.fMultivar.BenchmarkAnalysis.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = SP500 Index
    tkGetData(Data = "sp500IndexMonthly", infoName = "Monthly SP500 Index")
}


# ------------------------------------------------------------------------------


.fMultivar.BenchmarkAnalysis.2 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Maximum Draw-Down:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- maxDrawDown(x) 
        if (report) tkTitle("Maximum Draw-Down")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "maxDrawDown",
        tkoutput = TRUE,
        console = NULL,
        title = "Maximum Drawdown",
        description = NULL ) 
}


# ------------------------------------------------------------------------------


.fMultivar.BenchmarkAnalysis.3 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Sharpe Ratio:
    myFunction = function(series, r, scale, object2x, report) {
        x = tkEval(series)
        scale = eval(parse(text = scale))
        object <<- sharpeRatio(x = x, r = r, scale = scale) 
        if (report) tkTitle("Sharpe Ratio")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x", 
            r = 0, 
            scale = "sqrt(12)", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Sharpe Ratio",
        tkoutput = TRUE,
        console = NULL,
        title = "Sharpe Ratio",
        description = NULL ) 
}


# ------------------------------------------------------------------------------


.fMultivar.BenchmarkAnalysis.4 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Sterling Ratio:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- sterlingRatio(x = x) 
        if (report) tkTitle("Sterling Ratio")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Sterling Ratio",
        tkoutput = TRUE,
        console = NULL,
        title = "Sterling Ratio",
        description = NULL ) 
}


################################################################################
# Rolling Analysis


.fMultivar.RollingAnalysis.sp500IndexMonthly = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = SP500 Index
    tkGetData(Data = "sp500IndexMonthly", infoName = "Monthly SP500 Index")
}   


# ------------------------------------------------------------------------------


.fMultivar.RollingAnalysis.rollMean = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Mean:
    myFunction = function(n, trim, na.rm, doplot, par, object2x, report) {
        if (report) tkTitle("Rolling Mean")
        object <<- rollMean(x, n, trim, na.rm)
        if (doplot) {
            eval(parse(text = par))
            plot(x, ylab = "x", main = "Rolling Mean")
            lines(object, col = "steelblue")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 9, 
            trim = TRUE, 
            na.rm = FALSE,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            pbject2x = FALSE,
            report = TRUE ),
        infoName = "Rolling Mean" )      
}   


# ------------------------------------------------------------------------------


.fMultivar.RollingAnalysis.rollVar = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Variance:
    myFunction = function(n, trim, na.rm, doplot, par, object2x, report) {
        if (report) tkTitle("Rolling Variance")
        object <<- rollVar(x, n, trim, na.rm)
        if (doplot) {
            eval(parse(text = par))
            plot(x, ylab = "x", col = "steelblue", main = "Rolling Variance")
            plot(object, ylab = "rollVar(x)")
            abline(h = 0)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 9, 
            trim = TRUE, 
            na.rm = FALSE,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            pbject2x = FALSE,
            report = TRUE ),
        infoName = "Rolling Variance" )        
}   


# ------------------------------------------------------------------------------


.fMultivar.RollingAnalysis.rollMin = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Minimum:
    myFunction = function(n, trim, na.rm, doplot, par, object2x, report) {
        if (report) tkTitle("Rolling Minimum")
        object <<- rollMin(x, n, trim, na.rm)
        if (doplot) {
            eval(parse(text = par))
            plot(x, ylab = "x", main = "Rolling Minimum")
            lines(object, col = "steelblue")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 9, 
            trim = TRUE, 
            na.rm = FALSE,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            pbject2x = FALSE,
            report = TRUE ),
        infoName = "Rolling Min" )    
}   


# ------------------------------------------------------------------------------


.fMultivar.RollingAnalysis.rollMax = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Maximum:
    myFunction = function(n, trim, na.rm, doplot, par, object2x, report) {
        if (report) tkTitle("Rolling Maximum")
        object <<- rollMax(x, n, trim, na.rm)
        if (doplot) {
            eval(parse(text = par))
            plot(x, ylab = "x")
            lines(object, col = "steelblue", main = "Rolling Maximum")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 9, 
            trim = FALSE, 
            na.rm = FALSE,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            pbject2x = FALSE,
            report = TRUE ),
        infoName = "Rolling Max" )    
}   


################################################################################

