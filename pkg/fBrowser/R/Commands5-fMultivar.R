
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
    helpTopic <<- ""
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "lmData", report = report,
            FUN = "as.data.frame") 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "LM Data Set" )       
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.glmData = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # GLM / BINOMIAL / LOGIT - Example Data:
    # Example Data: x = 10*sin(x1) + exp(x2) + eps
    # family = binomial(link = logit)
    # glm(formula = x ~ x1 + x2, family = family, data = x)
    helpTopic <<- ""
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "glmData", report = report,
            FUN = "as.data.frame") 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "GLM Data Set" )      
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.gamData = 
function() 
{   # A function implemented by Diethelm Wuertz

    # GAM - Example Data:
    # Example Data: x = 0.7*sin(x1) + 0.3*exp(x2) + eps
    # fit = gam(formula = x ~ s(x1) + s(x2), data = x)  
    helpTopic <<- ""
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "gamData", report = report,
            FUN = "as.data.frame") 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "GAM Data Set" )         
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.lmFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Linear Modelling:
    helpTopic <<- "RegressionModelling"
    myFunction = function(series, formula, doplot, par, object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        object <<- regFit(formula = formula, data = x)
        if (doplot) {
            tkEval(par)
            plot(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",   
            doplot = TRUE, 
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = TRUE ), 
        subject = "LM Modelling" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.glmFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generalized Linear Modelling:
    helpTopic <<- "RegressionModelling"
    myFunction = function(series, formula, family, doplot, par, 
        object2x, report){
        x = tkEval(series)
        formula = as.formula(formula)
        family = tkEval(family)
        object <<- regFit(formula = formula, family = family, data = x, 
            method = "GLM")
        if (doplot) {
            tkEval(par)
            plot(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x", 
            formula = "x ~ x1 + x2",
            family = "gaussian()",
            doplot = TRUE, 
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = TRUE ), 
        subject = "GLM Modelling" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.gamFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generalized Additive Modelling
    helpTopic <<- "RegressionModelling"
    myFunction = function(series, formula, family, doplot, par, 
        object2x, report){
        x = tkEval(series)
        formula = as.formula(formula)
        family = tkEval(family)
        object <<- regFit(formula = formula, family = family, 
            data = x, method = "GAM")
        if (doplot) {
            tkEval(par)
            plot(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ s(x1) + s(x2)",
            family = "gaussian()",
            doplot = TRUE, 
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = TRUE ),  
        subject = "GAM Modelling" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.pprFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Projection Pursuit Regression:
    helpTopic <<- "RegressionModelling"
    myFunction = function(series, formula, nterms, doplot, par, 
        object2x, report){
        x = tkEval(series)
        formula = as.formula(formula)
        object <<- regFit(formula = formula, nterms = nterms,
            data = x, method = "PPR")
        if (doplot) {
            tkEval(par)
            plot(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            nterms = 2,
            doplot = TRUE, 
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = TRUE ), 
        subject = "PPR Modelling" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.marsFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # MARS Pursuit Regression:
    helpTopic <<- "RegressionModelling"
    myFunction = function(series, formula, doplot, par, object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        object <<- regFit(formula = formula, data = x, method = "MARS")
        if (doplot) {
            tkEval(par)
            plot(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            doplot = TRUE, 
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = TRUE ), 
        subject = "MARS Modelling" )
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.polymarsFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # POLYMARS Pursuit Regression:
    helpTopic <<- "RegressionModelling"
    myFunction = function(series, formula, doplot, par, object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        object <<- regFit(formula = formula, data = x, method = "POLYMARS")
        if (doplot) {
            tkEval(par)
            plot(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            doplot = TRUE, 
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = TRUE ), 
        subject = "POLYMARS Modelling" )  
}


# ------------------------------------------------------------------------------


.fMultivar.Regression.nnetFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Neural Network Regression:
    helpTopic <<- "RegressionModelling"
    myFunction = function(series, formula, size, doplot, par, 
        object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        object <<- regFit(formula = formula, data = x, method = "NNET", 
            size = size)
        if (doplot) {
            tkEval(par)
            plot(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            size = 4,
            doplot = TRUE, 
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = TRUE ), 
        subject = "NNET Modelling" ) 
}


################################################################################
# Regression Tests


.fMultivar.RegressionTests.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: realInvest ~ realGNP + realInterest
    helpTopic <<- ""
    data(Greene4Table131)
    Greene4Table131 = as.timeSeries(Greene4Table131[, c(1, 6:8)])
    colnames(Greene4Table131@Data) = c("x", "x1", "x2")
    x <<- tkSaveAsX(data = Greene4Table131,
        subject = "Green: x=realInvest | x1=realGNP x2=realInterest")
}


# ------------------------------------------------------------------------------


.fMultivar.RegressionTests.bgTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Breusch-Godfrey Test:
    helpTopic <<- "RegressionTests"
    myFunction = function(series, formula, order, type, object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        type = tkSplit(type)
        if (report) tkTitle("Breusch-Godfrey Test")
        object <<- bgTest(formula = formula, order = order, 
            type = type, data = x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            order = 1,
            type = "Chisq & F",
            object2x = FALSE,
            report = TRUE ),
        subject = "Breusch-Godfrey Test")
}


# ------------------------------------------------------------------------------


.fMultivar.RegressionTests.bpTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Breusch-Pagan Test:
    helpTopic <<- "RegressionTests"
    myFunction = function(series, formula, studentize, 
        object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        object <<- bpTest(formula = formula, varformula = NULL,
            studentize = studentize, data = x)
        if (report) tkTitle("Breusch-Pagan Test")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            studentize = TRUE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Breusch-Pagan Test" )
}


# ------------------------------------------------------------------------------


.fMultivar.RegressionTests.dwTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Durbin-Watson Test:
    helpTopic <<- "RegressionTests"
    myFunction = function(series, formula, alternative, iterations, 
        object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        alternative = tkSplit(alternative)
        object <<- dwTest(formula = formula, alternative = alternative,
            iterations = iterations, exact = NULL, tol = 1.0e-10, data = x)
        if (report) tkTitle("Durbin-Watson Test")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            alternative = "greater & two.sided & less",
            iterations = 15,
            object2x = FALSE,
            report = TRUE ),
        subject = "Durbin-Watson Test" )
}


# ------------------------------------------------------------------------------


.fMultivar.RegressionTests.gqTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Goldfeld - Quandt Test:
    helpTopic <<- "RegressionTests"
    myFunction = function(series, formula, point, 
        object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        object <<- gqTest(formula = formula, point = point, order.by =NULL,
            data = x)
        if (report) tkTitle("Goldfeld-Quandt Test")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            point = 0.5,
            object2x = FALSE,
            report = TRUE ),
        subject = "Goldfeld-Quandt Test" )
}


# ------------------------------------------------------------------------------


.fMultivar.RegressionTests.harvTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Harvey - Collier Test:
    helpTopic <<- "RegressionTests"
    myFunction = function(series, formula, order.by = NULL, 
        object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        object <<- harvTest(formula = formula, data = x)
        if (report) tkTitle("Harvey-Collier Test")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            object2x = FALSE,
            report = TRUE ),
        subject = "Harvey-Collier Test" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.RegressionTests.hmcTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Harrison - McCabe Test:
    helpTopic <<- "RegressionTests"
    myFunction = function(series, formula, point, simulate.p, nsim, doplot,
        object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        if (doplot) tkEval(par)
        object <<- hmcTest(formula = formula, point = point, order.by = NULL,
            simulate.p = simulate.p, nsim = nsim, plot = doplot, data = x)
        if (report) tkTitle("Harrison-McCabe Test")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            point = 0.5,
            simulate.p = TRUE,
            nsim = 1000,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = TRUE ),
        subject = "Harrison-McCabe Test" )
}


# ------------------------------------------------------------------------------


.fMultivar.RegressionTests.rainTest = 
function()
{
    # Rainbow Test:
    helpTopic <<- "RegressionTests"
    myFunction = function(series, formula, fraction, 
        object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        object <<- rainTest(formula = formula, fraction = fraction, 
            order.by = NULL, center = NULL, data = x)
        if (report) tkTitle("Rainbow Test")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            fraction = 0.5,
            object2x = FALSE,
            report = TRUE ),
        subject = "Rainbow Test")
}


# ------------------------------------------------------------------------------


.fMultivar.RegressionTests.resetTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Ramsey RESET Test:
    helpTopic <<- "RegressionTests"
    myFunction = function(series, formula, power, type,
        object2x, report) {
        x = tkEval(series)
        formula = as.formula(formula)
        power = tkEval(power)
        type = tkSplit(type)
        object <<- resetTest(formula = formula, power = power, type = type,
            data = x)
        if (report) tkTitle("Ramsey RESET Test")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            formula = "x ~ x1 + x2",
            power = "2:3",
            type = "fitted & regressor & princomp",
            object2x = FALSE,
            report = TRUE ),
        subject = "Ramsey RESET Test" ) 
}


################################################################################
# Equations Modelling


.fMultivar.EquationsModelling.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Equations Modelling:
    helpTopic <<- "EquationsModelling"
    tkinsert(txt, "end", "\n\nSorry, not yet implemented!\n")   
}


################################################################################
# Matrix Addon


.fMultivar.MatrixAddon.1 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Generate Pascal Matrix:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(n, object2x, report) {
        n = as.integer(n)
        object <<- pascal(n)
        colnames(object) = rownames(object) = LETTERS[1:5]
        if (report) tkTitle("Pascal Matrix")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 5,
            object2x = TRUE,
            report = TRUE ),
        subject = "Pascal Matrix")
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Return Diagonal Matrix:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, object2x, report) {
        x =  tkEval(matrix)
        object <<- diag(x)
        if (report) tkTitle("Diagonal Matrix")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Diagonal Matrix" ) 
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.3 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Lower Triangular Matrix:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, object2x, report) {
        x =  tkEval(matrix)
        object <<- triang(x)
        if (report) tkTitle("Lower Triangular Matrix")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Lower Triangular") 
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.4 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Upper Triangular Matrix:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, object2x, report) {
        x =  tkEval(matrix)
        object <<- Triang(x)
        if (report) tkTitle("Upper Triangular Matrix")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Upper Triangular" ) 
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.5 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Determinant:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, object2x, report) {
        x =  tkEval(matrix)
        object <<- det(x)
        if (report) tkTitle("Determinant")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Determinant" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.6 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Return Inverse Matrix:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, object2x, report) {
        x =  tkEval(matrix)
        object <<- inv(x)
        if (report) tkTitle("Inverse Matrix")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Inverse" ) 
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.7 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # norm(x, p) p-Norm:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, p, object2x, report) {
        x =  tkEval(matrix)
        p = as.numeric(tkSplit(p))
        object <<- norm(x, p)
        if (report) tkTitle("Norm of Matrix")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            p = c("2 & 1 & Inf"),
            object2x = FALSE,
            report = TRUE ),
        subject = "Norm" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.8 =
function()
{   # A function implemented by Diethelm Wuertz

    # rk(x, method) Rank:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, method, object2x, report) {
        x =  tkEval(matrix)
        method = tkSplit(method)
        object <<- rk(x = x, method = method)
        attr(object, "control") = method
        if (report) tkTitle("Rank of Matrix")
        object }
    tkExecute(
        fun = myFunction, 
        prototypes = list(,
            matrix = "x",
            method = "qr & chol",
            object2x = FALSE,
            report = TRUE ),
        subject = "Rank" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.9 =
function()
{   # A function implemented by Diethelm Wuertz

    # t(x) Transposed"
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, method, object2x, report) {
        x =  tkEval(matrix)
        object <<- t(x)
        if (report) tkTitle("Transpose of Matrix")
        object }
    tkExecute(
        fun = myFunction, 
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Transpose" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.10 =
function()
{   # A function implemented by Diethelm Wuertz

    # mexp(x, order, method) Exponentiate Square Matrix:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, order, method, report) {
        x =  tkEval(matrix)
        method = tkSplit(method)
        object <<- mexp(x = x, order = order, method = method)
        if (report) tkTitle("Exponentiated Matrix")
        object  }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            order = 8, 
            method = "pade & taylor",
            object2x = FALSE,
            report = TRUE ),
        subject = "Exponentiate Matrix" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.11 =
function()
{   # A function implemented by Diethelm Wuertz

    # chol(x, pivot) Cholesky Factors:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, pivot, object2x, report) {
        x =  tkEval(matrix)
        object <<- chol(x = x, pivot = pivot, LINPACK = pivot) 
        if (report) tkTitle("Cholesky Factors")
        object  }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            pivot = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Cholesky Factors" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.12 =
function()
{   # A function implemented by Diethelm Wuertz

    # eigen(x) Eigenvalues and Eigevectors:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, object2x, report) {
        x =  tkEval(matrix)
        object <<- eigen(x)
        if (report) tkTitle("Eigenvalues and Vectors")
        object  }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Eigenvalues and -vectors" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.13 =
function()
{   # A function implemented by Diethelm Wuertz

    # svd(x) Singular Value Decomposition:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, object2x, report) {
        x =  tkEval(matrix)
        object <<- svd(x)
        if (report) tkTitle("Singular Value Decomposition")
        object  }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "SV Decomposition" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.14 =
function()
{   # A function implemented by Diethelm Wuertz

    # kappa(x) Condition Number:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, object2x, report) {
        x =  tkEval(matrix)
        object <<- kappa(x)
        if (report) tkTitle("Condition Number")
        object  }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Condition Number" )
}
    

# ------------------------------------------------------------------------------

    
.fMultivar.MatrixAddon.15 =
function()
{   # A function implemented by Diethelm Wuertz

    # QR Decomposition:
    helpTopic <<- "VectorMatrixAddon"
    myFunction = function(matrix, object2x, report) {
        x =  tkEval(matrix)
        object <<- qr(x)
        if (report) tkTitle("QR Decomposition")
        object  }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            matrix = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "QR Decomposition" )
}


################################################################################
# Missing Values


.fMultivar.MissingValues.1 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = MSFT|SP500 Returns:
    helpTopic <<- "MissingValues"
    .tkGetData(Data = "msftsp500Monthly", subject = "Return Series with NA")  
    x <<- x[109:132, ]
    x@Data[1, 2] <<- NA
    x@Data[9, ] <<- c(NA, NA)
    x@Data[10, 2] <<- NA
    x@Data[24, 1] <<- NA
}


# ------------------------------------------------------------------------------


.fMultivar.MissingValues.removeNA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Remove NAs: 
    helpTopic <<- "MissingValues"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- removeNA(x = x) 
        if (report) tkTitle("NA's Removed")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x", 
            object2x = FALSE,
            report = TRUE ),
        subject = "Remove NAs" )   
}


# ------------------------------------------------------------------------------


.fMultivar.MissingValues.interpNA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Interpolate NAs:
    helpTopic <<- "MissingValues"
    myFunction = function(series, method, object2x, report) {
        x = tkEval(series)
        method = tkSplit(method)
        object <<- interpNA(x = x, method = method) 
        if (report) tkTitle("Interpolated NAs")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            method = "before & after & linear", 
            object2x = FALSE,
            report = TRUE ),
        subject = "Interpolate NAs" )      
}


# ------------------------------------------------------------------------------


.fMultivar.MissingValues.knnNA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # knn Algorithm:
    helpTopic <<- "MissingValues"
    myFunction = function(series, correlation, object2x, report) {
        x = tkEval(series)
        if (report) tkTitle("knn Algorithm")
        object <<- knnNA(x = x, k = max(dim(as.matrix(x))[1] * 0.01, 2), 
            correlation = correlation) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            correlation = FALSE, 
            object2x = FALSE,
            report = TRUE ),
        subject = "knn Algorithm" )          
}


# ------------------------------------------------------------------------------


.fMultivar.MissingValues.5 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = MSFT|SP500 Returns
    helpTopic <<- ""
    X = as.timeSeries(singleIndex.dat, format = "%d-%b-%Y")[108:132, ]
    X = returnSeries(X)
    X@units = paste(X@units, ".RET", sep = "")
    colnames(X@Data) = X@units
    X@FinCenter = "GMT"
    X@Data[1, 2] = NA
    X@Data[9, ] = c(NA, NA)
    X@Data[10, 2] = NA
    X@Data[24, 1] = NA
    x <<- tkSaveAsX(data = X, subject = "MSFT | SP500 Returns")
}


# ------------------------------------------------------------------------------


.fMultivar.MissingValues.6 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Substitute NAs:
    helpTopic <<- ""
    myFunction = function(series, type, object2x, report) {
        x = tkEval(series)
        substituteNA(x = x, type = type) }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            type = "zeros", 
            object2x = FALSE,
            report = TRUE ),
        subject = "Substitute NAs" )      
}


################################################################################
# Technical Analysis


.fMultivar.TechnicalAnalysis.emaTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # emaTA - Exponential Moving Average
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag, doplot, par, col, merge2x,
        object2x, report) {
        x = tkEval(series)
        select = tkSplit(select)
        object <<- .dailyTA(X = x, indicator = "ema", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            what = "Exponential Moving Average"          
            plot(object, ylab = paste("EMA", select), main = what, col = col)
        }
        if (report) tkTitle("Exponential Moving Average")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = "Close & Open & High & Low",  
            lag = 9,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Exponential Moving Average" )   
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.priceTA =
function()
{   # A function implemented by Diethelm Wuertz

    # Price Averages:
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, doplot, par, col, merge2x, 
        object2x, report) {
        x = tkEval(series)
        medprice = .dailyTA(X = x, indicator = "medprice", 
            select = NULL, lag = NULL)
        typicalprice = .dailyTA(X = x, indicator = "typicalprice", 
            select = NULL, lag = NULL)
        wclose = .dailyTA(X = x, indicator = "wclose", 
            select = NULL, lag = NULL)
        object <<- mergeSeries(medprice, cbind(typicalprice@Data, wclose@Data))
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            # Median Price
            what = "Median Price"
            plot(object[,1], ylab = "(High+Low)/2", main = what)
            # Typical Price
            what = "Typical Price"
            plot(object[,2], ylab = "(High+Low+Close)/3", main = what)
            # Typical Price
            what = "Weighted Closing Price"
            plot(object[,3], ylab = "(High+Low+2*Close)/4", main = what)
        }
        if (report) tkTitle("Price Indicators")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            doplot = TRUE,
            par = "par(mfrow=c(3,1),cex=0.7)",
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Price Averages" )
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.rocTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # rocTA - Rate of Change
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag, doplot, par, merge2x, 
        object2x, report) {
        x = tkEval(series)
        select = tkSplit(select)
        object <<- .dailyTA(X = x, indicator = "roc", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            # Price:
            what = select
            plot(x[, select], ylab = what, main = what)
            # Rate of Change:
            what = "Rate of Change"
            plot(object, ylab = what, main = what, col = "red")
        }
        if (report) tkTitle("Rate of Change")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = "Close & Open & High & Low", 
            lag = 9,
            doplot = TRUE,
            par = "par(mfrow=c(2,1),cex=0.7)",
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Rate of Change")  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.oscTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # oscTA - EMA Oscillator
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag1, lag2, doplot, par, col, merge2x, 
        object2x, report) {
        x = tkEval(series)
        select = tkSplit(select)
        object <<- .dailyTA(X = x, indicator = "osc", 
            select = select, lag = c(lag1, lag2))
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            what = "EMA Oscillator"
            plot(object, ylab = what, main = what, col = col)
        }
        if (report) tkTitle("EMA Oscillator")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = "Close & Open & High & Low",  
            lag1 = 9,
            lag2 = 23,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "EMA Oscillator" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.momTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Momentum measures the speed of price change and provides a 
    # leading indicator of changes in trend.
    
    # momTA - Momentum Oscillator
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag, doplot, par, col, merge2x, 
        object2x, report) {
        x = tkEval(series)
        select = tkSplit(select)
        object <<- .dailyTA(X = x, indicator = "mom", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            what = "Momentum Oscillator"
            plot(object, ylab = "Momentum", main = what, col = col)
        }
        if (report) tkTitle("Momentum Oscillator")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = "Close & Open & High & Low",
            lag = 9,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Momentum Oscillator" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.macdTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # macdTA -  MACD Oscillator
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag1, lag2, lag3, doplot, par, 
        merge2x, object2x, report) {
        x = tkEval(series)
        select = tkSplit(select)
        # MACD Indicator:
        macd = .dailyTA(X = x, indicator = "macd", 
            select = select, lag = c(lag1, lag2))
        # Signal Line:
        cds = .dailyTA(X = x, indicator = "cds", 
            select = select, lag = c(lag1, lag2, lag3))
        # Oscillator:
        cdo = .dailyTA(X = x, indicator = "cdo", 
            select = select, lag = c(lag1, lag2, lag3))   
        object <<- mergeSeries(macd, cbind(cds@Data, cdo@Data))   
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            # Price | Index:
            what = select
            plot(x[, select], ylab = select, main = what)
            # Add Points at Crossings:
            z = object[,1]-object[,2]
            z@Data = sign(z@Data)
            z = diffSeries(z)
            z@Data[1] = 0
            X = x[, select]
            X = X[z@Data !=0]
            points(X, pch = 19, col = "red")
            # MACD Signal Line | Slow MACD:
            what = "Signal Line | Slow MACD"
            plot(object[, 1], ylab = select, main = what, col = "black")
            lines(object[, 2], col = "darkgreen")
            X = object[, 1]
            X = X[z@Data !=0]
            points(X, pch = 19, col = "red")
            abline(h = 0, col = "grey")
            # MACD Histogram:
            what = "MACD Histogram"
            plot(object[, 3], type = "h", main = what, col = "steelblue")
            lines(object[, 3])
            X = object[, 3]
            X = X[z@Data !=0]
            points(X, pch = 19, col = "red")
            abline(h = 0)
        }
        if (report) tkTitle("MACD Oscillator")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = "Close & Open & High & Low", 
            lag1 = 12,
            lag2 = 26,
            lag3 = 9,
            doplot = TRUE,
            par = "par(mfrow=c(3,1),cex=0.7)",
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "MACD Oscillator" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.volatilityTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # vohlTA - High/Low Volatility
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, doplot, par, merge2x, 
        object2x, report) {
        x = tkEval(series)
        vohl = .dailyTA(X = x, indicator = "vohl", 
            select = NULL, lag = NULL)
        vor = .dailyTA(X = x, indicator = "vor", 
            select = NULL, lag = NULL)
        object <<- mergeSeries(vohl, vor@Data)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            # High/Low Volatility
            what = "High/Low Volatility"
            plot(object[, 1], ylab = "High-Low", main = what)
            # vorTA - Volatility Ratio
            what = "Volatility Ratio"
            plot(object[, 2], ylab = "(High-Low)/Low", main = what)
        }
        if (report) tkTitle("Volatility Indicators")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            doplot = TRUE,
            par = "par(mfrow=c(2,1),cex=0.7)",
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Volatility Indicators" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.stochasticTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # stochasticTA - Fast Stochastics
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, lag1, lag2, type, doplot, par, merge2x, 
        object2x, report) {
        x = tkEval(series)
        type = tkSplit(type)
        close = as.vector(x[, "Close"])
        high = as.vector(x[, "High"])
        low = as.vector(x[, "Low"])
        object <<- x
        object@Data = stochasticTA(close, high, low, lag1, lag2, type)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par)
            # Close:
            what = "Close" 
            plot(x[,"Close"], ylab = "Close", main = what)
            # Stochastics:
            what = "Stochastic"
            plot(object[, 1], ylab = what, main = what)
            lines(object[, 2], col = "red")
            abline(h = 20, col = "steelblue")
            abline(h = 80, col = "steelblue")
            
        }
        if (report) tkTitle("Stochastics")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            lag1 = 10,
            lag2 = 3,
            type = "fast & slow",
            doplot = TRUE,
            par = "par(mfrow=c(2,1),cex=0.7)",
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Stochastics Oscillator" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.fpkTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # fpkTA - Fast %K Stochastics
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag, doplot, par, col, merge2x, 
        object2x, report) {
        x = tkEval(series)
        object <<- .dailyTA(X = x, indicator = "fpk", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            what = "Fast %K Stochastics"
            plot(object, ylab = what, main = what, col = col)
        }
        if (report) tkTitle("Fast %K Stochastics")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = ..., 
            lag = ...,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Fast %K Stochastics" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.fpdTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # fpdTA - Fast %D Stochastics
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag, doplot, par, col, merge2x, 
        object2x, report) {
        x = tkEval(series)
        object <<- .dailyTA(X = x, indicator = "fpd", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            what = "Fast %D Stochastics"
            plot(object, ylab = what, main = what, col = col)
        }
        if (report) tkTitle("Fast %D Stochastics")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = ..., 
            lag = ...,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Fast %D Stochastics" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.spdTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # spdTA - Slow %D Stochastics
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag, doplot, par, col, merge2x, 
        object2x, report) {
        x = tkEval(series)
        object <<- .dailyTA(X = x, indicator = "spd", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            what = "Slow %D Stochastics"
            plot(object, ylab = what, main = what, col = col)
        }
        if (report) tkTitle("Slow %D Stochastic")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = ..., 
            lag = ...,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Slow %D Stochastics" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.apdTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # apdTA - Averaged %D Stochastics
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag, doplot, par, col, merge2x, 
        object2x, report) {
        x = tkEval(series)
        object <<- .dailyTA(X = x, indicator = "apd", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            what = "Averaged %D Stochastics"
            plot(object, ylab = what, main = what, col = col)
        }
        if (report) tkTitle("Averaged %D Stochastics")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = ..., 
            lag = ...,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Averaged %D Stochastics" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.wprTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Williams %R has proven very useful for anticipating market reversals.

    # wprTA - Williams %R Stochastics
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, lag, doplot, par, merge2x, 
        object2x, report) {
        x = tkEval(series)
        object <<- .dailyTA(X = x, indicator = "wpr", select = NULL, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            # Close:
            what = "Close"
            plot(x[, "Close"], ylab = what, main = what)
            # %R
            what = "Williams %R Stochastics"
            plot(object, ylab = "%R", main = what, col = "red")
            abline(h = 0.20, col = "steelblue")
            abline(h = 0.80, col = "steelblue") 
            abline(h = 0.10, col = "darkgreen")
            abline(h = 0.90, col = "darkgreen")   
        }
        if (report) tkTitle("Williams %R Stochastics")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            lag = 14,
            doplot = TRUE,
            par = "par(mfrow=c(2,1),cex=0.7)",
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Williams %R Stochastics" )  
}


.wprSlider =  
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        lag = .sliderMenu(no = 1)
        
        # Compute Data:  
        # x is global availalble
        object <<- .dailyTA(X = x, indicator = "wpr", select = NULL, lag = lag)
         
        # Frame:
        par(mfrow = c(2, 1), cex = 0.7)
        
        # Plot - Close:
        what = "Close"
        plot(x[, "Close"], ylab = what, main = what)
        
        # Plot - %R
        what = "Williams %R Stochastics"
        plot(object[,1], ylab = "%R", main = what, col = "red")
        abline(h = 0.20, col = "steelblue")
        abline(h = 0.80, col = "steelblue") 
        abline(h = 0.10, col = "darkgreen")
        abline(h = 0.90, col = "darkgreen")   
        
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c( "lag"),
       minima =      c(    1 ),
       maxima =      c(   52 ),
       resolutions = c(    1 ),
       starts =      c(   14 )
    )
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.rsiTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # RSI can provide an early warning of an opportunity to buy or sell.
    
    # rsiTA - Relative Strength Index
    helpTopic <<- "TechnicalAnalysis"
    myFunction = function(series, select, lag, doplot, par, merge2x, 
        object2x, report) {
        x = tkEval(series)
        select = tkSplit(select)
        object <<- .dailyTA(X = x, indicator = "rsi", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            tkEval(par) 
            # Price:
            what = select
            plot(x[, select], ylab = what, main = what)
            # ylim does not work - still to fix
            # RSI:
            what = "Relative Strength Index"
            z = object[c(1, dim(object@Data)[1])]
            z@Data[,1] = c(0,1)
            plot(z, type = "n", ylab = "RSI", main = what)
            lines(object, col = "red")
            abline(h = 0.3, col = "steelblue")
            abline(h = 0.7, col = "steelblue")
        }
        if (report) tkTitle("Relative Strength Index")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            select = "Close & Open & High & Low", 
            lag = 14,
            doplot = TRUE,
            par = "par(mfrow=c(2,1),cex=0.7)",
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Relative Strength Index" )  
}


################################################################################
# Benchmark Analysis


.fMultivar.BenchmarkAnalysis.maxDrawDown = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Maximum Draw-Down:
    helpTopic <<- "BenchmarkAnalysis"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- maxDrawDown(x) 
        if (report) tkTitle("Maximum Draw-Down")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x", 
            object2x = FALSE,
            report = TRUE ),
        subject = "maxDrawDown" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.BenchmarkAnalysis.sharpeRatio = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Sharpe Ratio:
    helpTopic <<- "BenchmarkAnalysis"
    myFunction = function(series, r, scale, object2x, report) {
        x = tkEval(series)
        scale = eval(parse(text = scale))
        object <<- sharpeRatio(x = x, r = r, scale = scale) 
        if (report) tkTitle("Sharpe Ratio")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x", 
            r = 0, 
            scale = "sqrt(12)", 
            object2x = FALSE,
            report = TRUE ),
        subject = "Sharpe Ratio" ) 
}


# ------------------------------------------------------------------------------


.fMultivar.BenchmarkAnalysis.sterlingRatio = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Sterling Ratio:
    helpTopic <<- "BenchmarkAnalysis"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- sterlingRatio(x = x) 
        if (report) tkTitle("Sterling Ratio")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x", 
            object2x = FALSE,
            report = TRUE ),
        subject = "Sterling Ratio" ) 
}


################################################################################
# Rolling Analysis


.fMultivar.RollingAnalysis.rollMean = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Mean:
    helpTopic <<- "RollingAnalysis"
    myFunction = function(series, n, trim, na.rm, doplot, par, 
        object2x, report) {
        if (report) tkTitle("Rolling Mean")
        x = tkEval(series)
        object <<- rollMean(x, n, trim, na.rm)
        if (doplot) {
            eval(parse(text = par))
            plot(x, ylab = "x", main = "Rolling Mean")
            lines(object, col = "steelblue")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            n = 9, 
            trim = TRUE, 
            na.rm = FALSE,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            pbject2x = FALSE,
            report = TRUE ),
        subject = "Rolling Mean" )      
}   


# ------------------------------------------------------------------------------


.fMultivar.RollingAnalysis.rollVar = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Variance:
    helpTopic <<- "RollingAnalysis"
    myFunction = function(series, n, trim, na.rm, doplot, par, 
        object2x, report) {
        if (report) tkTitle("Rolling Variance")
        x = tkEval(series)
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
        prototypes = list(
            series = "x",
            n = 9, 
            trim = TRUE, 
            na.rm = FALSE,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            pbject2x = FALSE,
            report = TRUE ),
        subject = "Rolling Variance" )        
}   


# ------------------------------------------------------------------------------


.fMultivar.RollingAnalysis.rollMin = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Minimum:
    helpTopic <<- "RollingAnalysis"
    myFunction = function(series, n, trim, na.rm, doplot, par, 
        object2x, report) {
        if (report) tkTitle("Rolling Minimum")
        x = tkEval(series)
        object <<- rollMin(x, n, trim, na.rm)
        if (doplot) {
            eval(parse(text = par))
            plot(x, ylab = "x", main = "Rolling Minimum")
            lines(object, col = "steelblue")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            n = 9, 
            trim = TRUE, 
            na.rm = FALSE,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            pbject2x = FALSE,
            report = TRUE ),
        subject = "Rolling Min" )    
}   


# ------------------------------------------------------------------------------


.fMultivar.RollingAnalysis.rollMax = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Rolling Maximum:
    helpTopic <<- "RollingAnalysis"
    myFunction = function(series, n, trim, na.rm, doplot, par, 
        object2x, report) {
        if (report) tkTitle("Rolling Maximum")
        x = tkEval(series)
        object <<- rollMax(x, n, trim, na.rm)
        if (doplot) {
            eval(parse(text = par))
            plot(x, ylab = "x")
            lines(object, col = "steelblue", main = "Rolling Maximum")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            n = 9, 
            trim = FALSE, 
            na.rm = FALSE,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            pbject2x = FALSE,
            report = TRUE ),
        subject = "Rolling Max" )    
}   


################################################################################

