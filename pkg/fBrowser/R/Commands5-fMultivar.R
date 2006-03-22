
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
    helpTopic <<- "regFit"
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
    helpTopic <<- "regFit"
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
    helpTopic <<- "regFit"
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
    helpTopic <<- "regFit"
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
    helpTopic <<- "regFit"
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
    helpTopic <<- "regFit"
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
    helpTopic <<- "regFit"
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
    helpTopic <<- "bgTest"
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
    helpTopic <<- "bpTest"
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
    helpTopic <<- "dwTest"
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
    helpTopic <<- "gqTest"
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
    helpTopic <<- "harvTest"
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
    helpTopic <<- "hmcTest"
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
    helpTopic <<- "rainTest"
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
    helpTopic <<- "resetTest"
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
    helpTopic <<- ""
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


.fMultivar.MissingValues.2 =
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


.fMultivar.MissingValues.3 =
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


.fMultivar.MissingValues.4 =
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
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x,
        object2x, report) {
        select = tkSplit(select)
        object <<- .dailyTA(X = x, indicator = "ema", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
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
        prototypes = list(
            select = "Close & Open & High & Low",  
            lag = 9,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Exponential Moving Average" )   
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.biasTA =
function()
{   # A function implemented by Diethelm Wuertz

    # biasTA - EMA Price Bias
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        select = tkSplit(select)
        object <<- .dailyTA(X = x, indicator = "bias", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "EMA Price Bias"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = "Bias", main = "Bias", col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = "Close & Open & High & Low", 
            lag = 9,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "EMA Price Bias" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.medpriceTA =
function()
{   # A function implemented by Diethelm Wuertz

    # medpriceTA - Median Price
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "medprice", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
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
        prototypes = list(
            select = "Close & Open & High & Low", 
            lag = 9,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Median Price" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.typicalpriceTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # typicalpriceTA - Typical Price
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "typicalprice", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
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
        prototypes = list(
            select = "Close & Open & High & Low", 
            lag = 9,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Typical Price" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.wcloseTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # wcloseTA - Weighted Close Price
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "wclose", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
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
        prototypes = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Weighted Closing Price" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.rocTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # rocTA - Rate of Change
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "roc", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "Rate of Change"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
            col = "red", 
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
    helpTopic <<- ""
    myFunction = function(select, lag1, lag2, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "osc", 
            select = select, lag = c(lag1, lag2))
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "EMA Oscillator"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = "Close", 
            lag1 = 9,
            lag2 = 23,
            doplot = TRUE,
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
    
    # momTA - Momentum Oscillator
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "mom", 
            select = select, lag = lag)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "Momentum Oscillator"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = "Close", 
            lag = 9,
            doplot = TRUE,
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
    helpTopic <<- ""
    myFunction = function(select, lag1, lag2, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "macd", 
            select = select, lag = c(lag1, lag2))
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
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
        prototypes = list(
            select = "Close", 
            lag1 = 9,
            lag2 = 23,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "MACD Oscillator" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.cdsTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # cdsTA - MACD Signal Line
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "cds", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "MACD Signal Line"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "MACD Signal Line" )
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.cdoTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # cdoTA - MACD Oscillator
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "cdo", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
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
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "MACD Oscillator" )
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.vohlTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # vohlTA - High/Low Volatility
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "vohl", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "High/Low Volatility"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "High/Low Volatility" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.vorTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # vorTA - Volatility Ratio
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "vor", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "Volatility Ratio"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Volatility Ratio" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.fpkTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # fpkTA - Fast %K Stochastics
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "fpk", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "Fast %K Stochastics"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
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
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "fpd", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "Fast %D Stochastics"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
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
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "spd", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "Slow %D Stochastics"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
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
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "apd", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "Averaged %D Stochastics"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
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
    
    # wprTA - Williams %R Stochastics
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "wpr", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "Williams %R Stochastics"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Williams %R Stochastics" )  
}


# ------------------------------------------------------------------------------


.fMultivar.TechnicalAnalysis.rsiTA =
function()
{   # A function implemented by Diethelm Wuertz
    
    # rsiTA - Relative Strength Index
    helpTopic <<- ""
    myFunction = function(select, lag, doplot, col, merge2x, 
        object2x, report) {
        object <<- .dailyTA(X = x, indicator = "rsi", select = select, ...)
        if (merge2x) {
            object2x = FALSE
            x <<- mergeSeries(x, object@Data)
        }
        if (doplot) {
            what = "Relative Strength Index"
            par(mfrow = c(2, 1), cex = 0.7)
            plot(x[, select], ylab = select, main = select)
            plot(object, ylab = what, main = what, col = col)
            abline(h = 0, col = "grey")
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            select = ..., 
            lag = ...,
            doplot = TRUE,
            col = "red", 
            merge2x = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Relative Strength Index" )  
}


################################################################################
# Benchmark Analysis


.fMultivar.BenchmarkAnalysis.2 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Maximum Draw-Down:
    helpTopic <<- ""
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


.fMultivar.BenchmarkAnalysis.3 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Sharpe Ratio:
    helpTopic <<- ""
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


.fMultivar.BenchmarkAnalysis.4 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Sterling Ratio:
    helpTopic <<- ""
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
    helpTopic <<- ""
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
    helpTopic <<- ""
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
    helpTopic <<- ""
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
    helpTopic <<- ""
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

