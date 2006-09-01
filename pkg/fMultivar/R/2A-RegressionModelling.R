
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
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
# FUNCTION:             REGRESSION MODELLING DESCRIPTION:
#  'fREG'                S4 Class Representation
#  regSim                Returns a regression example data set
#  regFit                Wrapper Function for Regression Models
#  gregFit                Wrapper Function for Generalized Regression Models
#  .lmFit                 Linear Regression Model
#  .rlmFit                Robust Linear Regression Model
#  .glmFit                Generalized Linear Model
#  .gamFit                Generalized Additive Model
#  .pprFit                Projection Pursuit Regression Model
#  .marsFit               Multivariate Adaptive Regression Spline Model
#  .polymarsFit           Polytochomous MARS Model
#  .nnetFit               Feedforward Neural Network Model
# S3-METHODS:           DESCRIPTION:
#  print.fREG            Prints results from a regression model fit     
#  plot.fREG             Plots fit and diagnostics for a regression model
#  .plot.lm
#  .plot.rlm
#  .plot.glm
#  .plot.gam
#  .plot.ppr
#  .plot.mars
#  .plot.polymars
#  .plot.nnet
#  summary               Summarizes fit and diagnostics for a regression model
# S3-METHODS:           DESCRIPTION:
#  predict.fREG          Predicts values from a fitted regression model
#  coefficients.fREG     Returns coefficients from a fitted regression model
#  fitted.fREG           Returns fitted values from a fitted regression model
#  residulals.fREG       Returns residuals from a fitted regression model
#  vcov.fREG             Returns variance-covariance matrix from a fitted model
################################################################################


################################################################################
# BUILTIN:              FROM MDA - MARS DESCRIPTION:
#  .mars                 Internal Function
#  .mars.formula         Internal Function
#  .mars.default         Internal Function
#  .predict.mars         Internal Function
#  .model.matrix.mars    Internal Function
################################################################################


################################################################################
# INTERFACE:            FROM POLSPLINE - POLYMARS DESCRIPTION:
#  .polymars
#  .polymars.formula     Formula Generator for Polytochomous MARS Model
#  .polymars.default     Internal Function
#  .predict.polymars     Internal Function
################################################################################


################################################################################
# FINMETRICS-LIKE:      OLS DESCRIPTION:
#  OLS                   Fit an OLS regression model - SPlus like Call
#  print.OLS             S3 Print method for an OLS regression model
#  plot.OLS              S3 Plot method for an OLS regression model
#  summary.OLS           S3 Summary method for an OLS regression model
################################################################################


################################################################################
# MODEL:        PACKAGE     print   plot   summary   print     predict
#                                    persp           summary
#   lm          stats       x       x      x         x         x
#   rlm         MASS
#   glm         stats       x       -      x         x         x
#   gam         mgcv        x       x      x         x         x
#   ppr         modreg      x       x      x         x         x
#   mars*       mda         -       -      -         -         x 
#   polymars*   polspline   -       xx     x         -         x
#   nnet        nnet        x       -      x         x         x
#
#   *BUILTIN
#    mda        Mixture and flexible discriminant analysis
#    polspline  Polynomial spline routines
#   *IMPORTANT NOTE:
#    Both packages r-cran-mda and r-cran-polspline are not available on the
#    Debian Server, therefore we made them accessible as Builtin functions
################################################################################


################################################################################
# BUILTIN - PACKAGE DESCRIPTION:
#  Package: mda
#  Version: 0.2-23
#  Author: S original by Trevor Hastie & Robert Tibshirani.  
#    R port by Friedrich Leisch, Kurt Hornik and Brian D. Ripley.
#  Maintainer: Kurt Hornik <Kurt.Hornik@R-project.org>
#  Description: Mixture and flexible discriminant analysis, multivariate
#    additive regression splines (MARS), BRUTO, ...
#  Title: Mixture and flexible discriminant analysis
#  Depends: class, R (>= 1.5.0)
#  License: GPL version 2
#  Packaged: Sat Jan 31 13:31:19 2004; hornik
################################################################################


################################################################################
# BUILTIN - PACKAGE DESCRIPTION:
#  Package: polspline
#  Version: 1.0.5
#  Date: 2004-04-22
#  Title: Polynomial spline routines
#  Author: Charles Kooperberg <clk@fhcrc.org>
#  Maintainer: Charles Kooperberg <clk@fhcrc.org>
#  Depends: R
#  Description: Routines for the polynomial spline fitting routines
#    hazard regression, hazard estimation with flexible tails, logspline,
#    lspec, polyclass, and polymars, by C. Kooperberg and co-authors
#  License: GPL version 2 or newer
#  Packaged: Thu Apr 22 13:59:50 2004; hornik
################################################################################


# ------------------------------------------------------------------------------
# Class Representation


setClass("fREG", 
    representation(
        call = "call",
        formula = "formula",
        family = "character",  
        method = "character",
        data = "timeSeries",
        fit = "list",
        residuals = "timeSeries",
        fitted.values = "timeSeries",
        title = "character",
        description = "character"
    )  
)
    

# ------------------------------------------------------------------------------


regSim = 
function(model = c("LM3", "LOGIT3", "GAM3"), n = 100)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns one from three artificial regression data sets
    
    # Details:
    #   LM3         3 Variable Linear Regression Model Example
    #   LOGIT2      2 Variable GLM Logit Model Example
    #   GAM         2 Variable Generalized Additive Model Example
    
    # FUNCTION:
    
    # Select Regression Models:
    model = match.arg(model)
    
    # LM3:
    if (model == "LM3") {
        x1 = rnorm(n)
        x2 = rnorm(n)
        x3 = rnorm(n)
        y = 0.75 * x1 + 0.25 * x2 - 0.5 * x3
        eps = 0.1 * rnorm(n)
        y = y + eps
        X = data.frame(Y = y, X1 = x1, X2 = x2, X3 = x3)
    }
    
    # LOGIT2:
    if (model == "LOGIT3") {
        # GLM / BINOMIAL/LOGIT - Example Data:
        x1 = rnorm(n)
        x2 = rnorm(n)
        x3 = rnorm(n)
        eps = 0.1 * rnorm(n)
        y = 0.75 * x1 + 0.25 * x2 - 0.5 * x3 + eps
        p = 1 / ( 1 + exp(-y) )
        X = data.frame(Y = p, X1 = x1, X2 = x2, X3 = x3)
    }
    
    # GAM2:
    if (model == "GAM3") {  
        # GAM - Example Data:
        set.seed(4711)
        x1 = runif(n)
        x2 = runif(n)
        x3 = runif(n)
        y1 = scale(sin(2 * pi * x1))
        y2 = scale(exp(x2))
        y3 = scale(x3)
        y = scale(y1 + y2 + y3)
        eps = 0.1 * rnorm(n, sd = sd(y))
        y = y + eps
        X = data.frame(Y = y, X1 = x1, X2 = x2, X3 = x3)
    }
    
    # Return Value:
    X
}
    
    

# ------------------------------------------------------------------------------


regFit = 
function (formula, data,
use = c("lm", "rlm", "am", "ppr", "mars", "polymars", "nnet"), 
title = NULL, description = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Common function call for several selected regression models.
    
    # Details:
    #   This is a wrapper function for the following regrssion models:
    #   LM          Linear Regression Modelling
    #   RLM         Robust Linear Regression Modelling
    #   AM          Additive Modelling
    #   PPR         Projection Pursuit Regression
    #   MARS        Multivariate Adaptive Regression Splines
    #   POLYMARS    Polytochomous MARS Modeling
    #   NNET        Feedforward Neural Net
    
    # Notes:
    #   Available Methods are
    #   "print", "plot", "summary", and "predict" method
    #   coefficients, "residuals" "fitted", "vcov" method
    
    # Example:
    #   regFit(Y ~ X1 + X2, regSim())
    
    # FUNCTION:
    
    # Get Method:
    if (!(class(data) == "timeSeries")) data = as.timeSeries(data)
    fun = use = match.arg(use)
    if (use == "am") fun = "gam"
    if (use == "mars") fun = ".mars"
    if (use == "polymars") fun = ".polymars"

    # Title:
    if (is.null(title)) {
        if (use == "lm") title = "Linear Regression Modelling"
        if (use == "rlm") title = "Robust Linear Regression Modelling"
        if (use == "am") title = "Additive Modelling"
        if (use == "ppr") title = "Projection Pursuit Regression"
        if (use == "mars") title = "Multivariate Adaptive Regression Splines"
        if (use == "polymars") title = "Polytochomous MARS Modeling"
        if (use == "nnet") title = "Feedforward Neural Network Modelling" 
    } 
    if (is.null(description)) {
        description = .description()
    }
    
    # Evaluate:
    cmd = match.call()
    if (!is.null(cmd$use)) cmd = cmd[-match("use", names(cmd), 0)]    
    cmd[[1]] <- as.name(fun)
    if (use == "ppr"  & !match("nterm",  names(cmd), 0) ) cmd$nterm = 2
    if (use == "nnet" & !match("trace",  names(cmd), 0) ) cmd$trace = FALSE
    if (use == "nnet" & !match("size",   names(cmd), 0) ) cmd$size = 2
    if (use == "nnet" & !match("linout", names(cmd), 0) ) cmd$linout = TRUE
    fit <- eval(cmd, parent.frame()) 
      
    # Add to Fit:
    if (is.null(fit$xlevels)) fit$xlevels = list()
    fit$residuals = as.vector(fit$residuals)    
    fit$fitted.values = as.vector(fit$fitted.values)
    fit$parameters = fit$coef
    if (use == "am") fit$fake.formula = interpret.gam(formula)$fake.formula
    noFitModels = c("ppr", "mars", "nnet")
    FitModelTest = as.logical(match(use, noFitModels, 0))
    if (FitModelTest) {
        mf <- match.call(expand.dots = FALSE)
        Names = c("formula", "data", "subset", "weights", "na.action", "offset")
        mf <- mf[c(1, match(Names, names(mf), 0))]
        mf$drop.unused.levels <- TRUE
        mf[[1]] <- as.name("model.frame")
        fit$model <- eval(mf, parent.frame())
    }
    class(fit) = c("list", class(fit))
    if (!inherits(fit, "lm")) class(fit) = c(class(fit), "lm")

    AM
    # Add Units to timeSeries:
    resUnits = paste(as.character(formula)[2], "RES", sep = ".")
    fittedUnits = paste(as.character(formula)[2], "FITTED", sep = ".")
    residualsTS = 
        timeSeries(fit$residuals, rownames(data), units = resUnits)
    fittedTS = 
        timeSeries(fit$fitted.values, rownames(data), units = fittedUnits)
        
    # Return Value:
    new("fREG",     
        call = as.call(match.call()),
        formula = as.formula(formula), 
        family = as.character(gaussian()),
        method = use,
        data = data,
        fit = fit,
        residuals = residualsTS,
        fitted.values = fittedTS,
        title = as.character(title), 
        description = as.character(description) 
    )
}


# ------------------------------------------------------------------------------


gregFit = 
function (formula, family, data, use = c("glm", "gam"), 
title = NULL, description = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Common function call for several selected regression models.
    
    # Details:
    #   This is a wrapper function for the following regrssion models:
    #   GLM         Generalized Linear Modelling
    #   GAM         Generalized Additive Modelling
    
    # Notes:
    #   Available Methods are
    #   "print", "plot", "summary", and "predict" method
    #   coefficients, "residuals" "fitted", "vcov" method
    
    # FUNCTION:
    
    # Get Method:
    if (class(data) == "data.frame") data = as.timeSeries(data)
    fun = method = match.arg(method)

    # Title:
    if (is.null(title)) {
        if (method == "glm") title = "Generalized Linear Modelling"
        if (method == "gam") title = "Generalized Additive Modelling"
    }  
    
    # Evaluate:
    cmd = match.call()
    if (!is.null(cmd$method)) cmd = cmd[-match("method", names(cmd), 0)]    
    cmd[[1]] <- as.name(fun)
    fit <- eval(cmd, parent.frame()) 
        
    # Add to Fit:
    fit$residuals = as.vector(fit$residuals)    
    fit$fitted.values = as.vector(fit$fitted.values)
    fit$parameters = fit$coef
    class(fit) = c("list", class(fit))
    
    # Return Value:
    new("fREG",     
        call = as.call(match.call()),
        formula = as.formula(formula), 
        family = as.character(gaussian()),
        method = as.character(method),
        data = timeSeries(data, rownames(data)),
        fit = fit,
        residuals = timeSeries(fit$residuals, rownames(data)),
        fitted.values = timeSeries(fit$fitted.values, rownames(data)),
        title = as.character(title), 
        description = as.character(description) 
    )
}


# ------------------------------------------------------------------------------


print.fREG = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print method for Regression Modelling, an object of class "fREG"
    
    # FUNCTION:
    
    # Settings:
    object = x
    
    # Title:
    cat("\nTitle:\n ")
    cat(as.character(object@title), "\n")
    
    # Call:
    # cat("\nCall:\n")
    # cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), 
    #    "\n", sep = "") 
        
    # Formula:
    cat("\nFormula:\n ")
    # cat(as.character(object@formula), "\n")
    print(object@formula)
    
    # Family:
    if (object@family[1] != "" && object@family[2] != "") {     
        cat("\nFamily:\n ")
        cat(as.character(object@family[1:2]), "\n") }
    
    # Digits:
    digits = max(4, getOption("digits") - 4)
        
    # Model Parameters:
    cat("\nModel Parameters:\n")        
        
        # Regression Model LM / RLM:
        if (object@method == "lm" | object@method == "rlm") {
            print.default(format(object@fit$coef, digits = digits), 
                print.gap = 2, quote = FALSE) 
        }
        
        # Regression Model GLM:
        if (object@method == "glm") {
            if (length(object@fit$coef)) {
                if (is.character(co = object@fit$contrasts)) 
                cat("  [contrasts: ", apply(cbind(names(co), co), 
                    1, paste, collapse = "="), "]")
                # cat(":\n")
                print.default(format(object@fit$coefficients,
                    digits = digits), print.gap = 2, quote = FALSE)
            } else { 
                cat("No coefficients\n\n") 
            } 
        }   
        
        # Regression Model GAM:
        if (object@method == "gam" | object@method == "am") {
            print.default(format(object@fit$coef, digits = digits), 
                print.gap = 2, quote = FALSE) 
                
        }       
        
        # Regression Model PPR:
        if (object@method == "ppr") {
            cat("-- Projection Direction Vectors --\n")
            print(object@fit$alpha)
            cat("-- Coefficients of Ridge Terms --\n")
            print(object@fit$beta) 
        }    
        
        # Regression Model MARS:
        if (object@method == "mars") {      
            Parameters = round(object@fit$parameters, digits = digits)      
            print(data.frame(Parameters)) 
        }             
        
        # Regression Model POLYMARS:
        if (object@method == "polymars") {
            print(object@fit$Model) 
        }  
        
        # Regression Model NNET:
        if (object@method == "nnet") {
            cat("   a ",object@fit$n[1], "-", object@fit$n[2], "-", 
                object@fit$n[3], " network", " with ", 
                length(object@fit$wts), " weights\n", sep="")
            cat("   options were -")
            tconn = diff(object@fit$nconn)
            if (tconn[length(tconn)] > object@fit$n[2]+1) 
                cat(" skip-layer connections ")
            if (object@fit$nunits > object@fit$nsunits && 
                !object@fit$softmax) 
                cat(" linear output units ")
            if (object@fit$entropy) 
                cat(" entropy fitting ")
            if (object@fit$softmax) 
                cat(" softmax modelling ")
            if (object@fit$decay[1] > 0) 
                cat(" decay=", object@fit$decay[1], sep="")
            cat("\n")
            Weights = object@fit$wts
            print(Weights) 
        } 
        
    # Residual Variance:
    # cat("\nResidual Variance:\n", var(object@fit$residuals))
    cat("\n")
        
    # Return Value:
    invisible()
}            
    

# ------------------------------------------------------------------------------


plot.fREG = 
function(x, ...)
{   # A function imlemented by Diethelm Wuertz

    # Description:
    #   Plot method for Regression Modelling, an object of class "fREG"
    
    # FUNCTION:
    
    # Settings:
    .plot(x@fit)
    
    # Return Value:
    invisible()
}    


# ------------------------------------------------------------------------------


.plot.lm =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tailored plot function for an object of class 'lm', 'rlm', 'glm'
    
    # Changes:
    #
    
    # FUNCTION:
    
    # 1. Responses + Fitted Values Plot:
    # 2. Residuals Plot:
    # 3. Quantile Plot:
    # 4. Fitted Values vs. Residuals Plot:
    # 5. ACF Plot
    # 6. PACF Plot
    # 7. Positive Mean Excess Plot"
    # 8. Negative Mean Excess Plot"
    .plot1 <<- function(x, ...) .responsesPlot(residuals(x)+fitted(x),fitted(x))
    .plot2 <<- function(x, ...) .residualsPlot(residuals(x))    
    .plot3 <<- function(x, ...) .qqbayesPlot(residuals(x))
    .plot4 <<- function(x, ...) .firePlot(fitted(x), residuals(x)) 
    .plot5 <<- function(x, ...) .acfPlot(residuals(x))
    .plot6 <<- function(x, ...) .pacfPlot(residuals(x))
    .plot7 <<- function(x, ...) .mrlPlot(residuals(x))
    .plot8 <<- function(x, ...) .mrlPlot(-residuals(x))
    
    # 1. lm: Residuals vs Fitted:
    # 2. lm: Normal Q-Q:
    # 3. lm: Scale-Location:
    # 4. lm: Cook's distance:
    # 5. lm: Residuals vs Leverage:
    # 6. lm: Cook's distance vs Leverage:
    .plot9 <<- function(x, ...) plot(x, 1, pch = 19, col = "steelblue", ...)  
    .plot10<<- function(x, ...) plot(x, 2, pch = 19, col = "steelblue", ...)
    .plot11<<- function(x, ...) plot(x, 3, pch = 19, col = "steelblue", ...)
    .plot12<<- function(x, ...) plot(x, 4, pch = 19, col = "steelblue", ...)
    .plot13<<- function(x, ...) plot(x, 5, pch = 19, col = "steelblue", ...)
    .plot14<<- function(x, ...) plot(x, 6, pch = 19, col = "steelblue", ...)
    
    # Plot:
    .interactiveRegressionPlot(
        x,
        choices = c(
            "Responses + Fitted Values",
            "Residuals",
            "Normal Q-Q",
            "Residuals vs Fitted",
            "ACF of Residuals",
            "PACF of Residuals",
            "Positive Mean Excess Plot",
            "Negative Mean Excess Plot",
            "lm: Residuals vs Fitted", 
            "lm: Normal Q-Q", 
            "lm: Scale-Location", 
            "lm: Cook's distance", 
            "lm: Residuals vs Leverage", 
            "lm: Cook's distance vs Leverage"),
        plotFUN = paste(".plot", 1:14, sep = ""),
        which = which) 
            
    # Return Value:
    invisible(x)
} 


# ------------------------------------------------------------------------------


.plot.common =
function(x, which = "ask", ...)
{
    # 1. Responses + Fitted Values Plot:
    # 2. Residuals Plot:
    # 3. Quantile Plot:
    # 4. Fitted Values vs. Residuals Plot:
    .plot1 <<- function(x, ...) .responsesPlot(residuals(x)+fitted(x),fitted(x))
    .plot2 <<- function(x, ...) .residualsPlot(residuals(x))    
    .plot3 <<- function(x, ...) .qqbayesPlot(residuals(x))
    .plot4 <<- function(x, ...) .firePlot(fitted(x), residuals(x)) 
    .plot5 <<- function(x, ...) .acfPlot(residuals(x))
    .plot6 <<- function(x, ...) .pacfPlot(residuals(x))
    .plot7 <<- function(x, ...) .mrlPlot(residuals(x))
    .plot8 <<- function(x, ...) .mrlPlot(-residuals(x))
    
    # Plot:
    .interactiveRegressionPlot(
        x,
        choices = c(
            "Responses + Fitted Values",
            "Residuals",
            "Normal Q-Q",
            "Residuals vs Fitted",
            "ACF of Residuals",
            "PACF of Residuals",
            "Positive Mean Excess Plot",
            "Negative Mean Excess Plot"),
        plotFUN = paste(".plot", 1:8, sep = ""),
        which = which) 
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


.plot.gam = .plot.common
.plot.ppr = .plot.common
.plot.mars = .plot.common
.plot.polymars = .plot.common
.plot.nnet = .plot.common


# ******************************************************************************


summary.fREG =       
function(object, ...)
{   # A function imlemented by Diethelm Wuertz

    # Description:
    #   Summary method for Regression Modelling, an object of class "fREG"
    
    # FUNCTION:
    
    # Digits:
    digits = max(4, getOption("digits") - 4)
    
    # Print all from print Method:
    print(object)
    
    # Add Residual Variance:
    cat("Residual Variance:\n", var(object@fit$residuals))
    cat("\n\n")

    # Internal Function: fResiduals
    fResiduals = 
    function(x, digits) 
    {
        cat("Non-Weighted Residuals:\n")
        names = c("Min", "1Q", "Median", "3Q", "Max")
        rq = structure(quantile(x), names = names)
        print(rq, digits = digits) 
        names = c("Variance", "StDev", "Skewness", "Kurtosis")
        skewness = sum((x - mean(x))^3/sqrt(var(x))^3)/length(x)
        kurtosis = sum((x - mean(x))^4/var(x)^2)/length(x) - 3
        rq = structure(c(var(x), sqrt(var(x)), skewness, kurtosis), 
            names = names)
        print(rq, digits = digits) 
        print("done")
        cat("\n") 
        invisible() 
    }
        
    # Internal Function: print.summary.LM
    print.summary.LM = 
    function (x, ...) 
    {
        digits = max(4, getOption("digits") - 4)
        symbolic.cor = x$symbolic.cor
        signif.stars = getOption("show.signif.stars")
        # cat("\nCall:\n")
        # cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        #   "\n\n", sep = "")
        resid = x$residuals
        df = x$df
        rdf = df[2]
        cat(if (!is.null(x$w) && diff(range(x$w))) 
            "Weighted ", "Residuals:\n", sep = "")
        if (rdf > 5) {
            nam = c("Min", "1Q", "Median", "3Q", "Max")
            rq = if (length(dim(resid)) == 2) 
                structure(apply(t(resid), 1, quantile), 
                    dimnames = list(nam, dimnames(resid)[[2]]))
            else structure(quantile(resid), names = nam)
            print(rq, digits = digits, ...) 
        } else if (rdf > 0) {
            print(resid, digits = digits, ...)
        } else {
            cat("ALL", df[1], "residuals are 0: no residual ",
                "degrees of freedom!\n") 
        }
        if (length(x$aliased) == 0) {
            cat("\nNo Coefficients\n") 
        } else {
            if (nsingular<-df[3] - df[1]) {
                cat("\nCoefficients: (", nsingular, " not defined ",
                    "because of singularities)\n", sep = "")
            } else {
                cat("\nCoefficients:\n")
            }
            coefs = x$coefficients
            if (!is.null(aliased = x$aliased) && any(x$aliased)) {
                cn = names(aliased)
                coefs = matrix(NA, length(aliased), 4, dimnames = 
                    list(cn, colnames(coefs)))
                coefs[!aliased, ] = x$coefficients 
            }
            printCoefmat(coefs, digits = digits, signif.stars = 
                signif.stars, na.print = "NA", ...) 
        }
        cat("\nResidual standard error:", format(signif(x$sigma, 
            digits)), "on", rdf, "degrees of freedom\n")
        if (!is.null(x$fstatistic)) {
            cat("Multiple R-Squared:", formatC(x$r.squared, 
                digits = digits))
            cat(",  Adjusted R-squared:", formatC(x$adj.r.squared, 
                digits = digits), "\nF-statistic:", 
                formatC(x$fstatistic[1], digits = digits), "on", 
                x$fstatistic[2], "and", x$fstatistic[3], 
                "DF,  p-value:", format.pval(pf(x$fstatistic[1], 
                x$fstatistic[2], x$fstatistic[3], lower.tail = FALSE), 
                digits = digits), "\n") }
        correl = x$correlation
        if (!is.null(correl)) {
            p = NCOL(correl)
            if (p > 1) {
                cat("\nCorrelation of Coefficients:\n")
                if (is.logical(symbolic.cor) && symbolic.cor) {
                    print(symnum(correl, abbr.col = NULL)) }
                else {
                    correl = format(round(correl, 2), nsmall = 2, 
                      digits = digits)
                    correl[!lower.tri(correl)] = ""
                    print(correl[-1, -p, drop = FALSE], quote = FALSE) }} }
        cat("\n")
        invisible() 
    }
        
    # Internal Function: print.summary.GLM
    print.summary.GLM = 
    function (x, ...) 
    {
        digits = max(4, getOption("digits") - 4)
        symbolic.cor = x$symbolic.cor
        signif.stars = getOption("show.signif.stars")
        #cat("\nCall:\n")
        #cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        #   "\n\n", sep = "")
        cat("Deviance Residuals: \n")
        if (x$df.residual > 5) {
            x$deviance.resid = quantile(x$deviance.resid, na.rm = TRUE)
            names(x$deviance.resid) = c("Min", "1Q", "Median", "3Q", 
                "Max") }
        print.default(x$deviance.resid, digits = digits, na = "", 
            print.gap = 2)
        if (length(x$aliased) == 0) {
            cat("\nNo Coefficients\n") 
        } else {
            if (!is.null(df = x$df) && (nsingular = df[3] - df[1])) 
                cat("\nCoefficients: (", nsingular, " not defined ",
                "because of singularities)\n", sep = "")
            else cat("\nCoefficients:\n")
            coefs = x$coefficients
            if (!is.null(aliased = x$aliased) && any(aliased)) {
                cn = names(aliased)
                coefs = matrix(NA, length(aliased), 4, dimnames = 
                    list(cn, colnames(coefs)))
                coefs[!aliased, ] = x$coefficients }
            printCoefmat(coefs, digits = digits, signif.stars = 
                signif.stars, na.print = "NA", ...) 
            }
        cat("\n(Dispersion parameter for ", x$family$family, 
            " family taken to be ", format(x$dispersion), ")\n\n", 
            apply(cbind(paste(format.char(c("Null", "Residual"), 
            width = 8, flag = ""), "deviance:"), 
            format(unlist(x[c("null.deviance", "deviance")]), 
            digits = max(5, digits + 1)), " on", 
            format(unlist(x[c("df.null", "df.residual")])), 
            " degrees of freedom\n"), 1, paste, collapse = " "), 
            "AIC: ", format(x$aic, digits = max(4, digits + 1)), 
            "\n\n", "Number of Fisher Scoring iterations: ", 
            x$iter, "\n", sep = "")
        correl = x$correlation
        if (!is.null(correl)) {
            p = NCOL(correl)
            if (p > 1) {
                cat("\nCorrelation of Coefficients:\n")
                if (is.logical(symbolic.cor) && symbolic.cor) {
                    print(symnum(correl, abbr.col = NULL)) }
                else {
                    correl = format(round(correl, 2), nsmall = 2, 
                      digits = digits)
                    correl[!lower.tri(correl)] = ""
                    print(correl[-1, -p, drop = FALSE], quote = FALSE) }}}
        cat("\n")
        invisible() 
    }

    # Internal Function: print.summary.GAM
    print.summary.GAM = 
    function(x, ...) 
    { 
        if (length(x$p.coeff) > 0) {
            cat("Parametric coefficients:\n")
            width = max(nchar(names(x$p.coeff)))
            cat(rep(" ",width), "   Estimate  std. err.    t ratio",
                "    Pr(>|t|)\n", sep = "")
            for (i in 1:length(x$p.coeff))
                cat(formatC(names(x$p.coeff)[i], width = width), " ",
                    formatC(x$p.coeff[i], width=10, digits=5), " ",
                    formatC(x$se[i], width = 10, digits = 4), " ",
                    formatC(x$p.t[i], width = 10, digits = 4), "    ",
                    format.pval(x$p.pv[i]), "\n", sep="") }
        cat("\n")
        if (x$m > 0) { 
            cat("Approximate significance of smooth terms:\n")
            width = max(nchar(names(x$chi.sq)))
            cat(rep(" ",width), "        edf       chi.sq     ",
                "p-value\n", sep = "")
            for (i in 1:x$m)
                cat(formatC(names(x$chi.sq)[i], width = width), " ", 
                    formatC(x$edf[i], width = 10, digits = 4), "   ",
                    formatC(x$chi.sq[i], width = 10, digits = 5), "     ",
                    format.pval(x$s.pv[i]), "\n", sep = "") }
        cat("\nR-sq.(adj) = ", formatC(x$r.sq, digits = 3, width = 5),
            "   Deviance explained = ", formatC(x$dev.expl*100, 
            digits = 3, width = 4), "%", sep = "")
        if (is.null(x$ubre)) {
            cat("\nGCV score = ", formatC(x$gcv, digits = 5), " ", sep = "")
        } else {
            cat("\nUBRE score = ", formatC(x$ubre, digits = 5), sep = "")
        }
        cat("  Scale est. = ", formatC(x$scale, digits = 5, 
            width = 8, flag = "-"), "  n = ", x$n, "\n", sep = "") 
        invisible() 
    }           

    # Fit:
    fit = object@fit 
            
    # Regression Model: LM
    if (object@method == "lm") {
        class(fit) = "lm"
        ans = summary.lm(object = fit, ...)
        print.summary.LM(x = ans, ...) 
    }    
    
    # Regression Model: GLM
    if (object@method == "glm") {
        class(fit) = c("glm", "lm")
        ans = summary.glm(object = fit, ...)
        print.summary.GLM(x = ans, ...) 
    }   
    
    # Regression Model: GAM
    if (object@method == "gam") {
        class(fit) = "gam"
        ans = summary.gam(object = fit, ...)
        print.summary.GAM(x = ans, ...) 
    }   
    
    # Regression Model: GAM
    if (object@method == "gam") {
        class(fit) = "gam"
        ans = summary.gam(object = fit, ...)
        print.summary.GAM(x = ans, ...) 
    }
        
    # Regression Model: PPR
    if (object@method == "ppr") {
        # This is what print.ppr produces.
        mu = fit$mu; ml = fit$ml
        cat("Goodness of fit:\n")
        gof = fit$gofn; names(gof) = paste(1:ml, "terms")
        print(format(gof[mu:ml], ...), quote = FALSE)
        # This is what summary.ppr produces.
        if (any(fit$edf > 0)) {
            cat("\nEquivalent df for ridge terms:\n")
            edf = fit$edf
            names(edf) = paste("term", 1:fit$mu)
            print(round(edf, 2), ...)} 
    }        
                
    # Regression Model: MARS
    if (object@method == "mars") {
        # Use the print Method 
    } 
            
    # Regression Model: POLYMARS
    if (object@method == "polymars") {
        # This is what summary.polymars produces.
        # There is no print.summary.polymars.
        cat("Model Fitting:\n")
        print(fit$fitting)
        if(fit$responses != 1)
            cat("\nResponses:", fit$responses, "\n")
        if(!is.null(fit$Rsquared))
            cat("\nRsquared:",round(fit$Rsquared, 4),"\n") 
        cat("\n") 
    }
        
    # Regression Model: NNET
    if (object@method == "nnet") {
        # Use the print Method
    }       

    # Return Value:
    invisible() 
}


# ------------------------------------------------------------------------------


.predict.lm = predict.lm
.predict.gam = predict.gam
.predict.glm = predict.glm 
.predict.ppr = function(object, ...) { predict(object, ...) }
.predict.nnet = function(object, ...) { predict(object, ...) }


# ------------------------------------------------------------------------------


predict.fREG =
function(object, newdata, se.fit = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Predict method for Regression Modelling, an object of class "fREG"
    
    # FUNCTION:
    
    # Fit:
    fit = object@fit
      
    # Data:
    if (missing(newdata)) newdata = object@data
    newdata = as.data.frame(newdata)
     
    # Predict:
    ans = .predict(object = fit, newdata = newdata, se.fit = se.fit, 
        type = type, ...) 
    
    # Make the output from 'predict' unique:
    if (se.fit) {
        if (!is.list(ans)) {
            if (is.matrix(ans)) ans = as.vector(ans)
            names(ans) = rownames(newdata) 
            ans = list(fit = ans, se.fit = NA*ans) 
        } else {
            ans = ans[1:2]
        }
    } else {
        if (is.matrix(ans)) ans = as.vector(ans)
        names(ans) = rownames(newdata) 
    }
            
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

        
coef.fREG = 
function(object, ...)
{   # A function imlemented by Diethelm Wuertz

    # Description:
    #   Coefficients method for Regression Modelling
    
    # FUNCTION:
    
    # Fitted Values:
    ans = coefficients(object@fit) 
            
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

        
fitted.fREG = 
function(object, ...)
{   # A function imlemented by Diethelm Wuertz

    # Description:
    #   Fitted values method for Regression Modelling
    
    # FUNCTION:
    
    # Fitted Values:
    ans = object@fitted.values
            
    # Return Value:
    ans
}
        

# ------------------------------------------------------------------------------

                   
residuals.fREG = 
function(object, ...)
{   # A function imlemented by Diethelm Wuertz

    # Description:
    #   Residuals method for Regression Modelling
    
    # FUNCTION:
    
    # Residuals:
    ans = object@residuals
            
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

        
vcov.fREG = 
function(object, ...)
{   # A function imlemented by Diethelm Wuertz

    # Description:
    #   Variance-Covariance Matrix method for Regression Modelling
    
    # FUNCTION:
    
    # Fitted Values:
    ans = vcov(object@fit) 
            
    # Return Value:
    ans
}


################################################################################
# FUNCTION:
#  .mars
#  .mars.default
#  .predict.mars
#  .showcuts.mars


.mars = 
function(formula, data, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Settings:
    m = match.call(expand = FALSE)
    m$contrasts = m$... = NULL
    m[[1]] = as.name("model.frame")
    m = eval(m, parent.frame())
    na.act = attr(m, "na.action")
    Terms = attr(m, "terms")
    attr(Terms, "intercept") = 0
    X = model.matrix(Terms, m, contrasts)
    Y = model.extract(m, response)
    w = model.extract(m, weights)
    if (length(w) ==  0) w = rep(1, nrow(X))
    
    # Fit:
    fit = .mars.default(X, Y, w, ...)
    fit$terms = Terms   
    
    # Result:
    fit$family = c("", "")
    fit$parameters = as.vector(fit$coefficients)
    fit$model = data
    
    # Return Value:
    fit 
}


# ------------------------------------------------------------------------------


.mars.default = 
function (x, y, w = rep(1, nrow(x)), wp, degree = 1, nk = max(21, 
2 * ncol(x) + 1), penalty = 2, thresh = 0.001, prune = TRUE, 
trace.mars = FALSE, forward.step = TRUE, prevfit = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   A (modified) copy from contributed R-package 'mda' 
    
    # FUNCTION:
        
    # MARS:
    this.call = match.call()
    if ((nk%%2) != 1) nk = nk - 1
    x = as.matrix(x)
    np = dim(x)
    n = np[1]
    p = np[2]
    y = as.matrix(y)
    nclass = ncol(y)
    if (is.null(np)) {
        np = c(length(x), 1)
        x = as.matrix(x) 
    }
    if (forward.step) {
        interms = 1
        lenb = nk
        bx = matrix(rep(0, nrow(x) * nk), nrow = n)
        res = matrix(rep(0, nrow(x) * ncol(y)), nrow = n)
        fullin = rep(0, nk)
        cuts = NULL
        factor = NULL 
    } else {
        bx = model.matrix.mars(prevfit, x, full = TRUE)
        interms = ncol(bx)
        lenb = prevfit$lenb
        o = prevfit$all.terms
        fullin = rep(0, ncol(bx))
        fullin[o] = 1
        res = prevfit$res
        factor = prevfit$factor
        cuts = prevfit$cuts
        if (missing(penalty)) 
            penalty = prevfit$penalty
        degree = prevfit$degree
        nk = lenb
        thresh = prevfit$thresh 
    }
    if (missing(penalty) & (degree > 1)) {
        penalty = 3
    }
    if (!missing(wp)) {
        if (any(wp <= 0)) 
            stop("wp should all be positive")
        wp = sqrt(wp/sum(wp))
        y = y * outer(rep(1, n), wp) 
    } else {
        wp = NULL
    }
    tagx = x
    storage.mode(tagx) = "integer"
    for (j in 1:p) {
        tagx[, j] = order(x[, j]) 
    }
    bestin = rep(0, nk)
    flag = matrix(rep(0, nk * p), nrow = nk, ncol = p)
    if (is.null(cuts)) {
        cuts = matrix(rep(0, nk * p), nrow = nk, ncol = p)
    }
    if (is.null(factor)) {
        dir = matrix(rep(0, nk * p), nrow = nk, ncol = p) 
    } else {
        dir = factor 
    }
    alpha = rep(0, nclass)
    beta = matrix(rep(0, nk * nclass), nrow = nk)
    bestgcv = 0
    storage.mode(y) = "double"
    storage.mode(x) = "double"
    storage.mode(bx) = "double"
    storage.mode(flag) = "integer"
    storage.mode(cuts) = "double"
    storage.mode(dir) = "double"
    storage.mode(res) = "double"
    storage.mode(beta) = "double"
    lenscrat = 1 + n + 2 * n * nk + 4 * nk * nk + 3 * nk + 3 * 
        nk * nclass + 3 * nclass + 28 * n + 51
    junk = .Fortran("marss", as.integer(n), as.integer(n), as.integer(p), 
        as.integer(nclass), as.matrix(y), as.matrix(x), as.double(w), 
        as.matrix(tagx), as.integer(degree), as.integer(nk), 
        as.double(penalty), as.double(thresh), as.logical(forward.step), 
        as.integer(interms), as.logical(prune), bx = as.matrix(bx), 
        fullin = as.integer(fullin), lenb = as.integer(lenb), 
        bestgcv = as.double(bestgcv), bestin = as.integer(bestin), 
        flag = as.matrix(flag), cuts = as.matrix(cuts), dir = as.matrix(dir), 
        res = as.matrix(res), alpha = as.double(alpha), beta = as.matrix(beta), 
        double(lenscrat), integer(4 * nk), trace.mars, PACKAGE = "fMultivar")
    lenb = junk$lenb
    all.terms = seq(lenb)[junk$fullin[1:lenb] == 1]
    selected.terms = seq(lenb)[junk$bestin[1:lenb] == 1]
    coefficients = junk$beta[seq(selected.terms), , drop = FALSE]
    residuals = junk$res
    fitted.values = y - residuals
    if (!is.null(wp)) {
        TT = outer(rep(1, n), wp)
        residuals = residuals/TT
        fitted.values = fitted.values/TT
        coefficients = coefficients/outer(rep(1, length(selected.terms)), wp) 
    }
    dir = junk$dir[seq(lenb), , drop = FALSE]
    dimnames(dir) = list(NULL, dimnames(x)[[2]])
    cutss = junk$cuts[seq(lenb), , drop = FALSE]
    x = junk$bx[, selected.terms, drop = FALSE]
    
    # Return Value:
    structure(list(call = this.call, all.terms = all.terms, 
        selected.terms = selected.terms, 
        penalty = penalty, degree = degree, nk = nk, thresh = thresh, 
        gcv = junk$bestgcv, factor = dir, cuts = cutss, 
        residuals = residuals, 
        fitted.values = fitted.values, lenb = junk$lenb, 
        coefficients = coefficients, 
        #x = x
        ), 
        class = "mars")
}


# ------------------------------------------------------------------------------


.predict.mars = 
function (object, newdata, se.fit = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   A (modified) copy from contributed R-package 'mda' 
    
    # FUNCTION:
    
    # Select Object:
    if (class(object)[1] == "fREG") object = object@fit
    
    # Predict:
    if (missing(newdata)) {
        z <- fitted(object)
        if (is.null(z)) {
            stop("need to supply newdata")
        } else {
            ans = z
        }
    } else {
        tt = terms(object)
        Terms = delete.response(tt)
        x <- model.frame(Terms, newdata)
        x = as.matrix(x)
        dd = dim(x)
        n = dd[1]
        p = dd[2]
        which = object$selected.terms
        nterms = length(which)
        dir = object$factor
        cut = object$cuts
        
        modelMatrix = matrix(1, nrow = n, ncol = nterms)
        which = which[-1]
        for (i in seq(along = which)) {
            j = which[i]
            if (all(dir[j, ] == 0)) { stop("error in factor or which") }
            temp1 = 1
            for (k in 1:p) {
                if (dir[j, k] != 0) {
                    temp2 = dir[j, k] * (x[, k] - cut[j, k])
                    temp1 = temp1 * temp2 * (temp2 > 0) 
                } 
            }
            modelMatrix[, i + 1] = temp1
        }    
        ans = modelMatrix %*% object$coefficients
        ans = as.vector(ans)
        names(ans) = rownames(newdata)
    }
    
    # Add Standard Errors ?
    if (se.fit) {
        se.fitted = NA*ans
        names(se.fitted) = names(ans)
        ans = list(fit = ans, se.fit = se.fitted)
    }
    
    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


.showcuts.mars = 
function(object)
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   A (modified) copy from contributed R-package 'mda' 
    
    # FUNCTION:
    
    # Select Object:
    if (class(object)[1] == "fREG") object = object@fit
    
    # Cuts:
    tmp = object$cuts[object$sel, ]
    dimnames(tmp) = list(NULL, names(trees)[-3])
    
    # Return Value:
    tmp
}


################################################################################
# FUNCTION:
#  .polymars
#  .polymars.default
#  .predict.polymars
# Contributed package "polspline" is required!


.polymars =
function(formula, data, ...)
{   # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Extract Model Data:
    mf = match.call(expand.dots = FALSE)
    m = match(c("formula", "data"), names(mf), 0)
    mf = mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    Data = eval(mf, parent.frame())
    y = Data[, 1]
    x = Data[, -1]
    
    # Fit:
    fit = .polymars.default(responses = y, predictors = x, ...) 
    
    # Add to fit:
    fit$fitted.values = fit$fitted
    fit$terms = terms(formula)
    fit$Model = fit$model
    fit$model = Data
    class(fit) = "polymars"
    
    # Return Value:
    fit
}


# ------------------------------------------------------------------------------


.polymars.default = 
function(responses, predictors, maxsize, gcv = 4, additive = FALSE, 
startmodel, weights, no.interact, knots, knot.space = 3, ts.resp, ts.pred, 
ts.weights, classify, factors, tolerance = 1e-06, verbose = FALSE) 
{   # A function implemented by Diethelm Wuertz
    
    # Arguments:
    #  responses - a vector (or matrix) of responses. (Can be a a vector of 
    #       characters for classification)
    #  predictors - a matrix of predictors with same number of cases as 
    #       response. Columns are predictors.
    
    # Optional Arguments:
    #  maxsize - maximum number of basis function the model can contain 
    #  gcv - parameter for overall best model seletion
    #  additive - boolean, is the model to be additive
    #  startmodel - either a matrix (m*4 or m*5) or a polymars object from 
    #       a previous call to polymars 
    #       an initial model the procedure should start with in model 
    #       selection
    #  weights - a vector of length equal to the number of cases
    #  no.interact - a 2*l matrix of columns numbers of the predictor 
    #       matrix (each row pair cannot have interaction terms)
    #  knots - a vector specifying many knots per predictor are 
    #       wanted (with -1 for categorical variables) 
    #       ncol(predictors)==length(knots), or a matrix with 
    #       ncol(predictors) == ncol(knots) with actual knot 
    #       specified and filled out with NA's.
    #       Can also be a single number - "knots" number of knots 
    #       per predictor                    
    #  knot.space - minimum number of order statistics between knots
    #  ts.resp - testset reponses, same format as responses
    #  ts.pred - testset predictors, same format as predictors
    #  ts.weights - testset weights, same format as weights
    #  classify - whether classification is to be done, set = TRUE if the 
    #       response vector is integer, if 
    #       if character classify is automatically true
    #  factors - a vector of column numbers of the predictor matrix of 
    #       categorical variables
    #  tolerance - a numerical parameter which may need to be made smaller 
    #       if the program crashes store the call to the polymars 
    #       function

    # FUNCTION:
    
    # require(polspline)
    require(polspline)
    
    # Fit:
    .Call <- match.call()
    .Call[[1]] <- as.name("polymars")
    ans = eval(.Call, parent.frame())
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.predict.polymars =
function(object, newdata, se.fit = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Predict:
    if (missing(newdata)) {
        y = as.vector(object$fitted)
    } else {
        object$model = object$Model
        tt = terms(object)
        Terms = delete.response(tt)
        modelFrame = model.frame(Terms, newdata)
        x = model.matrix(Terms, modelFrame)[, -1]
        class(object) = "polymars"
        y = as.vector(predict(object, x = x, ...))
        names(y) = rownames(newdata)
    }
    
    # Add optionally standard errors:
    if (se.fit) y = list(fit = y, se.fit = NA*y)
   
    # Return Value:
    y
}


################################################################################
#  OLS            Fit an OLS regression model
#   print.OLS      S3 Print method for an OLS regression model
#   plot.OLS       S3 Plot method for an OLS regression model
#   summary.OLS    S3 Summary method for an OLS regression model


OLS = 
function(formula, data, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   OLS Estimator
    
    # Notes:
    #   A Finmetrics S-Plus like implementation
    
    # FUNCTION:
    
    # Estimate:
    fit = lm(formula = formula, data = data, ...)
    fit$call = match.call()
    fit$formula = formula
    fit$data = data
    class(fit) = "OLS"
    
    # Return Value:
    fit 
}

# ------------------------------------------------------------------------------


print.OLS = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Print Method
    
    # FUNCTION:
    
    # Print:
    class(x) = "lm"
    print.lm(x, ...) 
    
    # Return Value:
    inbvisible(x)
}
    

# ------------------------------------------------------------------------------


plot.OLS = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Plot Method
    
    # FUNCTION:
    
    # Plot:
    class(x) = "lm"
    plot.lm(x, ...)
    
    # Return Value:
    inbvisible(x)
}


# ------------------------------------------------------------------------------

    
summary.OLS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Summary Method
    
    # FUNCTION:
    
    # Summary:
    class(object) = "lm"
    summary.lm(object, ...) 
    
    # Return Value:
    inbvisible(object)
}


################################################################################


.interactiveRegressionPlot = 
function(x, choices = paste("Plot", 1:19), 
plotFUN = paste("plot.", 1:19, sep = ""), which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "template".
    
    # Arguments:
    #   x - an object to be plotted
    #   choices - the character string for the choice menu
    #   plotFUN - the names of the plot functions
    #   which - plot selection, which graph should be 
    #     displayed. If a character string named "ask" the 
    #     user is interactively asked which to plot, if
    #     a logical vector of length N, those plots which
    #     are set "TRUE" are displayed, if a character string
    #     named "all" all plots are displayed.
    
    # Note:
    #   At maximum 19 plots are supported.

    # FUNCTION:
    
    # Some cecks:
    if (length(choices) != length(plotFUN)) 
        stop("Arguments choices and plotFUN must be of same length.")
    if (length(which) > length(choices)) 
        stop("Arguments which has incorrect length.")
    if (length(which) > length(plotFUN)) 
        stop("Arguments which has incorrect length.")
    if (length(choices) > 19)
        stop("Sorry, only 19 plots at max are supported.")
    
    # Internal "askPlot" Function:                
    multPlot = function (x, choices, ...) 
    {
        # Selective Plot:
        selectivePlot = function (x, choices, FUN, which){
            # Internal Function:
            askPlot = function (x, choices, FUN) {
                # Pick and Plot:
                pick = 1; n.plots = length(choices)
                while (pick > 0) { pick = menu (
                    choices = paste("plot:", choices), 
                    title = "\nMake a plot selection (or 0 to exit):")
                    if (pick > 0) match.fun(FUN[pick])(x) } }                   
            if (as.character(which[1]) == "ask") {
                askPlot(x, choices = choices, FUN = FUN, ...) }
            else { 
                for (i in 1:n.plots) if (which[i]) match.fun(FUN[i])(x) }
            invisible() }  
        # match Functions, up to nine ...
        if (length(plotFUN) < 19) plotFUN = 
            c(plotFUN, rep(plotFUN[1], times = 19 - length(plotFUN)))
        plot.1  = match.fun(plotFUN[1]);  plot.2  = match.fun(plotFUN[2]) 
        plot.3  = match.fun(plotFUN[3]);  plot.4  = match.fun(plotFUN[4]) 
        plot.5  = match.fun(plotFUN[5]);  plot.6  = match.fun(plotFUN[6]) 
        plot.7  = match.fun(plotFUN[7]);  plot.8  = match.fun(plotFUN[8]) 
        plot.9  = match.fun(plotFUN[9]);  plot.10 = match.fun(plotFUN[10])
        plot.11 = match.fun(plotFUN[11]); plot.12 = match.fun(plotFUN[12]) 
        plot.13 = match.fun(plotFUN[13]); plot.14 = match.fun(plotFUN[14]) 
        plot.15 = match.fun(plotFUN[15]); plot.16 = match.fun(plotFUN[16]) 
        plot.17 = match.fun(plotFUN[17]); plot.18 = match.fun(plotFUN[18]) 
        plot.19 = match.fun(plotFUN[19])        
        pick = 1
        while (pick > 0) { pick = menu (
            ### choices = paste("plot:", choices),
            choices = paste(" ", choices), 
            title = "\nMake a plot selection (or 0 to exit):")
            # up to 19 plot functions ...
            switch (pick, 
                plot.1(x),  plot.2(x),  plot.3(x),  plot.4(x),  plot.5(x), 
                plot.6(x),  plot.7(x),  plot.8(x),  plot.9(x),  plot.10(x),
                plot.11(x), plot.12(x), plot.13(x), plot.14(x), plot.15(x), 
                plot.16(x), plot.17(x), plot.18(x), plot.19(x)) 
        } 
    }
                              
    # Plot:
    if (is.numeric(which)) {
        Which = rep(FALSE, times = length(choices))
        Which[which] = TRUE
        which = Which
    }
    if (which[1] == "all") {
        which = rep(TRUE, times = length(choices))
    }
    if (which[1] == "ask") {
        multPlot(x, choices, ...) 
    } else {
        for ( i in 1:length(which) ) {
            FUN = match.fun(plotFUN[i])
            if (which[i]) FUN(x) 
        } 
    }
            
    # Return Value:
    invisible(x)
}


################################################################################


.terms.fREG = 
function(object, formula = Y ~ X1)
{  
    select = all.vars(formula)
    print(class(object@fit)[-1])
    
    fit = object@fit
    data = as.data.frame(object@data)
    X = predict(fit, data, type = "terms")[, select[2]]
    Y = predict(fit, data, type = "response")
    
    plot(X, Y, xlab = select[2], ylab = select[1], col = "steelblue", pch = 19)
    grid()   
    rug(X)
    
    invisible()
}


# ------------------------------------------------------------------------------


.response2Plot = 
function(object, formula = Y ~ X1 + X2, N = 10, fun = mean)
{  
    Data = fit@data
    select = all.vars(formula)
    
    X = data[, select[2]]
    Y = data[, select[3]]
    Z = data[, select[1]]
    rangeX = range(X)
    rangeY = range(Y)
    statsData = colStats(Data, fun)
    
    U = seq(rangeX[1], rangeX[2], length = N)
    V = seq(rangeY[1], rangeY[2], length = N)
    newGrid = grid2d(U, V)
    
    newData = matrix(rep(statsData, times = N*N), 
        byrow = TRUE, ncol = ncol(Data))
    colnames(newData) = colnames(Data)
    newData[, select[2]] = newGrid$x
    newData[, select[3]] = newGrid$y
    newData[, select[1]] = NA
    newData = data.frame(newData)
    P = predict(object, newdata = newData)$fit
    
    W = matrix(P, byrow = FALSE, ncol = N)
    persp(U, V, W, xlab = select[2], ylab = select[3], zlab = select[1],
        phi = 30, theta = -40, col = "steelblue")->res
        
    R = sign(fit@residuals)
    points(trans3d(X[R>0], Y[R>0], Z[R>0], pm = res), col = 5, pch =16)
    points(trans3d(X[R<0], Y[R<0], Z[R<0], pm = res), col = 6, pch =16)
    
    invisible()
}


################################################################################

