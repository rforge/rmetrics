
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
# FUNCTION:             SIMULATION:
#  'fREG'                S4 Class Representation
#  regSim                Returns a regression example data set
################################################################################
# FUNCTION:             PARAMETER ESTIMATION:
#  regFit                Wrapper Function for Regression Models
#  gregFit               Wrapper Function for Generalized Regression Models
#  .lmFit                 Linear Regression Model
#  .rlmFit                Robust Linear Regression Model
#  .glmFit                Generalized Linear Model
#  .gamFit                Generalized Additive Model
#  .pprFit                Projection Pursuit Regression Model
#  .polymarsFit           Polytochomous MARS Model
#  .nnetFit               Feedforward Neural Network Model
# REQUIREMENT:          DESCRIPTION:
#  polspline              Contributed R package
################################################################################


################################################################################
# MODEL:        PACKAGE     print   plot   summary   print     predict
#                                    persp           summary
#   lm          stats       x       x      x         x         x
#   rlm         MASS
#   glm         stats       x       -      x         x         x
#   gam         mgcv        x       x      x         x         x
#   ppr         modreg      x       x      x         x         x
#   polymars*   polspline   -       xx     x         -         x
#   nnet        nnet        x       -      x         x         x
#
#   *BUILTIN:
#    polspline  required!
#   *IMPORTANT NOTE:
#    Both packages r-cran-mda and r-cran-polspline are not available on the
#    Debian Server, therefore we made them accessible as Builtin functions
################################################################################


setClass("fREG", 
    # Class Representation
    representation(
        call = "call",
        formula = "formula",
        family = "character",  
        method = "character",
        data = "timeSeries",
        fit = "list",
        residuals = "timeSeries",
        fitted = "timeSeries",
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
      

################################################################################


regFit = 
function (formula, data,
use = c("lm", "rlm", "am", "ppr", "nnet", "polymars"), 
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
    #   POLYMARS    Polytochomous MARS Modeling
    #   NNET        Feedforward Neural Net
    
    # Notes:
    #   Available Methods are
    #   "print", "plot", "summary", and "predict" method
    #   coefficients, "residuals" "fitted", "vcov" method
    
    # Example:
    #   regFit(Y ~ X1 + X2, regSim())
    
    # FUNCTION:
    
    # Trace:
    trace = FALSE
   
    # Get Method:
    if (!(class(data) == "timeSeries")) {
        data = as.timeSeries(data, silent = TRUE)
    }
    
    # Function to be called:
    fun = use = match.arg(use)
    if (use == "am") {
        fun = "gam"
        formula = .amFormula(formula)
    }
    if (use == "polymars") fun = ".polymars"

    # Title:
    if (is.null(title)) {
        if (use == "lm") title = "Linear Regression Modelling"
        if (use == "rlm") title = "Robust Linear Regression Modelling"
        if (use == "am") title = "Additive Modelling"
        if (use == "ppr") title = "Projection Pursuit Regression"
        if (use == "polymars") title = "Polytochomous MARS Modeling"
        if (use == "nnet") title = "Feedforward Neural Network Modelling" 
    } 
    
    # Description:
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
    if (trace) print(cmd)
    fit <- eval(cmd, parent.frame()) 
    if (trace) print(fit)
      
    # Add to Fit:
    if (is.null(fit$xlevels)) fit$xlevels = list()
    fit$residuals = as.vector(fit$residuals)    
    fit$fitted.values = as.vector(fit$fitted.values)
    fit$parameters = fit$coef
    if (use == "am") fit$fake.formula = interpret.gam(formula)$fake.formula
    noFitModels = c("ppr", "nnet")
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
        fitted = fittedTS,
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
    #   Common function call for generalized regression models.
    
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
    fun = use = match.arg(use)

    # Title:
    if (is.null(title)) {
        if (use == "glm") title = "Generalized Linear Modelling"
        if (use == "gam") title = "Generalized Additive Modelling"
    }  
    
    # Description:
    if (is.null(description)) {
        description  = .description()
    }  
    
    # Evaluate:
    cmd = match.call()
    if (!is.null(cmd$use)) cmd = cmd[-match("use", names(cmd), 0)]    
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
        method = as.character(use),
        data = timeSeries(data, rownames(data)),
        fit = fit,
        residuals = timeSeries(fit$residuals, rownames(data)),
        fitted = timeSeries(fit$fitted.values, rownames(data)),
        title = as.character(title), 
        description = as.character(description) 
    )
}


################################################################################


.amFormula =
function(formula)
{
    # Description:
    #   Adds s() around term labels
    
    # FUNCTION:
    
    TF = terms(formula)
    attTF = attr(TF, "term.labels")
    newF = NULL
    for (i in 1:length(attTF)) {
        addF = paste(" s(", attTF[i], ") ", sep = "")
        newF = paste(newF, addF, sep = "+")
    }
    newF = substr(newF, 3, 99)
    if (attr(TF, "intercept") == 0) newF = paste(newF, "- 1", sep = "")
    newF = paste("~", newF)
    if (attr(TF, "response") == 1) newF = paste(as.character(formula)[2], newF)
    
    # Return Value:
    as.formula(newF)
}


################################################################################

