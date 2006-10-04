
# This library is free software; you can redistribute it and/or
# Modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# Version 2 of the License, or (at your option) any later version.
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
# For this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# For the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# For the code accessed (or partly included) from contributed R-ports
# And other sources
#   see Rmetrics's copyright file


################################################################################
# REQUIRED PACKAGE:     DESCRIPTION:  
#  .systemfitBuiltin     
# FUNCTION:             SYSTEMFIT WRAPPER:
#  'fSYSTEM'             S4 Class Representation
#  systemFit             Wrapper Function for "systemfit" and "sem" Models:
#  * OLS                  Ordinary Least Squares
#  * WLS                  Weighted Least Squares
#  * SUR                  Seemingly Unrelated Regressions
#  * 2SLS                 Two-Stage Least Squares
#  * W2SLS                Weighted Two-Stage Least Squares
#  * 3SLS                 Three-Stage Least Squares
#  * W3SLS                Weighted Three-Stage Least Squares
# S3-METHODS:           DESCRIPTION:
#  print.fSYSTEM         S3: Print method for an object of class fSYSTEM  
#  ? plot 
#  summary.fSYSTEM       S3: Summary method for an object of class fSYSTEM
# S3-METHODS:           DESCRIPTION:
#  coef.fSYSTEM          S3: Method for coefficients
#  coefficients.fSYSTEM  S3: Synonyme for coef.fSYSTEM
#  fitted.fSYSTEM        S3: Method for fitted values
#  residuals.fSYSTEM     S3: Method for residual values
# S3-METHODS:           DESCRIPTION:
#  predict.fSYSTEM       S3: Prediction method for an object of class fSYSTEM
# FINMETRICS LIKE:      WRAPPER:
#  SUR                   SUR Wrapper
################################################################################


################################################################################
# REQUIRED PACKAGE - PACKAGE DESCRIPTION:
# Package: systemfit
# Version: 0.7-2
# Date: 2004/08/19
# Title: Simultaneous Equation Estimation Package
# Author: Jeff D. Hamann <jeff.hamann@forestinformatics.com> and
#   Arne Henningsen <ahenningsen@agric-econ.uni-kiel.de>
# Maintainer: Jeff D. Hamann <jeff.hamann@forestinformatics.com>
# Depends: R (>= 1.8.0)
# Description: This package contains functions for fitting simultaneous
#   systems of linear and nonlinear equations using Ordinary Least
#   Squares (OLS), Weighted Least Squares (WLS), Seemingly Unrelated
#   Regressions (SUR), Two-Stage Least Squares (2SLS), Weighted
#   Two-Stage Least Squares (W2SLS), Three-Stage Least Squares (3SLS),
#   and Weighted Three-Stage Least Squares (W3SLS).
# License: GPL version 2 or newer
# URL: http://www.r-project.org, http://www.forestinformatics.com, 
#   http://www.arne-henningsen.de
################################################################################


.systemBuiltin =
function(builtin = "/fMultivar/demo/systemfitBuiltin.R") 
{
    sink("@sink@")
    Builtin = paste(.Library, builtin, sep = "")
    source(Builtin)
    sink()
    unlink("@sink@")
}


# ------------------------------------------------------------------------------


setClass("fSYSTEM", 
    representation(
        call = "call",
        formula = "list",
        method = "character",
        parameter = "list",
        data = "list",
        fit = "list", 
        residuals = "data.frame",
        fitted.values = "data.frame",
        predicted.values = "data.frame",  
        title = "character",
        description = "character"
    )
)
       
      
# ------------------------------------------------------------------------------

   
systemFit = 
function(formula, data = list(), 
method = c("OLS", "WLS", "SUR", "2SLS", "W2SLS", "3SLS", "W3SLS"), 
title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Common function call for several system equation models.
    
    # Arguments:
    #   formula - the list of formulae describing the system of 
    #       equations
    #   data - the input data set in form of a 'data.frame' or 
    #       'timeSeries' object
    #   method - a character string describing the desired method, 
    #       one of: "OLS", "WLS", "SUR", "2SLS", "W2SLS", "3SLS", 
    #       or "W3SLS".
    #   title - a character string which allows for a project title
    #   description - a character string which allows for a brief 
    #       description
    #   ... - additional optional arguments to be passed to the 
    #       underlying function 'systemfit' 
    
    # Value:
    # The function 'systemFit' returns an object of class "fSYSTEM"
    # with the following slots:
    #   @call - the matched function call
    #   @data - the input data in form of a 'data.frame' or a 
    #       'timeSeries' object
    #   @description - a character string which allows for a brief 
    #       project description
    #   @method - the character string describing the desired method
    #   @formula - the list of formulae describing the system of 
    #       equations
    #   @title - a character string which allows for a project title
    #   @fit - a summary of the  results as a list returned from the 
    #       underlying functions from the 'systemfit' package.  

    # FUNCTION:
    
    # Match Arguments:
    method = match.arg(method)  
    
    # Fit:
    fit = systemfit(method = method, eqns = formula, 
        data = as.data.frame(data), ...) 
    class(fit) = c("list", "systemfit")
    
    # Add Rmetrics Conform Output: 
    fit$name = "systemfit"
    fit$coef = coef(fit)
        
    # Add Title and Description:
    if (is.null(title)) title = paste(method[1], "Estimation")
    if (is.null(description)) description = .description()
    
    # Return Value:
    new("fSYSTEM",     
        call = match.call(),
        formula = formula, 
        method = as.character(method), 
        parameter = list(),
        data = list(data = data),
        fit = fit,
        residuals = data.frame(residuals(fit)),
        fitted.values = data.frame(fitted(fit)), 
        predicted.values = data.frame(),
        title = as.character(title), 
        description = as.character(description) )
}

    
# ------------------------------------------------------------------------------    


print.fSYSTEM =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n ")
    cat(paste(x@method, "Equations Fit\n"))
    
    # Formulas:
    cat("\nFormulas:\n")
    form = t(matrix(capture.output(x@formula), ncol = 2))
    cat(paste("", form[, 2]), sep = "\n")
    
    # Coefficidents:
    cat("\nCoefficients:\n")
    output = capture.output(data.frame(coef(x)))  
    for (i in 1:length(output)) output[i] = paste("", output[i])
    cat(output, fill = FALSE, sep = "\n")
    cat("\n")
    
    # Description:
    cat("Description:\n ")
    cat(x@description, "\n\n")
    
    # Return Value:
    invisible(x)
}  


# ------------------------------------------------------------------------------


# plot.fSYSTEM = 


# ------------------------------------------------------------------------------


summary.fSYSTEM = 
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Summary Method for an object of class "fSYSTEM"
    
    # Arguments:
    #   object - an object of class "fSYSTEM"
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n ")
    cat(paste(object@method, "Equations Fit\n"))
    
    # Formulas:
    cat("\nFormulas:\n")
    form = t(matrix(capture.output(object@formula), ncol = 2))
    cat(paste("", form[, 2]), sep = "\n")
    
    # Coefficidents:
    cat("\nFitted Results:\n")
    output = capture.output(summary(object@fit))[-(1:3)]  
    for (i in 1:length(output)) output[i] = paste("", output[i])
    cat(output, fill = FALSE, sep = "\n")
    cat("\n")
    
    # Description:
    cat("Description:\n ")
    cat(object@description, "\n\n")
    
    # Return Value:
    invisible(object)  
}


# ------------------------------------------------------------------------------


coef.fSYSTEM = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the fitted values
    
    # FUNCTION:
    
    # From Slot:
    coef = object@fit$b
    coef = as.data.frame(coef)
   
    # Return Value:
    coef
}


# ------------------------------------------------------------------------------


fitted.fSYSTEM = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the fitted values
    
    # FUNCTION:
    
    # From Slot:
    x = object@fit
    
    # Fitted values:
    fitted = array(NA, c(length(x$eq[[1]]$fitted), x$g))
    colnames(fitted) = as.character(1:ncol(fitted))
    for (i in 1:x$g)  {
        fitted[, i] = x$eq[[i]]$fitted
        colnames(fitted)[i] = paste("eq", as.character(i), sep = "")
    }
    ans = data.frame(fitted)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


residuals.fSYSTEM = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns all residuals
    
    # FUNCTION:
    
    # From Slot:
    x = object@fit
    
    # Residuals:
    residuals = data.frame(eq1 = x$eq[[1]]$residuals)
    if (x$g > 1) {
        for (i in 2:x$g) {
            residuals = cbind(residuals, new = x$eq[[i]]$residuals)
                names(residuals)[i] = paste("eq", as.character(i), sep = "")
        }
    }
    ans = data.frame(residuals)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


predict.fSYSTEM = 
function (object, newdata = object@data, se.fit = FALSE, se.pred = FALSE, 
interval = "none", ci = 0.95, ...) 
{   # A function implemented by Diethelm Wuertz
   
    # FUNCTION:
    
    # Predict - Don't print conflicts:
    sink("@sink@")
    ans = predict.systemfit(object = object@fit, data = newdata,
        se.fit = se.fit, se.pred = se.pred, interval = interval, 
        level = ci, ...)
    sink()
    unlink("@sink@")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


SUR = 
function(formula, data = list(), ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Fit:
    ans = systemFit(formula = formula, data = data, method = "SUR", ...)
        
    # Return Value:
    ans
}


################################################################################


nlsystemFit = 
function(formula, data = list(), method = c("OLS", "SUR", "2SLS", "3SLS"), 
start = NULL, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Common function call for several system equation models.
    
    # Arguments:
    #   formula - the list of formulae describing the system of 
    #       equations
    #   data - the input data set in form of a 'data.frame' or 
    #       'timeSeries' object
    #   method - a character string describing the desired method, 
    #       one of: "OLS", "WLS", "SUR", "2SLS", "W2SLS", "3SLS", 
    #       or "W3SLS".
    #   title - a character string which allows for a project title
    #   description - a character string which allows for a brief 
    #       description
    #   ... - additional optional arguments to be passed to the 
    #       underlying function 'systemfit' 
    
    # Value:
    #   The function 'systemFit' returns an object of class "fSYSTEM"
    #   with the following slots:
    #       @call - the matched function call
    #       @data - the input data in form of a 'data.frame' or a 
    #           'timeSeries' object
    #       @description - a character string which allows for a brief 
    #           project description
    #       @method - the character string describing the desired method
    #       @formula - the list of formula describing the system of 
    #           equations
    #       @title - a character string which allows for a project title
    #       @fit - a summary of the  results as a list returned from the 
    #           underlying functions from the 'systemfit' package.  

    # FUNCTION:
    
    # nlsystemfit(method="OLS", eqns, startvals,
    #     eqnlabels = c(as.character(1:length(eqns))), inst = NULL,
    #     data = list(), solvtol = .Machine$double.eps,
    #     maxiter = 1000, ... )
    
    # Match Arguments:
    method = match.arg(method)  
    
    # Fit:
    fit = nlsystemfit(method = method, eqns = formula, 
        startvals = start, data = as.data.frame(data), ...) 
    class(fit) = c("list", "nlsystemfit.system")
    
    # Add Rmetrics Conform Output: 
    fit$name = "systemfit"
    fit$coef = coef(fit)
        
    # Add Title and Description:
    if (is.null(title)) title = paste(method[1], "Estimation")
    if (is.null(description)) description = .description()
    
    # Return Value:
    new("fSYSTEM",     
        call = match.call(),
        formula = formula, 
        method = as.character(method), 
        parameter = list(),
        data = list(data = data),
        fit = fit,
        residuals = data.frame(residuals(fit)),
        fitted.values = data.frame(fitted(fit)), 
        predicted.values = data.frame(),
        title = as.character(title), 
        description = as.character(description) )
}


# ------------------------------------------------------------------------------


