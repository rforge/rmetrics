
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
# FUNCTION:             REGRESSION MODELLING:
#  eqnsFit               Wrapper Function for "systemfit" and "sem" Models:
#  * OLS                  Ordinary Least Squares
#  * WLS                  Weighted Least Squares
#  * SUR                  Seemingly Unrelated Regressions
#  * 2SLS                 Two-Stage Least Squares
#  * W2SLS                Weighted Two-Stage Least Squares
#  * 3SLS                 Three-Stage Least Squares
#  * W3SLS                Weighted Three-Stage Least Squares
# S3-METHODS:           DESCRIPTION:
#  print.fEQNS           S3: Print method for an object of class fEQNS  
#  summary.fEQNS         S3: Summary method for an object of class fEQNS
# S3-METHODS:           DESCRIPTION:
#  coef.fEQNS            S3: Method for coefficients
#  fitted.fEQNS          S3: Method for fitted values
#  residuals.fEQNS       S3: Method for residual values
#  vcov.fEQNS            S3: Method for variance-covariance Matrix
# S3-METHODS:           DESCRIPTION:
#  predict.fEQNS         S3: Prediction method for an object of class fEQNS
# S-PLUS LIKE:          WRAPPER:
#  systemfitBuiltin      Builtin contributed R package "systemfit"
#  SUR                   SUR Wrapper
################################################################################
# REQUIRED PACLKAGE - PACKAGE DESCRIPTION:
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


setClass("fEQNS", 
    representation(
        call = "call",
        formulas = "list",
        data = "data.frame",
        method = "character",
        fit = "list", 
        predicted.values = "list",  
        title = "character",
        description = "character"))
        
      
# ------------------------------------------------------------------------------

   
eqnsFit = 
function(formulas, data = list(), 
method = c("OLS", "WLS", "SUR", "2SLS", "W2SLS", "3SLS", "W3SLS"), 
title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Common function call for several system equation models.
    
    # Arguments:
    #   formulas - the list of formulas describing the system of 
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
    # The function 'eqnsFit' returns an object of class "fEQNS"
    # with the following slots:
    #   @call - the matched function call
    #   @data - the input data in form of a 'data.frame' or a 
    #       'timeSeries' object
    #   @description - a character string which allows for a brief 
    #       project description
    #   @method - the character string describing the desired method
    #   @formulas - the list of formulas describing the system of 
    #       equations
    #   @title - a character string which allows for a project title
    #   @fit - a summary of the  results as a list returned from the 
    #       underlying functions from the 'systemfit' package.  

    # FUNCTION:
    
    # Fit:
    ans = systemfit(method = method[1], eqns = formulas, 
        data = as.data.frame(data), ...) 
    
    # Rmetrics Conform Output:
    fit = list()
    fit$fit = ans   
    fit$name = "systemfit"
    fit$coef = coef(ans)
    fit$fitted = fitted(ans)
    fit$residuals = residuals(ans)
    fit$vcov = vcov(ans)
        
    # Add Title and Description:
    if (is.null(title)) title = paste(method[1], "Estimation")
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fEQNS",     
        call = match.call(),
        formulas = formulas, 
        data = as.data.frame(data),
        method = as.character(method[1]), 
        fit = fit,
        predicted.values = list(),
        title = as.character(title), 
        description = as.character(description) )
}

    
# ------------------------------------------------------------------------------    


print.fEQNS =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    cat("\nTitle:\n")
    cat(paste(x@method, "Equations Fit\n"))
    
    cat("\nFormulas:\n")
    form = t(matrix(capture.output(x@formulas), ncol = 2))
    cat(form[, 2], sep = "\n")
    
    cat("\nFit Results:\n")
    output = capture.output(summary(x@fit$fit)) [-(1:3)]
    for (i in 1:length(output)) output[i] = paste(" ", output[i])
    cat(output, fill = FALSE, sep = "\n")
    
    cat("Description:\n")
    cat(x@description, "\n\n")
    
    invisible(x)
}  


# ------------------------------------------------------------------------------


summary.fEQNS = 
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Summary Method for an object of class "fEQNS"
    
    # Arguments:
    #   object - an object of class "fEQNS"
    
    # FUNCTION:
    
    # Print:
    print(object, ...)
    
    # Return Value:
    invisible(object)  
}


# ******************************************************************************


coef.fEQNS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the fitted values
    
    # FUNCTION:
    
    # From Slot:
    coef = object@fit$fit$b
    coef = as.data.frame(coef)
   
    # Return Value:
    coef
}


# ------------------------------------------------------------------------------


fitted.fEQNS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the fitted values
    
    # FUNCTION:
    
    # From Slot:
    x = object@fit$fit
    
    # Fitted values:
    fitted = array(NA, c(length(x$eq[[1]]$fitted), x$g))
    colnames(fitted) = as.character(1:ncol(fitted))
    for (i in 1:x$g)  {
        fitted[, i] = x$eq[[i]]$fitted
        colnames(fitted)[i] = paste("eq", as.character(i), sep = "")
    }
    
    # Return Value:
    fitted
}


# ------------------------------------------------------------------------------


residuals.fEQNS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns all residuals
    
    # FUNCTION:
    
    # From Slot:
    x = object@fit$fit
    
    # Residuals:
    residuals = data.frame(eq1 = x$eq[[1]]$residuals)
    if (x$g > 1) {
        for (i in 2:x$g) {
            residuals = cbind(residuals, new = x$eq[[i]]$residuals)
                names(residuals)[i] = paste("eq", as.character(i), sep = "")
        }
    }
    
    # Return Value:
    residuals
}


# ------------------------------------------------------------------------------


vcov.fEQNS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the variance-covariance matrix of the coefficients
    
    # FUNCTION:
    
    # Return Value:
    object@fit$fit$bcov
}


# ------------------------------------------------------------------------------


predict.fEQNS = 
function (object, newdata = object@data, se.fit = FALSE, se.pred = FALSE, 
interval = "none", ci = 0.95, ...) 
{   # A function implemented by Diethelm Wuertz

    # Predict:
    ans = predict.systemfit(object = object@fit$fit, data = newdata,
        se.fit = se.fit, se.pred = se.pred, interval = interval, 
        level = ci, ...)
    
    # Return Value:
    ans
}


# ******************************************************************************


systemfitBuiltin = 
function(builtin = "/fMultivar/demo/funSystemfit.R") 
{   # A function implemented by Diethelm Wuertz

    # Builtin:
    Builtin = paste(.Library, builtin, sep = "") 
    
    # Return Value:
    source(Builtin) 
}


# ------------------------------------------------------------------------------


SUR = 
function(formulas, data = list(), ...)
{   # A function implemented by Diethelm Wuertz

    # Fit:
    ans = eqnsFit(formulas = formulas, data = data, method = "SUR", ...)
        
    # Return Value:
    ans
}


################################################################################

