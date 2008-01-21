
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
# FUNCTION:             SIMULATION AND PARAMETER ESTIMATION:
#  'fASSETS'             Class representation for "fASSETS" Objects
#  assetsFit             Estimates the parameters of a set of assets
#   method = "norm"       assuming a multivariate Normal distribution
#   method = "snorm"      assuming a multivariate skew-Normal distribution
#   method = "st"         assuming a multivariate skew-Student-t 
# FUNCTION:             PRINT, PLOT AND SUMMARY METHOD: 
#  show.fASSETS          S4: Print method for an object of class fASSETS
#  plot.fASSETS          S3: Plot method for an object of class fASSETS
#  summary.fASSETS       S3: Summary method for an object of class fASSETS 
# REQUIREMENTS:         DESCRIPTION:
#  sn                    Contributed R-package "sn"
################################################################################


setClass("fASSETS", 
    representation(
        call = "call",              # call: The matched function call
        method = "character",       # method: One of "mn", "msn", "mst"
        model = "list",             # model: A list(mu, Omega, alpha, df)    
        data = "data.frame",        # Data: The data records 
        fit = "list",               # fit: Results parameter estimation     
        title = "character",        # title: A short title string      
        description = "character")  # description: A brief description
)


# ------------------------------------------------------------------------------


assetsFit =
function(x, method = c("st", "snorm", "norm"), title = NULL, 
description = NULL, fixed.df = NA, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits the parameters of a multivariate data set of assets
    #   and returns a list with the values for the mean, the covariance,
    #   the skewness, and the fatness of tails.
    
    # Arguments:
    #   x - A multivariate time series, a data frame, or any other
    #       rectangular object of assets which can be converted into
    #       a matrix by the function as.matrix. Optional Dates are 
    #       rownames, instrument names are column names.
    #   type - Which type of distribution should be fitted?
    #       a) norm - multivariate Normal
    #       b) snorm - multivariate skew-Normal
    #       c) st - multivariate skew-Student-t        
    
    # Value:
    #   The function returns a list with the following entries:
    #   mu - Mean values of each asset time series
    #   Omega - Covariance matrix of assets
    #   alpha - Skewness vector
    #   df - Degrees of freedom, measures kurtosis

    # Notes: 
    #   Requires function "msn.mle" ans "mst.mle" from R's GPL licensed 
    #     contributed package "sn", (C) 1998-2004 A. Azzalini.
    #   The list returned by this function can serve as input for the 
    #     function assetsSim().
    
    # FUNCTION:
    
    # Settings:
    assets = as.matrix(x)
    method = method[1]
    colNames = colnames(x)
    
    # Normal Distribution:
    if (method == "norm") {
        # Fit Normal:
        fit = list()
        mu = apply(assets, 2, mean)
        Omega = cov(assets)
        alpha = rep(0, times = length(mu))
        df = Inf 
    }
        
    # Skew-Normal Distribution:
    if (method == "snorm") {
        # Fit skew-Normal:
        fit = mvFit(assets, method = "snorm", ...)
        mu = as.vector(fit@fit$dp$beta)
        Omega = fit@fit$dp$Omega
        alpha = as.vector(fit@fit$dp$alpha)
        df = Inf 
        fit = fit@fit
    }
        
    # Skew-Student-t Distribution:
    if (method == "st") {
        # Fit skew-Student:
        fit = mvFit(assets, method = "st", fixed.df = fixed.df, ...)
        mu = as.vector(fit@fit$beta)
        Omega = fit@fit$dp$Omega
        alpha = as.vector(fit@fit$dp$alpha)
        df = fit@fit$dp$df 
        fit = fit@fit
    }
        
    # Add Names:   
    names(mu) = colNames
    names(alpha) = colNames
    rownames(Omega) = colNames
    colnames(Omega) = colNames
    
    # Add Title:
    if (is.null(title)) 
        title = paste("Fitted Asset Data Model: ", method)
        
    # Add Description:
    if (is.null(description)) 
        description = .description()
        
    # Return Value: 
    new("fASSETS", 
        call = as.call(match.call()), 
        method = as.character(method), 
        model = list(mu = mu, Omega = Omega, alpha = alpha, df = df),
        data = as.data.frame(x), 
        fit = as.list(fit),
        title = as.character(title), 
        description = as.character(description)
        )       
}


# ------------------------------------------------------------------------------


show.fASSETS =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Print Method for an object of class fASSETS
    
    # Arguments:
    #   x - an object of class fASSETS
      
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n")
    cat(as.character(object@title), "\n")
    
    # Call:
    cat("\nCall:\n")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")
          
    # Model Parameters:
    cat("\nModel Parameters:\n")
    print(object@model)
    
    # Description:
    cat("Description:\n")
    print(object@description)
    cat("\n")
    
    # Return Value:
    invisible(object)
}


# ------------------------------------------------------------------------------


setMethod("show", "fASSETS", show.fASSETS)


# ------------------------------------------------------------------------------


plot.fASSETS =  
function(x, which = "ask", ...) 
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Plots a fit from an assets data set or a model
    
    # Arguments:
    #   x - an object of class fASSETS
    #   ... - arguments to be passed
    
    # Notes:
    #   Library 'sn', is version  0.32-2 (2004-03-13), 
    #     (C) 1998-2004 A. Azzalini, GPL
    #   For "fMV" objects have a look in "fMultivar".
    
    # FUNCTION:
    
    # Transform to a S4 object of class "fMV":
    object = new("fMV", 
        call = x@call, 
        method = x@method, 
        model = x@model, 
        data = x@data, 
        fit = x@fit, 
        title = x@title, 
        description = 
        x@description)   
              
    # Use plot method for objects of class "fMV"
    plot(object, which = which, xlab = "Time", ylab = "Value", ...)
    
    # Return value:
    invisible(x)
}


# ------------------------------------------------------------------------------


summary.fASSETS =
function(object, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Summarizes a fit from an assets data set or a model
    
    # Print:
    print(object, ...)
    
    # Plot:
    plot(object, which = which, ...)
    
    # Return value:
    invisible(object)
}


################################################################################

