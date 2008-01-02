
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             PARAMETER ESTIMATION:
#  gregFit               Wrapper Function for Generalized Regression Models
#  .glmFit                Generalized Linear Model
#  .gamFit                Generalized Additive Model
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


gregFit <-  
    function (formula, family, data, use = c("glm", "gam"), 
    trace = TRUE, title = NULL, description = NULL, ...) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Common function call for generalized regression models.
    
    # Details:
    #   This is a wrapper function for the following regrssion models:
    #   GLM         Generalized Linear Modelling
    #   GAM         Generalized Additive Modelling
    
    # Notes:
    #   Available Methods are
    #   "print", "plot", "summary", and "predict" method
    #   "coef", "residuals" "fitted", "vcov" method
    
    # FUNCTION:
    
    # Transform data into a dataframe
    Data = data
    data = as.data.frame(data)
    
    # Function to be called:
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
        data = list(),
        fit = fit,
        residuals = fit$residuals,
        fitted = fit$fitted.values,
        title = as.character(title), 
        description = .description() 
    )
}


################################################################################

