
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
#  regFit                Wrapper Function for Regression Models
#  .lmFit                 Linear Regression Model
#  .rlmFit                Robust Linear Regression Model
#  .pprFit                Projection Pursuit Regression Model
#  .polymarsFit           Polytochomous MARS Model
#  .nnetFit               Feedforward Neural Network Model
# UTILITY:              DESCRIPTION:
#  .amFormula            Adds s() around term labels
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


regFit <- 
    function (formula, data,
    use = c("lm", "rlm", "am", "ppr", "nnet", "polymars"), 
    trace = TRUE, title = NULL, description = NULL, ...) 
{   
    # A function implemented by Diethelm Wuertz

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
    #   regFit(Y ~ X1 + X2 + X3, regSim())
    
    # FUNCTION:
    
    # Debugging:
    DEBUG = FALSE
   
    # Transform data into a dataframe
    Data = data
    data = as.data.frame(data)
    
    # Function to be called:
    fun = use = match.arg(use)
    if (use == "am") {
        # Use gam() and add smoothing s()
        fun = "gam"
        formula = .amFormula(formula)
    }
    if (use == "polymars") {
        # Use internal call to function .polymars
        fun = ".polymars"
    }

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
    
    # Compose Command to be Called:
    cmd = match.call()
    if (!is.null(cmd$use)) cmd = cmd[-match("use", names(cmd), 0)]    
    cmd[[1]] <- as.name(fun)
    if (use == "ppr"  & !match("nterm",  names(cmd), 0) ) cmd$nterm = 2
    if (use == "nnet" & !match("trace",  names(cmd), 0) ) cmd$trace = FALSE
    if (use == "nnet" & !match("size",   names(cmd), 0) ) cmd$size = 2
    if (use == "nnet" & !match("linout", names(cmd), 0) ) cmd$linout = TRUE
    if (DEBUG) print(cmd)
    
    # Fit regression Model:
    fit <- eval(cmd, parent.frame()) 
    if (DEBUG) print(fit)
      
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

    # Return Value:
    new("fREG",     
        call = as.call(match.call()),
        formula = as.formula(formula), 
        family = as.character(gaussian()),
        method = use,
        data = list(x = data, data = Data),
        fit = fit,
        residuals = fit$residuals,
        fitted = fit$fitted.values,
        title = as.character(title), 
        description = as.character(description)
    )
}


# ------------------------------------------------------------------------------


.amFormula <- 
    function(formula)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Adds s() around term labels
    
    # Note:
    #   Called by regFit() for use="am".
    
    # Example:
    #   > .amFormula(z ~ x + y)
    #   z ~ s(x) + s(y)
    
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

