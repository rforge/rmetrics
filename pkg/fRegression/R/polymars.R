
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
# INTERFACE:            FROM POLSPLINE - POLYMARS DESCRIPTION:
#  .polymars
#  .polymars.default     Internal Function
#  .predict.polymars     Internal Function
################################################################################


.polymars.formula <- 
    function(formula, data, ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Extract Model Data:
    mf = match.call(expand.dots = FALSE)
    m = match(c("formula", "data"), names(mf), 0L)
    mf = mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf = eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    x <- model.matrix(mt, mf)
    
    # Fit:
    fit = .polymars.default(responses = y, predictors = x, ...) 
    
    # Add to fit:
    fit$fitted.values = fit$fitted
    fit$terms = terms(formula)
    fit$model = mf
    fit$terms = mt
    class(fit) = "polymars"
    
    # Return Value:
    fit
}


# ------------------------------------------------------------------------------


.polymars.default <-  
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
    
    # Fit:
    .Call <- match.call()
    .Call[[1]] <- as.name("polymars")
    ans = eval(.Call, parent.frame())
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.predict.polymars <- 
    function(object, newdata, se.fit = FALSE, ...)
{   
    # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Predict:
    if (missing(newdata)) {
        y = as.vector(object$fitted)
    } else {
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

