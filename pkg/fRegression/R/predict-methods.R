
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
# FUNCTION:             DESCRIPTION REGRESSION METHODS:
#  predict.fREG          Predicts values from a fitted regression model
################################################################################


setMethod(f = "predict", signature(object = "fREG"), definition =  
    function(object, newdata, se.fit = FALSE, type = "response", ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Predict method for Regression Modelling, an object of class "fREG"
    
    # FUNCTION:
    
    # Fit:
    fit = object@fit
      
    # Data:
    if (missing(newdata)) newdata = object@data
    newdata = as.data.frame(newdata)
     
    # Predict:
    if (object@method == "nnet" & type == "response") type = "raw"
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
})


# ------------------------------------------------------------------------------


.predict.lm <- predict.lm
.predict.gam <- predict.gam
.predict.glm <- predict.glm 
.predict.ppr <- function(object, ...) { predict(object, ...) }
.predict.nnet <- function(object, ...) { predict(object, ...) }


################################################################################

