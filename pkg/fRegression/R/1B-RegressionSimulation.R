
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
#  regSim                Returns a regression example data set
################################################################################


regSim = 
function(model = c("LM3", "LOGIT3", "GAM3"), n = 100, 
returnClass = c("timeSeries", "data.frame"))
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
    returnClass = match.arg(returnClass) 
    
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
    
    # Return Class:
    if (returnClass == "timeSeries") {
        X = dummyDailySeries(x = as.matrix(X), units = colnames(X), 
            zone = myFinCenter, FinCenter = myFinCenter)  
    }
    
    # Return Value:
    X
}
      

################################################################################

