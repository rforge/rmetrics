
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
# METHOD:                 PREDICTION:
#  predict.fGARCH          S3 prediction method for an object of class 'fGARCH'
################################################################################


predict.fGARCH = 
function(object, n.ahead = 10, trace = FALSE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:  
    #   S3 Prediction method for an object of class fGARCH
    
    # Arguments:
    #   object - an object of class fGARCH as returned by the
    #       function garchFit().
    #   n.ahead - number of steps to be forecasted, an integer
    #       value, by default 10)
    #   trace - should the prediction be traced? A logical value,
    #       by default FALSE)
    
    # FUNCTION:
    
    # Retrieve "fit" from Parameter Estimation:
    fit = object@fit
    
    # Get ARMA(u,v)-GARCH(p,q) Order:
    u = fit$series$order[1]
    v = fit$series$order[2]
    p = fit$series$order[3]
    q = fit$series$order[4]
    max.order = max(u, v, p, q)
    
    # Get Start Conditions:
    h.start = fit$series$h.start
    llh.start = fit$series$llh.start  
    index = fit$params$index
    params = fit$params$params
    par = fit$par
    Names = names(index)
    for (Name in Names) params[Name] = par[Name]
    Names = names(params)
    
    # Retrieve From Initialized Parameters:
    cond.dist = fit$params$cond.dist
    
    # Extract the Parameters by Name:
    leverage = fit$params$leverage
    mu = params["mu"]
    if (u > 0) {
        ar = params[substr(Names, 1, 2) == "ar"] 
    } else {
        ar = c(ar1 = 0)
    }
    if (v > 0) {
        ma = params[substr(Names, 1, 2) == "ma"] 
    } else {
        ma = c(ma1 = 0)
    }
    omega = params["omega"]
    if (p > 0) {
        alpha = params[substr(Names, 1, 5) == "alpha"] 
    } else {
        alpha = c(alpha1 = 0)
    }
    if (p > 0 & leverage) {
        gamma = params[substr(Names, 1, 5) == "gamma"] 
    } else {
        gamma = c(gamma1 = 0)
    }
    if (q > 0) {
        beta  = params[substr(Names, 1, 4) == "beta"] 
    } else {
        beta = c(beta1 = 0)
    }
    delta = params["delta"]
    skew = params["skew"]
    shape = params["shape"]
    
    # Trace Parameters:
    if (trace) {
        cat("\nModel Parameters:\n")
        print(c(mu, ar, ma, omega, alpha, gamma, beta, delta, skew, shape))
    }
    
    # Retrieve Series Lengths:
    M = n.ahead
    N = length(object@data$x)
    
    # Get and Extend Series:
    x = c(object@data$x, rep(mu, M))
    h = c(object@h.t, rep(0, M))
    z = c(fit$series$z, rep(mu, M))
    
    # Forecast and Optionally Trace Mean Model:
    # Note we set maxit=0 to get an object of class Arima with fixed
    #   init parameters ...
    ARMA = arima(x = object@data$x, order = c(max(u, 1), 0, max(v, 1)), 
        init = c(ar, ma, mu), transform.pars = FALSE, optim.control = 
        list(maxit = 0))
    prediction = predict(ARMA, n.ahead)
    meanForecast = as.vector(prediction$pred)
    meanError = as.vector(prediction$se)
    if (trace) {
        cat("\nForecast ARMA Mean:\n") 
        print(ARMA)
        cat("\n")
        print(prediction)
    }
    
    # Forecast and Optionally Trace Variance Model:
    var.model = fit$series$model[2] 
    # Forecast GARCH Variance:
    if (var.model == "garch") {
        if (trace) cat("\nForecast GARCH Variance:\n")
        for (i in 1:M) {
            h[N+i] = omega  + sum(beta*h[N+i-(1:q)])
            for (j in 1:p) {
                if (i-j > 0) {
                    s = h[N + i - j]
                } else { 
                    s = z[N + i - j]^2
                }
                h[N+i] = h[N+i] + alpha[j] * s
            }
        }
    }    
    # Forecast APARCH Variance:
    if (var.model == "aparch") {
        if (trace) cat("\nForecast APARCH Variance:\n")
        for (i in 1:M) {
            h[N+i] = omega  + sum(beta*h[N+i-(1:q)])
            for (j in 1:p) {
                kappa = garchKappa(cond.dist = "dnorm", gamma = gamma[j],
                    delta = delta, skew = skew, shape = shape)
                if (i-j > 0) {
                    s = kappa * h[N + i - j]
                } else { 
                    s = kappa 
                }
                h[N+i] = h[N+i] + alpha[j] * s
            }
        }
    }
    
    # Standard Deviations:
    standardDeviation = h^(1/delta)
        
    # Result:
    forecast = data.frame(
        meanForecast = meanForecast, 
        meanError = meanError, 
        standardDeviation = standardDeviation[-(1:N)])
    
    # Return Value:
    forecast
}


################################################################################

