
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
# FUNCTION:               SIMULATION:
#  garchSim                Simulates a GARCH/APARCH process
#  .garchSim               Simulates a GARCH/APARCH from specification object
################################################################################


garchSim <- 
    function (model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), n = 100, 
    n.start = 100, presample = NULL, cond.dist = c("rnorm", "rged", "rstd", 
    "rsnorm", "rsged", "rsstd"), rseed = NULL, returnClass = c("ts", "numeric"))
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates a time series process from the GARCH family
    
    # Arguments:
    #   model - either a specification object of class 'garchSpec' 
    #     or a list with the model parameters as entries
    #     ar - a vector of autoregressive coefficients of 
    #       length m for the ARMA specification,
    #     ma - a vector of moving average coefficients of 
    #       length n for the ARMA specification,
    #     omega - the variance value for GARCH/APARCH 
    #       specification,
    #     alpha - a vector of autoregressive coefficients 
    #       of length p for the GARCH/APARCH specification,
    #     gamma - a vector of leverage coefficients of 
    #       length p for the APARCH specification,
    #     beta - a vector of moving average coefficients of 
    #       length q for the GARCH/APARCH specification,
    #     mu - the mean value for ARMA specification,
    #     delta - the exponent value used in the variance 
    #       equation.
    #     skew - a numeric value for the skew parameter.
    #     shape - a numeric value for the shape parameter.
    #   n - an integer, the length of the series
    #   n.start - the length of the warm-up sequence to reduce the 
    #       effect of initial conditions. 
    #   presample - either a multivariate "timeSeries", a 
    #       multivariate "ts", a "data.frame" object or a numeric 
    #       "matrix" with 3 columns and at least max(m,n,p,q) 
    #       rows. The first culumn ...
    #   cond.dist - a character string naming the conditional distribution 
    #       function. Valid strings are: "rnorm", "rged", "rstd", "rsnorm", 
    #       "rsged", and "rsstd".
    
    # Notes:
    #   The parameters omega, alpha, and beta in the model list
    #   must be explicitely specified, otherwise a warning message 
    #   will be printed. The other parameters will be assigned by 
    #   default values.
    
    # FUNCTION:
    
    # Match Arguments:
    returnClass = match.arg(returnClass)
    
    # Simulate Series:
    if (class(model) == "list") {
        # Create Specification Object:
        spec = garchSpec(model = model, presample = presample, 
            cond.dist = cond.dist, rseed = rseed)
        ans = .garchSim(n = n, n.start = n.start, spec = spec)
    } else if (class(model) == "garchSpec") {
        ans = .garchSim(n = n, n.start = n.start, spec = model)
    } else {
        stop("model must be an object of class list or garchSpec")
    }
    
    if (returnClass == "numeric") {
        ans = as.numeric(as.vector(ans))
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.garchSim <- 
    function(n = 1000, n.start = 1000, spec = garchSpec())
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Simulates GARCH series from 'garchSpec'
    
    # Arguments:
    #   n - length of time series
    #   spec - GARCH specification structure
    
    # FUNCTION:
    
    # Random Seed:
    if (spec@rseed != 0) set.seed(spec@rseed)
  
    # Enlarge Series:
    n = n + n.start
    
    # Determine Orders:
    order.ar = order.ma = order.alpha = order.gamma = order.beta = 1    
    if (sum(abs(spec@model$ar)) != 0) {  
        model.ar = spec@model$ar
        order.ar = length(spec@model$ar) 
    } else {
        model.ar = 0
    }
    if (sum(abs(spec@model$ma)) != 0) {
        model.ma = spec@model$ma
        order.ma = length(spec@model$ma)
    } else {
        model.ma = 0
    }
    if (sum(abs(spec@model$alpha)) != 0) {
        model.alpha = spec@model$alpha
        order.alpha = length(spec@model$alpha)
    } else {
        model.alpha = 0
    }
    if (sum(abs(spec@model$gamma)) != 0) {
        model.gamma = spec@model$gamma
        order.gamma = length(spec@model$gamma)
    } else {
        model.gamma = 0
    }
    if (sum(abs(spec@model$beta)) != 0) {
        model.beta = spec@model$beta
        order.beta = length(spec@model$beta)
    } else {
        model.beta = 0
    }
  
    # Create Innovations:
    if (spec@distribution == "rnorm")     
        z = rnorm(n)
    if (spec@distribution == "rged")      
        z = rged(n, nu = spec@model$shape)
    if (spec@distribution == "rstd")       
        z = rstd(n, nu = spec@model$shape)
    if (spec@distribution == "rsnorm") 
        z = rsnorm(n, xi = spec@model$skew)
    if (spec@distribution == "rsged")  
        z = rsged(n, nu = spec@model$shape, xi = spec@model$skew)
    if (spec@distribution == "rsstd")   
        z = rsstd(n, nu = spec@model$shape, xi = spec@model$skew)
    
    # Expand to whole Sample:
    delta = spec@model$delta
    z = c(rev(spec@presample[, 1]), z)
    h = c(rev(spec@presample[, 2])^delta, rep(NA, times = n))
    y = c(rev(spec@presample[, 3]), rep(NA, times = n))
    m = length(spec@presample[, 1])
    names(z) = names(h) = names(y) = NULL
        
    # Iterate APARCH Model:
    # [This includes the GARCH case]
    deltainv = 1/delta
    eps = h^deltainv*z
    for (i in (m+1):(n+m)) {
        h[i] =  spec@model$omega +  
            sum(model.alpha*(abs(eps[i-(1:order.alpha)]) -  
                model.gamma*(eps[i-(1:order.alpha)]))^delta) +
            sum(model.beta*h[i-(1:order.beta)]) 
        eps[i] = h[i]^deltainv * z[i]
        y[i] = spec@model$mu  +    
            sum(model.ar*y[i-(1:order.ar)]) +
            sum(model.ma*(h[i-(1:order.ma)]**deltainv)) + eps[i]   
    }
    
    # Sample:       
    data = cbind(
        z = z[(m+1):(n+m)], 
        h = h[(m+1):(n+m)]^deltainv, 
        y = y[(m+1):(n+m)])
    rownames(data) = as.character(1:n)
    data = data[-(1:n.start),]
        
    # Add Series:
    ans = as.ts(as.vector(data[, 3]))
    class(spec) = "garchSpec"
    attr(ans, "control") = list(garchSpec = spec)
  
    # Return Value: 
    ans
}


################################################################################

