
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
# FUNCTION:               SPECIFICATION: 
#  'garchSpec'             S4: garchSpec Class representation 
#  garchSpec               S4: Creates a 'garchSpec' object from scratch
#  show.garchSpec          S4: Print method for an object of class 'garchSpec'
################################################################################


setClass("garchSpec", 
    representation(
        call = "call",
        formula = "formula",        
        model = "list",
        presample = "matrix",
        distribution = "character",
        rseed = "numeric")  
)
        
        
# ------------------------------------------------------------------------------


garchSpec <- 
    function (model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), 
    presample = NULL, cond.dist = c("rnorm", "rged", "rstd", "rsnorm", 
    "rsged", "rsstd"), rseed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a "garchSpec" object from scratch.
    
    # Arguments:
    #   model - a list with the model parameters as entries
    #     omega - the variance value for GARCH/APARCH 
    #       specification,
    #     alpha - a vector of autoregressive coefficients 
    #       of length p for the GARCH/APARCH specification,
    #     gamma - a vector of leverage coefficients of 
    #       length p for the APARCH specification,
    #     beta - a vector of moving average coefficients of 
    #       length q for the GARCH/APARCH specification,
    #     mu - the mean value for ARMA specification,
    #     ar - a vector of autoregressive coefficients of 
    #       length m for the ARMA specification,
    #     ma - a vector of moving average coefficients of 
    #       length n for the ARMA specification,
    #     delta - the exponent value used in the variance equation.
    #     dist - a numeric value or a vector listing the 
    #       distributional parameters.
    #   presample - either a multivariate "timeSeries", a 
    #       multivariate "ts", a "data.frame" object or a numeric 
    #       "matrix" with 3 columns and at least max(m,n,p,q) 
    #       rows. The first culumn are the innovations, the second
    #       the conditional variances, and the last the time series.
    #   condd.dist - a character string naming the distribution 
    #       function.
    #   rseed - optional random seed.
    
    # Slots:
    #   call - the function call.
    #   formula - a formula object describing the model, e.g. 
    #       ARMA(m,n) + GARCH(p,q). ARMA can be missing or 
    #       specified as AR(m) or MA(n) in the case of pure 
    #       autoregressive or moving average models. GARCH may 
    #       alternatively specified as ARCH(p) or APARCH(p,q).
    #       If formula is set to "NA", the formula is constructed
    #       from the "model" list.
    #   model - as declared in the input.
   
    # FUNCTION:
    
    # Seed:
    if (is.null(rseed)) {
        rseed = 0
    } else {
        set.seed(rseed)
    }
    
    # Add Missing Default Values:
    if (!any(names(model) == "omega")) {
        model$omega = 1.0e-6
    }
    if (!any(names(model) == "alpha")) {
        model$alpha = 0.1
    }
    if (!any(names(model) == "beta")) {
        model$beta = NULL
    }
              
    # Define Missing GARCH Coefficients:
    formula.var = "garch" 
    if (is.null(model$omega)) {
        model$omega = 1.0e-6
    }
    if (is.null(model$alpha)) {
        model$alpha = order.alpha = 0
    } else {
        order.alpha = length(model$alpha)     
    }
    if (is.null(model$gamma)) {
        model$gamma = rep(0, times = length(model$alpha)) 
    } else {
        formula.var = "aparch" 
    }   
    if (is.null(model$beta)) {
        model$beta = order.beta = 0
        formula.var = "arch"
    } else {
        order.beta = length(model$beta)
    } 
       
    # Define Missing Mean Value and Autoregressive Coefficients:
    formula.mean = ""
    if(is.null(model$mu)) {
        model$mu = 0  
    }
    if (is.null(model$ar)) {
        model$ar = order.ar = 0 
    } else {
        order.ar = length(model$ar) 
    }   
    if (is.null(model$ma)) {
        model$ma = order.ma = 0 
    } else {
        order.ma = length(model$ma) 
    }
           
    # Define Missing Delta Exponent:
    if (is.null(model$delta)) {
        model$delta = 2 
    } else {
        formula.var = "aparch" 
    }
    
    # Define Distributional Parameters:
    distribution = cond.dist[1]
    if (is.null(model$skew)) {                      
        if (distribution == "rnorm")  model$skew = c(skew = NULL)
        if (distribution == "rged")   model$skew = c(skew = NULL)
        if (distribution == "rstd")   model$skew = c(skew = NULL)
        if (distribution == "rsnorm") model$skew = c(skew = 0.9)
        if (distribution == "rsged")  model$skew = c(skew = 0.9)
        if (distribution == "rsstd")  model$skew = c(skew = 0.9) 
    } else { 
        names(model$skew) = "skew" 
    }
    if (is.null(model$shape)) {                      
        if (distribution == "rnorm")  model$shape = c(shape = NULL)
        if (distribution == "rged")   model$shape = c(shape = 2)
        if (distribution == "rstd")   model$shape = c(shape = 4)
        if (distribution == "rsnorm") model$shape = c(shape = NULL)
        if (distribution == "rsged")  model$shape = c(shape = 2)
        if (distribution == "rsstd")  model$shape = c(shape = 4) 
    } else { 
        names(model$shape) = "shape" 
    }
    
    # Compose Formula Object:
    if (order.ar > 0 && order.ma == 0) {
        formula.mean = paste ("~ ar(", as.character(order.ar), ")", 
            sep = "")
    }
    if (order.ar == 0 && order.ma > 0) {
        formula.mean = paste ("~ ma(", as.character(order.ma), ")", 
            sep = "")
    }
    if (order.ar > 0 && order.ma > 0) {
        formula.mean = paste ("~ arma(", as.character(order.ar), ", ",
            as.character(order.ma), ")", sep = "")
    }
    if (formula.mean == "") {
        formula.mean = "~ " 
    } else {
        formula.mean = paste(formula.mean, " + ") 
    }       
    if (order.beta == 0) {
        formula.var = paste(formula.var, "(", as.character(order.alpha), 
            ")", sep = "")  
    } else {
        formula.var = paste(formula.var, "(", as.character(order.alpha), 
            ", ", as.character(order.beta), ")", sep = "")  
    }
    formula = paste(formula.mean, formula.var)
   
    # Define Missing Presample:
    order.max = max(order.ar, order.ma, order.alpha, order.beta)
    iterate = TRUE
    if (!is.matrix(presample)) {
        if (is.null(presample)) {
            iterate = FALSE
            n.start = order.max 
        } else {
            n.start = presample 
        }
        z = rnorm(n = n.start)
        # GARCH(p, q)
        h = rep(model$omega/(1-sum(model$alpha)-sum(model$beta)), 
            times = n.start)
        y = rep(model$mu/(1-sum(model$ar)), times = n.start) 
        # APARCH :
        # ...
    } else {
        z = presample[, 1]
        h = presample[, 2]
        y = presample[, 3]
    }
    presample = cbind(z, h, y)       
    # Presample Iteration:
    if (iterate) {
        n.iterate = length(z) - order.max
        deltainv = 1/model$delta
        for (i in n.iterate:1) {
            h[i] = model$omega +    
                sum(model$alpha*(abs(abs(y[i+(1:order.alpha)]) - 
                    model$gamma*y[i+(1:order.alpha)])^model$delta)) +
                sum(model$beta*h[i+(1:order.beta)]) 
            y[i] = model$mu  +  
                sum(model$ar*y[i+(1:order.beta)]) +
                sum(model$ma*(h[i+(1:order.beta)]**deltainv)) +
                h[i]^deltainv * z[i] 
        }
        presample = cbind(z, h, y) 
    }
    rownames(presample) = as.character(0:(1-length(z)))
    
    # Result:
    ans = new(
        "garchSpec",
            call = match.call(),     
            formula = as.formula(formula), 
            model = list(omega = model$omega, alpha = model$alpha, 
                gamma = model$gamma, beta = model$beta, mu = model$mu, 
                ar = model$ar, ma = model$ma, delta = model$delta, 
                skew = model$skew, shape = model$shape), 
            presample = as.matrix(presample),
            distribution = as.character(distribution),
            rseed = as.numeric(rseed)
        )    
        
    # Return Value:
    ans     
}


# ------------------------------------------------------------------------------


show.garchSpec =
    function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S4 Print Method for objects of class 'garchSpec'
    
    # Arguments:
    #   object - Object of class 'garchSpec'
    
    # FUNCTION:
    
    # Formula:
    x = object
    cat("\nFormula: \n ")
    cat(as.character(x@formula))
    
    # Model:
    cat("\nModel:")
    if (sum(abs(x@model$ar)) != 0) 
        cat("\n ar:   ", x@model$ar)
    if (sum(abs(x@model$ma)) != 0)    
        cat("\n ma:   ", x@model$ma)
    if (x@model$mu != 0)              
        cat("\n mu:   ", x@model$mu)
    if (x@model$omega != 0)           
        cat("\n omega:", x@model$omega)
    if (sum(abs(x@model$alpha)) != 0) 
        cat("\n alpha:", x@model$alpha)
    if (sum(abs(x@model$gamma)) != 0) 
        cat("\n gamma:", x@model$gamma)
    if (sum(abs(x@model$beta)) != 0)  
        cat("\n beta: ", x@model$beta)
    if (x@model$delta != 2)  
        cat("\n delta:", x@model$delta)
    
    # Distribution: 
    cat("\nDistribution: \n ")
    cat(x@distribution)   
    if (x@distribution != "rnorm") {
        if (x@distribution == "rsnorm") {
            cat("\nDistributional Parameters: \n")
            cat(" xi =", x@model$skew)
        }
        if (x@distribution == "rged" | x@distribution == "rstd") {
            cat("\nDistributional Parameter: \n")
            cat(" nu =", x@model$shape) 
        }
        if (x@distribution == "rsged" | x@distribution == "rsstd") {
            cat("\nDistributional Parameters: \n")
            cat(" nu =", x@model$shape, " xi =", x@model$skew)
        }
    }
    
    # Seed: 
    if (x@rseed != 0) {
        cat("\nRandom Seed: \n ")
        cat(x@rseed)
    }     
    
    # Presample:
    cat("\nPresample: \n")
    n = -(length(x@presample[, 1])-1)
    time = 0:n
    print(data.frame(cbind(time, x@presample)))
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------
   
    
setMethod("show", "garchSpec", show.garchSpec)


################################################################################

