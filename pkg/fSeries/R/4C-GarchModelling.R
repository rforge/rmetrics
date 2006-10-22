
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# PART I - FUNCTION:      SPECIFICATION: 
#  setClass[garchSpec]     S4: garchSpec Class representation 
#  garchSpec               S4: Creates a 'garchSpec' object from scratch
#  print.garchSpec         S3: Print method for an object of class 'garchSpec'
#  .garchSpecRUnit         RUnit Testing
################################################################################
# PART II - FUNCTION:     SIMULATION:
#  garchSim                Simulates a GARCH/APARCH process
#  .garchSim               Simulates a GARCH/APARCH from specification object
#  .garchSimRUnit          RUnit Testing
################################################################################
# PART III - FUNCTION:    PARAMETER ESTIMATION: 
#  setClass[fGARCH]        S4: fGARCH Class representation   
#  garchFit                Fits GARCH and APARCH processes
#  .garchFit               ... old Version
#  .garchInitSeries        Initializes Series
#  .garchInitParameters    Initializes Parameters
#  .garchSetCondDist       Selects conditional density function
#   .garchDist              Defines conditional density function
#  .garchOptimizeLLH       Opimizes log-likelihood function by 'nlminb'/'bfgs'
#   .garchLLH               Computes log-likelihood function
#   .garchHessian           Computes Hessian matrix numerically
#  .garchNames              Slot names, @fit slot, parameters and controls
#  .garchTsFit             Wrapper for 'garch()' from 'tseries' package
# METHODS:                DESCRIPTION:
#  print.fGARCH            S3 Print method for an object of class fGARCH
#  summary.fGARCH          S3 Summary method for an object of class fGARCH
#  plot.fGARCH             S3 Plot method for an object of class fGARCH
#  .interactiveGarchPlot   Utility Function
#  residuals.fGARCH        S3 Residuals method for an object of class fGARCH
#  fitted.fGARCH           S3 Fitted values method for an object of class fGARCH
#  predict.fGARCH          S3 Prediction method for an object of class fGARCH
# STATISTICS:             Description:
#  .truePersistence        Compute Persistence
################################################################################
# PART IV - FUNCTION:     FORECASTING: 
#  garchKappa               Computes Expection for APARCH Models
#  .funE                    Internal function used by kappa()
################################################################################


################################################################################
# FUNCTION:              SPECIFICATION: 
#  setClass[garchSpec]    S4: garchSpec Class representation 
#  garchSpec              S4: Creates a 'garchSpec' object from scratch
#  print.garchSpec        S3: Print method for an object of class 'garchSpec'
#  .garchSpecRUnit        RUnit Testing
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


garchSpec =
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
        if (distribution == "rssdt")  model$skew = c(skew = 0.9) 
    } else { 
        names(model$skew) = "skew" 
    }
    if (is.null(model$shape)) {                      
        if (distribution == "rnorm")  model$shape = c(shape = NULL)
        if (distribution == "rged")   model$shape = c(shape = 2)
        if (distribution == "rstd")   model$shape = c(shape = 4)
        if (distribution == "rsnorm") model$shape = c(shape = NULL)
        if (distribution == "rsged")  model$shape = c(shape = 2)
        if (distribution == "rssdt")  model$shape = c(shape = 4) 
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


print.garchSpec =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Print Method for objects of class 'garchSpec'
    
    # Arguments:
    #   x - Object of class 'garchSpec'
    
    # FUNCTION:
    
    # Formula:
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


.garchSpecRUnit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   R Unit Testing
    
    # Arguments:
    #   none
    
    # FUNCTION:
    
    # Internal print Function:
    Print = function(x) {
        cat("RUnit Test:\n ")
        print(x@call)
        print(x)
        cat("\n")
    }
    
    # ARCH(1) - use default omega and default alpha[1]
    Print(garchSpec(model = list())) 
    
    # ARCH(1) - use default omega and specify alpha
    Print(garchSpec(model = list(alpha = 0.2))) 
    
    # ARCH(1) - specify omega and alpha
    Print(garchSpec(model = list(omega = 3.0e-6, alpha = 0.3)))
    
    # AR(1)-ARCH(1) - use default omega/alpha and specify alpha[1]
    Print(garchSpec(model = list(ar = 0.5))) 
    
    # AR([1,5])-ARCH(1) - use default omega, specify alpha and subset ar[.]
    Print(garchSpec(model = list(ar = c(0.5,0,0,0,0.1), alpha = 0.25)))
    
    # ARMA(1,2)-ARCH(1) - use default omega/alpha and specify ar[1]/ma[2]
    Print(garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3))))
    
    # ARMA(2,2)-ARCH(1) use default omega/alpha and specify ar[2]/ma[2]
    Print(garchSpec(model = list(ar = c(0.5, -0.5), ma = c(0.3,-0.3))))
    
    # ARCH(2) - use default omega and specify alpha[2]
    Print(garchSpec(model = list(alpha = c(0.12, 0.04))))
    
    # GARCH(1,1) - use just defaults
    Print(garchSpec())
    
    # GARCH(1,1) - use default omega and specify alpha/beta
    Print(garchSpec(model = list(alpha = 0.2, beta = 0.7)))
    
    # GARCH(1,1) - specify omega/alpha/beta
    Print(garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8)))
    
    # GARCH(1,2) - use default omega and specify alpha[1]/beta[2]
    Print(garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4))))
    
    # GARCH(2,1) - use default omega and specify alpha[2]/beta[1]
    Print(garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08)))
    
    # rsnorm-ARCH(1) - use defaults with skew Normal
    Print(garchSpec(model = list(dist = 2), cond.dist = "rsnorm"))
    
    # rged-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = 4), cond.dist = "rged"))
    
    # rsged-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsged"))
    
    # rstd-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = 4), cond.dist = "rstd"))
    
    # rsstd-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsstd"))
    
    # TS-GARCH(1,1)
    Print(garchSpec(model = list(delta = 1)))
    
    # AR(1)-t-APARCH(2, 1)
    Print(garchSpec(model = list(mu = 1.0e-4, ar = 0.5, omega = 1.0e-6, 
        alpha = c(0.10, 0.05), gamma = c(0, 0), beta = 0.8, delta = 1.8, 
        dist = c(nu = 4, xi = 0.5)), cond.dist = "rsstd"))
    
    invisible()
}


################################################################################
# PART II:
# SIMULATION:          DESCRIPTION:
#  garchSim             Simulates a GARCH/APARCH process
#  .garchSim            Simulates a GARCH/APARCH from specification object
#  .garchSimRUnit       Unit Testing
################################################################################


garchSim =
function (model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), n = 100, 
n.start = 100, presample = NULL, cond.dist = c("rnorm", "rged", "rstd", 
"rsnorm", "rsged", "rsstd"), rseed = NULL)
{   # A function implemented by Diethelm Wuertz

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

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.garchSim =
function(n = 1000, n.start = 1000, spec = garchSpec())
{   # A function implemented by Diethelm Wuertz
    
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
    # spec@series = data[, 1:2]
    ans = ts(as.vector(data[, 3]))
    attr(ans, "spec") = spec
  
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


.garchSimRUnit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Unit Testing
    
    # Arguments:
    #   none
    
    # FUNCTION:
    
    # ARCH(1)
    spec = garchSpec(model = list())
    print(garchSim(n = 10, model = spec))
    
    # ARCH(1)
    spec = garchSpec(model = list(alpha = 0.1))
    print(garchSim(n = 10, model = spec))
    
    # ARCH(1)
    spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1))
    print(garchSim(n = 10, model = spec))
    
    # AR(1)-ARCH(1)
    spec = garchSpec(model = list(ar = 0.5)) 
    print(garchSim(n = 10, model = spec))
    
    # AR([1,5])-ARCH(1)
    spec = garchSpec(model = list(ar = c(0.5,0,0,0,0.1)))
    print(garchSim(n = 10, model = spec))
    
    # ARMA(1,2)-ARCH(1)
    spec = garchSpec(model = list(ar = 0.5, ma = c(0.3,-0.3)))
    print(garchSim(n = 10, model = spec))
    
    # rsnorn-ARCH(2)
    spec = garchSpec(model = list(alpha = c(0.12, 0.04), dist = 2/3), 
        cond.dist = "rsnorm")
    print(garchSim(n = 10, model = spec))
    
    # GARCH(1,1)
    spec = garchSpec()
    print(garchSim(n = 10, model = spec))
    
    # GARCH(1,1)
    spec = garchSpec(model = list(alpha = 0.1, beta = 0.8))
    print(garchSim(n = 10, model = spec))
    
    # GARCH(1,1)
    spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8))
    print(garchSim(n = 10, model = spec))
    
    # GARCH(1,2)
    spec = garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4)))
    print(garchSim(n = 10, model = spec))
    
    # GARCH(2,1)
    spec = garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08))
    print(garchSim(n = 10, model = spec))
    
    # r[s]norm-GARCH(1,1)   
    spec = garchSpec(model = list(), cond.dist = "rnorm")
    print(garchSim(n = 10, model = spec))
    
    spec = garchSpec(model = list(parm = 2), cond.dist = "rsnorm")
    print(garchSim(n = 10, model = spec))
    
    # r[s]ged-GARCH(1,1)
    spec = garchSpec(model = list(parm = 4), cond.dist = "rged")
    print(garchSim(n = 10, model = spec))
    
    spec = garchSpec(model = list(parm = c(4, 2)), cond.dist = "rsged")
    print(garchSim(n = 10, model = spec))
    
    # r[s]std-GARCH(1,1)
    spec = garchSpec(model = list(parm = 4), cond.dist = "rstd")
    print(garchSim(n = 10, model = spec))
    
    spec = garchSpec(model = list(parm = c(4, 2)), cond.dist = "rsstd")
    print(garchSim(n = 10, model = spec))
    
    # TS-GARCH(1,1)
    spec = garchSpec(list(alpha = 0.1, delta = 1))
    print(garchSim(n = 10, model = spec))
    
    invisible()
}


################################################################################
# PART III - FUNCTION:    PARAMETER ESTIMATION: 
#  setClass[fGARCH]        S4: fGARCH Class representation   
#  garchFit                Fits GARCH and APARCH processes
#  .garchFit               ... old Version
#  .garchInitSeries        Initializes Series
#  .garchInitParameters    Initializes Parameters
#  .garchSetCondDist       Selects conditional density function
#   .garchDist              Defines conditional density function
#  .garchOptimizeLLH       Opimizes log-likelihood function
#   .garchLLH               Computes log-likelihood function
# METHODS:                DESCRIPTION:
#  print.fGARCH            S3 Print method for an object of class fGARCH
#  summary.fGARCH          S3 Summary method for an object of class fGARCH
#  plot.fGARCH             S3 Plot method for an object of class fGARCH
################################################################################



# Class Representation:
setClass("fGARCH", 
    representation(
        call = "call",
        formula = "list",
        method = "character",
        data = "list",
        fit = "list",
        residuals = "numeric",
        fitted = "numeric",
        h.t = "numeric",
        sigma.t = "numeric",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------
      

garchFit = 
function(formula, data, init.rec = c("mci", "uev"), delta = 2, skew = 1, 
shape = 4, cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
include.mean = TRUE, include.delta = NULL, include.skew = NULL, 
include.shape = NULL, leverage = NULL, trace = TRUE, 
algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
control = list(), title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description
    #   Fit parameters to a ARMA-GARCH model
    
    # Call:
    CALL = match.call()
    
    # Get Data:
    mf = match.call(expand.dots = FALSE)
    m = match(c("formula", "data"), names(mf), 0)
    mf = mf[c(1, m)]
    mf[[1]] = as.name(".modelSeries")
    mf$fake = FALSE
    mf$lhs = TRUE
    x = eval(mf, parent.frame())
    x = as.vector(x[, 1])
    if (class(mf$data) == "timeSeries") names(x) = rownames(data)
    # print(head(x))
    
    # Compose Mean and variance Formula:
    allLabels = attr(terms(formula), "term.labels")
    print(allLabels)
    if (length(allLabels) == 2) {
        formula.mean = as.formula(paste("~", allLabels[1]))
        formula.var = as.formula(paste("~", allLabels[2]))
    } else if (length(allLabels) == 1) {
        formula.mean = as.formula("~ arma(0, 0)")
        formula.var = as.formula(paste("~", allLabels[1]))
    }
    # print(formula.mean)
    # print(formula.var)
    
    # Fit:
    ans = .garchFit(formula.mean, formula.var, series = x, init.rec, delta, 
        skew, shape, cond.dist, include.mean, include.delta, include.skew, 
        include.shape, leverage, trace, algorithm, control, title, 
        description, ...)
    ans@call = CALL
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.garchFit =
function(formula.mean = ~arma(0, 0), formula.var = ~garch(1, 1), 
series = x, init.rec = c("mci", "uev"), delta = 2, skew = 1, shape = 4,
cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
include.mean = TRUE, include.delta = NULL, include.skew = NULL,
include.shape = NULL, leverage = NULL, trace = TRUE,  
algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
control = list(), title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description
    #   Fit parameters to a ARMA-GARCH model
    
    # Arguments:
    #   formula.mean - ARMA(m,n) mean specification
    #   formula.var - GARCH/APARCH(p,q) variance specification
    #   series - time series 
    #       by default the data are taken from the series x
    #           if x is a data.frame then the first column is selected
    #       if series is a character string then the data are
    #           retrieved from data(series)
    #   init.rec - names type of initialization of recurrence
    #       mci = mu-current-iteration, or
    #       uev = unconditional-expected-variances
    #   delta - numeric value of the exponent delta
    #   skew - optional skewness parameter
    #   shape - optional shape parameter 
    #   cond.dist - name of the conditional distribution 
    #   include.mean - should the mean value be estimated ?
    #   include.delta - should the exponent be estimated ?
    #   leverage - should the leverage factors be estimated ?
    #   trace - should the optimization be traced ?
    #   control - list of additional control parameters for solver
    #   title - an optional title string
    #   description - an optional project description string
    
    # Example:
    #   # GARCH(1,1): 
    #   > data(dem2gbp); x = dem2gbp[,1, fit = garchFit(); fit
    #   > data(dem2gbp); fit = garchFit(series = dem2gbp[,1]); fit
    #   > fit = garchFit(series = "dem2gbp"); fit
        
    # FUNCTION:
  
    # Check for Recursion Initialization:
    if (init.rec[1] != "mci" & algorithm[1] != "sqp") {
        stop("Algorithm only supported for mci Recursion")
    }
    
    # Series:
    if (is.character(series)) {
        eval(parse(text = paste("data(", series, ")")))
        series = eval(parse(text = series))
    }
    if (is.data.frame(series)) {
        series = series[, 1]
    }
    series = as.vector(series)
    
    # Start Time:
    Start <<- Sys.time()
    
    # Generate Control List - Define Default Settings:
    con <- list(
        # In General:
        fscale = FALSE,
        xscale = FALSE,
        algorithm = algorithm,
        llh = c("filter", "internal", "testing")[1],
        # BFGS - NLMINB Algorithm:
        tol1 = 1, 
        tol2 = 1, 
        # SQP Algorithm:
        MIT = 200,     # maximum number of iterations (200)
        MFV = 500,     # maximum number of function evaluations (500)
        MET = 2,       # specifies scaling strategy:
                       #  MET=1 - no scaling 
                       #  MET=2 - preliminary scaling in 1st iteration (default)
                       #  MET=3 - controlled scaling 
                       #  MET=4 - interval scaling 
                       #  MET=5 - permanent scaling in all iterations 
        MEC = 2,       # correction for negative curvature:
                       #  MEC=1 - no correction
                       #  MEC=2 - Powell correction (default)
        MER = 1,       # restarts after unsuccessful variable metric updates:
                       #  MER=0 - no restarts
                       #  MER=1 - standard restart 
        MES = 4,       # interpolation method selection in a line search:
                       #  MES=1 - bisection
                       #  MES=2 - two point quadratic interpolation
                       #  MES=3 - three point quadratic interpolation
                       #  MES=4 - three point cubic interpolation (default)
        XMAX = 1.0e3, 
        TOLX = 1.0e-16, 
        TOLC = 1.0e-6, 
        TOLG = 1.0e-6, 
        TOLD = 1.0e-6, 
        TOLS = 1.0e-4, 
        RPF  = 1.0e-4)  
    con[(namc <- names(control))] <- control
       
    # Trace Information - Save the Flag globally:
    .trace <<- trace  
    
    # Initialize Time Series Information - Save Globally:            
    .series <<- .garchInitSeries(formula.mean = formula.mean, 
        formula.var = formula.var, series = series, scale = sd(series),
        init.rec = init.rec[1], h.start = NULL, llh.start = NULL)
        
    # Initialize Model Parameters - Save Globally:
    .params <<- .garchInitParameters(formula.mean = formula.mean, 
        formula.var = formula.var, delta = delta, skew = skew, 
        shape = shape, cond.dist = cond.dist[1], 
        include.mean = include.mean, include.delta = include.delta, 
        include.skew = include.skew, include.shape = include.shape, 
        leverage = leverage, algorithm = algorithm[1], control = con)

    # Select Conditional Distribution Function:
    .garchDist <<- .garchSetCondDist(cond.dist[1]) 
    
    # Estimate Model Parameters - Minimize llh, start from big value: 
    .llh <<- 1.0e99   
    fit = .garchOptimizeLLH(...) 
    fit$llh = .llh
     
    # Add to Fit: 
    names(.series$h) <- NULL
    fit$series = .series
    fit$params = .params
        
    # Retrieve Residuals and Fitted Values: 
    residuals = .series$z 
    fitted.values = .series$x - residuals
    h.t = .series$h
    deltainv = 1/fit$params$delta
    sigma.t = (.series$h)^deltainv
    
    # Standard Errors and t-Values:
    fit$cvar = solve(fit$hessian)
    fit$se.coef = sqrt(diag(fit$cvar))
    fit$tval = fit$coef/fit$se.coef
    fit$matcoef = cbind(fit$coef, fit$se.coef, 
        fit$tval, 2*(1-pnorm(abs(fit$tval))))
    dimnames(fit$matcoef) = list(names(fit$tval), c(" Estimate", 
        " Std. Error", " t value", "Pr(>|t|)"))
    
    # Add Title and Description:
    if (is.null(title)) title = "GARCH Modelling"
    if (is.null(description)) description = .description()
        
    # Return Value:
    new("fGARCH",     
        call = as.call(match.call()),
        formula = list(formula.mean = formula.mean, formula.var = formula.var),
        method = "Max Log-Likelihood Estimation", 
        data = list(x = series),
        fit = fit,        
        residuals = residuals,
        fitted = fitted.values,
        h.t = h.t,
        sigma.t = as.vector(sigma.t),
        title = as.character(title),
        description = as.character(description) 
    )
}


# ------------------------------------------------------------------------------


.garchInitSeries = 
function(formula.mean, formula.var, series, scale, init.rec, 
h.start, llh.start)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Initialize time series
    
    # Arguments:
    #   see function garchFit()
    
    # Note:
    #   The variable '.trace' must be declared globally.
    
    # FUNCTION:
    
    # Check Mean Formula ARMA - Is it Valid ?
    mm = length(formula.mean)
    if (mm != 2) stop("Mean Formula misspecified") 
    end = regexpr("\\(", as.character(formula.mean[mm])) - 1
    model.mean = substr(as.character(formula.mean[mm]), 1, end)
    if (!any( c("ar", "ma", "arma") == model.mean))
        stop("formula.mean must be one of: ar, ma, arma")
    
    # Check Variance Formula GARCH - Is it Valid ?
    mv = length(formula.var)
    if (mv != 2) top("Variance Formula misspecified")
    end = regexpr("\\(", as.character(formula.var[mv])) - 1
    model.var = substr(as.character(formula.var[mv]), 1, end)
    if (!any( c("garch", "aparch") == model.var))
        stop("formula.var must be one of: garch, aparch") 
    
    # Determine Mean Order from ARMA Formula:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.mean), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    u = model.order[1]
    v = 0
    if (length(model.order) == 2) v = model.order[2]
    maxuv = max(u, v)
    if (u < 0 | v < 0) stop("*** ARMA orders must be positive.")
    
    # Determine Variance Order from GARCH Formula:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.var), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    p = model.order[1]
    q = 0
    if (length(model.order) == 2) q = model.order[2]
    if (p+q == 0)
        stop("Misspecified GARCH Model: Both Orders are zero!")
    maxpq = max(p, q)
    if (p < 0 | q < 0) stop("*** GARCH orders must be positive.")
    
    # Fix Start Position of Series "h" and for Likelihood Calculation:
    max.order = max(maxuv, maxpq)  
    if (is.null(h.start)) h.start = max.order + 1
    if (is.null(llh.start)) llh.start = 1
    
    # Check for Recursion Initialization:
    if (init.rec != "mci" & model.var != "garch") {
        stop("GARCH model only supported for mci Recutrsion")
    }
    
    # Trace the Result:
    if (.trace) {
        cat("\nSeries Initialization:")
        cat("\n ARMA model:               ", model.mean)
        cat("\n Formula mean:             ", as.character(formula.mean))
        cat("\n GARCH model:              ", model.var)
        cat("\n Formula var:              ", as.character(formula.var))
        cat("\n ARMA Order:               ", u, v)
        cat("\n Max ARMA Order:           ", maxuv)
        cat("\n GARCH Order:              ", p, q)
        cat("\n Max GARCH Order:          ", maxpq)
        cat("\n Maximum Order:            ", max.order)
        cat("\n h.start:                  ", h.start)
        cat("\n llh.start:                ", llh.start)
        cat("\n Length of Series:         ", length(series))
        cat("\n Recursion Init:           ", init.rec)
        cat("\n Series Scale:             ", scale)
        cat("\n\n")
    }

    # Result:
    ans  = list(
        model = c(model.mean, model.var), 
        order = c(u = u, v = v, p = p, q = q), 
        max.order = max.order,
        z = rep(0, times = length(series)), 
        h = rep(var(series), times = length(series)), 
        x = series, 
        scale = scale,
        init.rec = init.rec, 
        h.start = h.start, 
        llh.start = llh.start)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------
      

.garchInitParameters = 
function(formula.mean, formula.var, delta, skew, shape, cond.dist, 
include.mean, include.delta, include.skew, include.shape, leverage, 
algorithm, control = con)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Initialize model parameters
    
    # Arguments:
    #   see function garchFit()
    
    # Note:
    #   The variable '.trace' must be declared globally.
    
    # FUNCTION:
    
    # DEBUG:
    .DEBUG = FALSE
    
    # Determine Mean Order from ARMA Formula:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.mean), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    u = model.order[1]
    v = 0
    if (length(model.order) == 2) v = model.order[2]
    
    # Determine Variance Order from GARCH Formula:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.var), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    p = model.order[1]
    q = 0
    if (length(model.order) == 2) q = model.order[2]
    
    # Includes:
    model.var = .series$model[2]
    if (is.null(include.delta)) {
        if (model.var == "garch") {
            include.delta = FALSE 
        } else {
            include.delta = TRUE
        }
    }
    if (is.null(leverage)) {
        if (model.var == "garch") {
            leverage = FALSE 
        } else {
            leverage = TRUE
        }
    }
    
    # Distributional Includes:
    if (cond.dist == "t") cond.dist = "dstd"
    skewed.dists = c("dsnorm", "dsged", "dsstd")
    if (is.null(include.skew)) {
        if (any(skewed.dists == cond.dist)) {
            include.skew = TRUE
        } else {
            include.skew = FALSE
        }
    }
    shaped.dists = c("dged", "dsged", "dstd", "dsstd")
    if (is.null(include.shape)) {
        if (any(shaped.dists == cond.dist)) {
            include.shape = TRUE
        } else {
            include.shape = FALSE
        }
    }
     
    # Set Names for Parameters:
    Names = c(
        "mu", 
        if (u > 0) paste("ar", 1:u, sep = ""),
        if (v > 0) paste("ma", 1:v, sep = ""),   
        "omega",
        if (p > 0) paste("alpha", 1:p, sep = ""),
        if (p > 0) paste("gamma", 1:p, sep = ""),
        if (q > 0) paste("beta",  1:q, sep = ""),
        "delta",
        "skew", 
        "shape")
    if (.DEBUG) { cat("\nDEBUG - Names: \n"); print(Names) }    
    
    # Initialize Model Parameters to be Estimated:
    fit.mean = arima(.series$x, order = c(u, 0, v), 
        include.mean = include.mean)$coef
    alpha.start = 0.1
    beta.start = 0.8  
    # if (include.delta) delta = 1.5      
    params = c(
        if (include.mean) fit.mean[length(fit.mean)] else 0, 
        if (u > 0) fit.mean[1:u], 
        if (v > 0) fit.mean[(u+1):(length(fit.mean)-as.integer(include.mean))],
        var(.series$x, na.rm = TRUE)*(1-alpha.start-beta.start),
        if (p > 0) rep(alpha.start/p, times = p),
        if (p > 0) rep(0.1, times = p), 
        if (q > 0) rep(beta.start/q, times = q),
        delta,
        skew,
        shape)
    names(params) = Names
    if (.DEBUG) { cat("\nDEBUG - params: \n"); print(params) }   
    
    # Set Lower Limits of Parameters to be Estimated: 
    TINY = 1.0e-8
    U = c(
        -10*abs(mean(.series$x)), 
        if (u > 0) rep(-1+TINY, times = u),
        if (v > 0) rep(-1+TINY, times = v),
        1.0e-6*var(.series$x), 
        if (p > 0) rep( 0+TINY, times = p),
        if (p > 0) rep(-1+TINY, times = p),
        if (q > 0) rep( 0+TINY, times = q),
        0,
        1/10,
        1)     
    names(U) = Names
    if (.DEBUG) { cat("\nDEBUG - U: \n"); print(U) }
    
    # Set Upper Limits of Parameters to be Estimated:    
    V = c(
        10*abs(mean(.series$x)),  
        if (u > 0) rep(1-TINY, times = u),
        if (v > 0) rep(1-TINY, times = v),
        100*var(.series$x), 
        if (p > 0) rep(1-TINY, times = p),
        if (p > 0) rep(1-TINY, times = p),
        if (q > 0) rep(1-TINY, times = q),
        2,
        10,
        20)     
    names(V) = Names
    if (.DEBUG) { cat("\nDEBUG - V: \n"); print(V) }
    
    # Includes:
    includes = c(
        include.mean,
        if (u > 0) rep(TRUE, times = u),
        if (v > 0) rep(TRUE, times = v),
        TRUE, 
        if (p > 0) rep(TRUE, times = p),
        if (p > 0) rep(leverage, times = p),
        if (q > 0) rep(TRUE, times = q),
        include.delta,
        include.skew,
        include.shape)
    names(includes) = Names
    if (.DEBUG) { cat("\nDEBUG - V: \n"); print(includes) }   
     
    # Index List of Parameters to be Optimized:
    index = (1:length(params))[includes == TRUE]
    names(index) = names(params)[includes == TRUE]
    if (.DEBUG) { cat("\nDEBUG - fixed: \n"); print(index) }
    
    # Persistence:  
    if (p > 0) alpha = params[substr(Names, 1, 5) == "alpha"] 
    if (p > 0 & leverage) gamma = params[substr(Names, 1, 5) == "gamma"]
    if (p > 0 & !leverage) gamma = rep(0, times = p)
    if (q > 0) beta  = params[substr(Names, 1, 4) == "beta"] 
    if (.series$model[2] == "garch") {
        persistence = sum(alpha) + sum(beta)
    } else if (.series$model[2] == "aparch") {
        persistence = sum(beta)
        for (i in 1:p)
            persistence = persistence + alpha[i]*garchKappa(cond.dist,
                gamma[i], params["delta"], params["skew"], params["shape"])
    }
    names(persistence) = "persistence"
      
    # Trace the Result:
    if (.trace) {
        cat("Parameter Initialization:")
        cat("\n Initial Parameters:          $params")    
        cat("\n Limits of Transformations:   $U, $V")
        cat("\n Which Parameters are Fixed?  $includes")
        cat("\n Parameter Matrix:\n")
        ans = data.frame(U, V, params, includes)
        rownames(ans) = paste("   ", names(params))
        print(ans)
        cat(" Index List of Parameters to be Optimized:\n")
        print(index)
        cat(" Persistence:                 ", persistence, "\n")
    }

    # Return Value:
    list(params = params, U = U, V = V, includes = includes, 
        index = index, mu = params[1], delta = delta, skew = skew, 
        shape = shape, cond.dist = cond.dist, leverage = leverage, 
        persistence = persistence, control = control)
}
    

# ------------------------------------------------------------------------------

      
.garchSetCondDist =
function(cond.dist = "dnorm") 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Select Conditional Density Function
    
    # Arguments:
    #   cond.dist - a character string with the name of the 
    #       conditional distribution function. Valid strings are:
    #       "dnorm", "dsnorm", "dstd", "dsstd", "dged", "dsged".
    
    # Value:
    #   Returns the selection conditional distribution function
    #   named uniquely '.garchDist'.
    
    # Note:
    #   The variable '.trace' must be declared globally.
    
    # Details:
    #   Implemented Distributions: 
    #    dnorm - Normal Distribution: nothing to estimate
    #    dsnorm - Skew Normal Distribution: xi may be estimated 
    #    dstd - Student-t Distribution: nu may be estimated
    #    dsstd - Skew Student-t Distribution: nu and xi may be estimated
    #    dged - Generalized Error Distribution: nu may be estimated
    #    dsged - Skew Generalized Error Distribution: nu and xi may be estimated
    
    # FUNCTION:
    
    # Normal Distribution:
    if (cond.dist == "dnorm") {
         .garchDist = function(z, hh, skew, shape) {
            dnorm(x = z/hh, mean = 0, sd = 1) / hh 
        }
    }
    if (cond.dist == "dsnorm") { 
        .garchDist = function(z, hh, skew, shape) {
            dsnorm(x = z/hh, mean = 0, sd = 1, xi = skew) / hh 
        }
    }
    
    # Standardized Student-t:
    if (cond.dist == "dstd") { 
        .garchDist = function(z, hh, skew, shape) {
            dstd(x = z/hh, mean = 0, sd = 1, nu = shape) / hh
        }
    }
    if (cond.dist == "dsstd") { 
        .garchDist = function(z, hh, skew, shape) {
            dsstd(x = z/hh, mean = 0, sd = 1, nu = shape, xi = skew) / hh
        }
    }
      
    # Generalized Error Distribution:
    if (cond.dist == "dged") {
        .garchDist = function(z, hh, skew, shape) {
            dged(x = z/hh, mean = 0, sd = 1, nu = shape) / hh
        }
    }
    if (cond.dist == "dsged") {
        .garchDist = function(z, hh, skew, shape) {
            dsged(x = z/hh, mean = 0, sd = 1, nu = shape, xi = skew) / hh
        }
    }
                       
    # Trace the Result:
    if (FALSE) {
        cat("\n Distribution:     ", cond.dist, "\n    .garchDist = ")
        print(.garchDist)
    }
      
    # Return Value:
    .garchDist 
}


# ------------------------------------------------------------------------------
 

.garchLLH =
function(params) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute Log-Likelihood Function
    
    # Arguments:
    #   params - a named numeric vector with the model parameters
    #       to be optimized
    
    # Value:
    #   Returns the value of the max log-likelihood function.
    
    # Note:
    #   The variables '.series' and '.params' must be global available
    
    # FUNCTION:
    
    # DEBUG:
    .DEBUG = FALSE
    
    # Retrieve From Initialized Series:
    x = .series$x
    
    # Get Order:
    u = .series$order[1]
    v = .series$order[2]
    p = .series$order[3]
    q = .series$order[4]
    max.order = max(u, v, p, q)
    
    # Get Start Conditions:
    h.start = .series$h.start
    llh.start = .series$llh.start  
    
    # Get the Index Values and Add Names - Just to be Sure:
    index = .params$index 
    names(params) = names(.params$params[index])     
    Names = names(params)
    
    # Retrieve From Initialized Parameters:
    cond.dist = .params$cond.dist
    
    # Extracting the parameters by name ...
    mu = c(mu = .params$mu)
    delta = c(delta = .params$delta)
    skew = c(skew = .params$skew)
    shape = c(shape = .params$shape)
    leverage = c(leverage = .params$leverage)
    if (.params$includes["mu"]) mu = params["mu"]
    if (u > 0) ar = params[substr(Names, 1, 2) == "ar"]
    if (v > 0) ma = params[substr(Names, 1, 2) == "ma"]
    omega = params[substr(Names, 1, 5) == "omega"]
    if (p > 0) alpha = params[substr(Names, 1, 5) == "alpha"] 
    if (p > 0 & leverage) gamma = params[substr(Names, 1, 5) == "gamma"]
    if (q > 0) beta  = params[substr(Names, 1, 4) == "beta"] 
    if (.params$includes["delta"]) delta = params["delta"] 
    if (.params$includes["skew"])  skew  = params["skew"] 
    if (.params$includes["shape"]) shape = params["shape"] 
    
    # Iterate z: 
    N = length(x)  
    z = rep(0, N)
    if (u > 0 & v > 0) 
        for (i in (h.start):N) 
            z[i] = x[i] - mu - sum(ar*x[i-(1:u)]) - sum(ma*z[i-(1:v)])
    if (u > 0 & v == 0) 
        for (i in (h.start):N) 
            z[i] = x[i] - mu - sum(ar*x[i-(1:u)])      
    if (u == 0 & v > 0) 
        for (i in (h.start):N) 
            z[i] = x[i] - mu - sum(ma*z[i-(1:v)])                 
    if (u == 0 & v == 0)  
        z = x - mu                
    
    # Initialize Variance Equation:  
    deltainv = 1/delta
    if (.series$model[2] == "garch") {
        persistence = sum(alpha) + sum(beta)
    } else if (.series$model[2] == "aparch") {
        persistence = sum(beta)
        for (i in 1:p)
            persistence = persistence + alpha[i]*garchKappa(cond.dist,
                gamma[i], delta, skew, shape)
    }
    names(persistence) = "persistence"
    attr(persistence, "control") = NULL
    attr(persistence, "cond.dist") = NULL
    .params$persistence <<- persistence
    mvar = mean(z^2)
    h = rep(omega + persistence*mvar, N)
      
    # Iterate Conditional Variances h: 
    if (p == 0) { 
        alpha = 0 
        p = 1
    }
    if (q == 0) {
        beta = 0
        q = 1
    }
   
    # How to compute the LLH recursion?
    USE = .params$control$llh
       
    # Test Version Just a Simple Double 'for' Loop:
    if (USE == "testing") {
        # As You Can Imagine, Slow Version But Very Useful for Testing:
        if (!.params$leverage) {
            for (i in (h.start):N) {
                h[i] = omega + 
                    sum(alpha * ( abs(z[i-(1:p)])) ^ delta ) + 
                    sum(beta*h[i-(1:q)])  
            }
        } else {    
            for (i in (h.start):N) {
                h[i] = omega + 
                    sum(alpha * ( abs(z[i-(1:p)]) - 
                    gamma * eps[i-(1:p)]) ^ delta ) + sum(beta*h[i-(1:q)]) 
            }
        } 
    }
 
    # R Filter Representation:
    # Entirely written in S, and very effective ...
    if (USE == "filter") {
        # Note, sometimes one of the beta's can become undefined 
        # during optimization.
        if (!.params$leverage) gamma = rep(0, p)
        pq = max(p, q)
        edeltat = 0
        for (j in 1:p) {
            Filter = rep(0, length = p+1)
            Filter[j+1] = alpha[j]
            edelta = (abs(z) - gamma[j]*z)^delta
            edelta = filter(edelta, filter = Filter, sides = 1)
            edeltat = edeltat + edelta       
        }
        c.init = omega/(1-sum(beta))
        h = c( h[1:pq], c.init + filter(edeltat[-(1:pq)], filter = beta, 
             method = "recursive", init = h[q:1]-c.init))
        ### ? remove ?
        if ( sum(is.na(h)) > 0 ) {
            # We use the testing Version ...
            warning("Problems in Filter Representation")
            if (!.params$leverage) {
                for (i in (h.start):N) {
                    h[i] = omega + 
                        sum(alpha * ( abs(z[i-(1:p)])) ^ delta ) + 
                        sum(beta*h[i-(1:q)])  
                }
            } else {    
                for (i in (h.start):N) {
                    h[i] = omega + 
                        sum(alpha * ( abs(z[i-(1:p)]) - 
                        gamma * eps[i-(1:p)]) ^ delta ) + sum(beta*h[i-(1:q)]) 
                }
            } 
        }
    }
    
    # Fortran Implementation:
    # May be Even Faster Compared with R's Filter Representation ...
    if (USE == "internal") {
        if (!.params$leverage) gamma = rep(0, p)
        # For asymmetric APARCH Models Only !!! 
        h = .Fortran("aparchllh", as.double(z), as.double(h), as.integer(N),
            as.double(omega), as.double(alpha), as.double(gamma), 
            as.integer(p), as.double(beta), as.integer(q), as.double(delta), 
            as.integer(h.start), PACKAGE = "fSeries")[[2]]  
    }
    
    # Save h and eps:
    .series$h <<- h
    .series$z <<- z
    
    # Calculate Log Likelihood:    
    hh = (abs(h[(llh.start):N]))^deltainv
    zz = z[(llh.start):N]
    llh = -sum(log(.garchDist(z = zz, hh = hh, skew = skew, shape = shape)))
    if (.DEBUG) cat("DEBUG - LLH:   ", llh, "\n")
    names(params) = names(.params$params[.params$index])
    if (is.na(llh)) llh = .llh + 0.1*(abs(.llh))  
    if (!is.finite(llh)) llh = .llh + 0.1*(abs(.llh))
    
    # Print if LLH has Improved:
    if (llh <.llh) {
        .llh <<- llh
        if (.trace) {   
            cat(" LLH: ", llh, "   norm LLH: ", llh/N, "\n")
            print(params)
            if (persistence > 1) 
                cat("Warning - Persistence:", persistence, "\n")
        }
    }
    
    # Return Value:
    c(LogLikelihood = llh) 
}


# ------------------------------------------------------------------------------


.garchHessian =
function(par)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the Hessian Matrix
    
    # Details:
    #   This function computes the Hessian dependent on the 
    #   implementation. For the pure S implementations "nlminb"
    #   and "lbfgsb" the Hessian is also computed from a pure S
    #   function. In the case of the Fortran version we also 
    #   compute the Hessian from a much more effective Fortran
    #   implementation.

    # FUNCTION:
 
    # Compute Hessian:
    algorithm = .params$control$algorithm[1]
    EPS0 = 1.0e-4
    if (algorithm == "nlminb" | algorithm == "lbfgsb" | 
        algorithm == "nlminb+nm" | algorithm == "lbfgsb+nm") {
        # CASE I: NLMINB and BFGS
        keep.trace = .trace
        keep.control = .params$control
        eps = EPS0 * par
        n = length(par)
        H = matrix(0, ncol = n, nrow = n)
        .trace <<- FALSE
        for (i in 1:n) {
            for (j in 1:n) {
                x1 = x2 = x3 = x4 = par
                x1[i] = x1[i] + eps[i]
                x1[j] = x1[j] + eps[j]
                x2[i] = x2[i] + eps[i]
                x2[j] = x2[j] - eps[j]
                x3[i] = x3[i] - eps[i]
                x3[j] = x3[j] + eps[j]
                x4[i] = x4[i] - eps[i]
                x4[j] = x4[j] - eps[j]
                H[i, j] = ( 
                    .garchLLH(x1) - 
                    .garchLLH(x2) -
                    .garchLLH(x3) + 
                    .garchLLH(x4) ) / (4*eps[i]*eps[j])
            }
        }
        .trace <<- keep.trace  
        .params$control <<- keep.control 
    } else {
        # Case II: SQP
        N = length(.series$x)
        NF = length(par)
        if (.params$includes["delta"]) {
            XDELTA = par["delta"] 
        } else {
            XDELTA = .params$delta
        } 
        if (.params$includes["skew"]) {
            XSKEW = par["skew"] 
        } else {
            XSKEW = .params$skew
        }   
        if (.params$includes["shape"]) {
            XSHAPE = par["shape"] 
        } else {
            XSHAPE = .params$shape
        } 
        DPARM = c(XDELTA, XSKEW, XSHAPE)    
        MDIST = c(dnorm = 10, dsnorm = 11, dstd = 20, dsstd = 21, dged = 30, 
            dsged = 31)[.params$cond.dist]                # Which Distribution
        REC = 1
        if (.series$init.rec == "uev") REC = 2
        MYPAR = c(
            REC   = REC,                                  # How to initialize
            LEV   = as.integer(.params$leverage),         # Include Leverage 0|1 
            MEAN  = as.integer(.params$includes["mu"]),   # Include Mean 0|1 
            DELTA = as.integer(.params$includes["delta"]),# Include Delta 0|1                          
            SKEW  = as.integer(.params$includes["skew"]), # Include Skew 0|1 
            SHAPE = as.integer(.params$includes["shape"]),# Include Shape 0|1 
            ORDER = .series$order)                        # Order of ARMA-GARCH
        # Compute Hessian:
        ans = .Fortran("garchhess",
            N = as.integer(N), 
            Y = as.double(.series$x), 
            Z = as.double(rep(0, times = N)), 
            H = as.double(rep(0, times = N)), 
            NF = as.integer(NF), 
            X = as.double(par), 
            DPARM = as.double(DPARM), 
            MDIST = as.integer(MDIST), 
            MYPAR = as.integer(MYPAR), 
            E0 = as.double(EPS0),
            HESS = as.double(rep(0, times = NF*NF)),
            PACKAGE = "fSeries")
        # The Matrix:
        H = matrix(ans[["HESS"]], ncol = NF)  
        colnames(H) = rownames(H) = names(par)
    }
    
    # Return Value:
    H   
}


# ------------------------------------------------------------------------------


.garchOptimizeLLH =
function(...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Opimize Log-Likelihood Function
    
    # Arguments:
    #   none
    
    # FUNCTION:
       
    # DEBUG:
    .DEBUG = FALSE
    
    # Initialization:
    INDEX = .params$index
      
    # Algorithm:
    algorithm = .params$control$algorithm[1]
    TOL1 = .params$control$tol1
    TOL2 = .params$control$tol2
    
    # Optimize:
    if (.trace) cat("\nIteration Path:\n") 
    
    # First Method:
    # Two Step Apparoach - Trust Region + Nelder-Mead Simplex
    if (algorithm == "nlminb" | algorithm == "nlminb+nm") {
        if (.trace) cat("\n\n\nNow NLMINB \n\n\n")
        parscale = rep(1, length = length(INDEX))
        names(parscale) = names(.params$params[INDEX])
        parscale["omega"] = var(.series$x)^(.params$delta/2)
        fit = nlminb(
            start = .params$params[INDEX],
            objective = .garchLLH, 
            lower = .params$U[INDEX],
            upper = .params$V[INDEX],
            scale = parscale,
            control = list(eval.max = 2000, iter.max = 1500, 
                rel.tol = 1e-14*TOL1, x.tol = 1e-14*TOL1)
            )  
        fit$value = fit$objective 
        if (algorithm == "nlminb+nm") {          
            if (.trace) cat("\n\n\nNow Nelder-Mead \n\n\n")
            fnscale = abs(.garchLLH(.params$params[INDEX]))
            fit = optim(
                par = fit$par,
                fn = .garchLLH,
                method = "Nelder-Mead",
                control = list(
                    ndeps = rep(1e-14*TOL2, length = length(INDEX)), 
                    maxit = 10000, 
                    reltol = 1e-14*TOL2, 
                    fnscale = fnscale, 
                    parscale = c(1, abs(fit$par[-1]))),
                hessian = TRUE)
        }
    }
    
    # Second Method:
    # Two Step Approach - BFGS + Nelder-Mead Simplex
    if (algorithm == "lbfgsb" | algorithm == "lbfgsb+nm") {
        if (.trace) cat("\n\n\nNow L-BFGS-B \n\n\n")
        parscale = rep(1, length = length(INDEX))
        names(parscale) = names(.params$params[INDEX])
        parscale["omega"] = var(.series$x)^((.params$params["delta"])/2)
        fit = optim(
            par = .params$params[INDEX], 
            fn = .garchLLH, 
            lower = .params$U[INDEX], 
            upper = .params$V[INDEX], 
            method = "L-BFGS-B", 
            control = list(
                parscale = parscale, 
                lmm = 20, 
                pgtol = 1e-14 * TOL1, 
                factr = 1 * TOL1)
        )        
        if (algorithm == "lbfgsb+nm") {
            if (.trace) cat("\n\n\nNow Nelder-Mead \n\n\n")
            fnscale = abs(fit$value)
            parscale = abs(fit$par)
            fit = optim(
                par = fit$par, 
                fn = .garchLLH, 
                method = "Nelder-Mead", 
                control = list(
                    ndeps = rep(1e-14 * TOL2, length = length(INDEX)), 
                    maxit = 10000, 
                    reltol = 1e-14 * TOL2, 
                    fnscale = fnscale, 
                    parscale = parscale), 
                hessian = TRUE)
        }
    }
    
    # Third Method:
    # Sequential Programming Algorithm
    # IPAR, RPAR and MYPAR Parameter Setting:
    if (algorithm == "sqp") {
        if (.trace) cat(" SQP Algorithm\n\n")
        IPAR = c(
            IPRNT = as.integer(.trace),    #  [1, 200, 500, 2, 2, 1, 4]
            MIT = .params$control$MIT,    
                        # maximum number of iterations (200)
            MFV = .params$control$MFV,    
                        # maximum number of function evaluations (500)
            MET = .params$control$MET,      
                        # specifies scaling strategy:
                        #  MET=1 - no scaling 
                        #  MET=2 - preliminary scaling in 1st iteration (default)
                        #  MET=3 - controlled scaling 
                        #  MET=4 - interval scaling 
                        #  MET=5 - permanent scaling in all iterations 
            MEC = .params$control$MEC,    
                        # correction for negative curvature:
                        #  MEC=1 - no correction
                        #  MEC=2 - Powell correction (default)
            MER = .params$control$MER,    
                        # restarts after unsuccessful variable metric updates:
                        #  MER=0 - no restarts
                        #  MER=1 - standard restart 
            MES = .params$control$MES) 
                        # interpolation method selection in a line search:
                        #  MES=1 - bisection
                        #  MES=2 - two point quadratic interpolation
                        #  MES=3 - three point quadratic interpolation
                        #  MES=4 - three point cubic interpolation (default)            
        RPAR = c(
            XMAX = .params$control$XMAX,  
            TOLX = .params$control$TOLX, 
            TOLC = .params$control$TOLC,
            TOLG = .params$control$TOLG, 
            TOLD = .params$control$TOLD,
            TOLS = .params$control$TOLS,
            RPF  = .params$control$RPF)
        MDIST = c(dnorm = 10, dsnorm = 11, dstd = 20, dsstd = 21, dged = 30, 
            dsged = 31)[.params$cond.dist]
        if (.params$control$fscale) NORM = length(.series$x) else NORM = 1
        REC = 1
        if (.series$init.rec == "uev") REC = 2
        MYPAR = c(
            REC   = REC,                                  # How to initialize
            LEV   = as.integer(.params$leverage),         # Include Leverage 0|1 
            MEAN  = as.integer(.params$includes["mu"]),   # Include Mean 0|1 
            DELTA = as.integer(.params$includes["delta"]),# Include Delta 0|1                          
            SKEW  = as.integer(.params$includes["skew"]), # Include Skew 0|1 
            SHAPE = as.integer(.params$includes["shape"]),# Include Shape 0|1 
            ORDER = .series$order,                        # Order of ARMA-GARCH
            NORM  = as.integer(NORM))
        
        # Now Estimate Parameters:     
        MAX = max(.series$order)
        NF = length(INDEX)
        N = length(.series$x)
        DPARM = c(.params$delta, .params$skew, .params$shape)
        if (IPAR[1] == 0) sink("@sink@")
        ans = .Fortran("garchfit",
            N = as.integer(N), 
            Y = as.double(.series$x), 
            Z = as.double(rep(2, times = N)), 
            H = as.double(rep(0, times = N)), 
            NF = as.integer(NF), 
            X = as.double(.params$params[INDEX]), 
            XL = as.double(.params$U[INDEX]), 
            XU = as.double(.params$V[INDEX]), 
            DPARM = as.double(DPARM), 
            MDIST = as.integer(MDIST), 
            IPAR = as.integer(IPAR), 
            RPAR = as.double(RPAR), 
            MYPAR = as.integer(MYPAR),
            F = as.double(F),
            PACKAGE = "fSeries")
        if (IPAR[1] == 0) {
            sink()        
            unlink("@sink@")
        }
     
        # Result:
        if (.trace) {
            cat("\nControl Parameter:\n")
            print(IPAR)
            print(RPAR)
        }
        fit = list()
        fit$par = ans[[6]]
        fit$value = ans[[14]] 
        
        # Update .series
        names(fit$par) = names(.params$params[INDEX]) 
        updateLLH = .garchLLH(fit$par)
    } 
    
    # Add Names:
    names(fit$par) = names(.params$params[INDEX]) 
    fit$coef = fit$par
    
    # Execution Time:
    Time =  Sys.time() - Start
    if (.trace) {
        cat("\nTime to Estimate Parameters:\n ")
        print(Time) 
    }
    
    # Compute Hessian:
    Start = Sys.time()
    H = .garchHessian(fit$par)
    Time =  Sys.time() - Start
    if (.trace) {
        cat("\nTime to Compute Hessian:\n ")
        print(Time)  
    }  
    
    # Information Criterion Statistics:
    N = length(.series$x)
    NPAR = length(fit$par)
    fit$ics = c(
        AIC  = (-2*fit$value)/N + 2 * NPAR/N,
        BIC  = (-2*fit$value)/N + NPAR * log(N)/N,
        SIC  = (-2*fit$value)/N + log((N+2*NPAR)/N),
        HQIC = (-2*fit$value)/N + (2*NPAR*log(log(N)))/N )
        
    # Final Function Evaluation: 
    if (.trace) {
        # Note, that .garchLLH() will print the final estimate ...
        .llh <<- 1.0e99
        cat("\nFinal Estimate:\n")
        .llh <<- .garchLLH(fit$par)
    }  
    
    # Hessian:
    colnames(H) = rownames(H) = names(.params$params[INDEX])
    fit$hessian = H
    
    # Print Hessian Matrix:
    if (.trace) {
        cat("\nHessian Matrix:\n")
        print(fit$hessian)
        cat("\n--- END OF TRACE ---\n\n") 
    }
    
    # Alternative Variable of Coefficients:
    fit$coef = fit$par
 
    # Return Value:
    fit 
}


# ------------------------------------------------------------------------------


.garchNames =
function(object)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print slot names, @fit slot, parameters and controls
    
    # Arguments:
    #   object - an object of class 'fGARCH'
    
    # FUNCTION:
    
    # Slot Names:
    cat("\nNames - @ \n")
    print(slotNames(object))
    
    # @fit Slot:
    cat("\nNames - @fit \n")
    print(names(object@fit))
    
    # Parameters:
    cat("\nNames - @fit$params \n")
    print(names(object@fit$params))
    
    # Control:
    cat("\nNames - @fit$params$control \n")
    print(names(object@fit$params$control))

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.garchTsFit = 
function(formula.var = ~garch(1, 1), series = x, 
title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Wrapper for garch() function from R's 'tseries' package
    
    # Example:
    #   data(dem2gbp); ans = .garchTsFit(series = dem2gbp[,1])
    
    # Note:
    #   Requires Contributed R-package 'tseries'
    
    # FUNCTION:
    
    # Load Contributed 'tseries' Package:
    # require(tseries)
    
    # Check Variance Formula GARCH - Is it Valid ?
    mv = length(formula.var)
    if (mv != 2) top("Variance Formula misspecified")
    end = regexpr("\\(", as.character(formula.var[mv])) - 1
    model.var = substr(as.character(formula.var[mv]), 1, end)
    if (!any( c("garch", "aparch") == model.var))
        stop("formula.var must be one of: garch, aparch") 
    
    # Determine Variance Order from GARCH Formula:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.var), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    p = model.order[1]
    q = 0
    if (length(model.order) == 2) q = model.order[2]
    if (p+q == 0)
        stop("Misspecified GARCH Model: Both Orders are zero!")
    if (p < 0 | q < 0) stop("*** GARCH orders must be positive.")
    
    # Parameter Estimation:
    fit = garch(x = series, order = c(p, q))
    
    # Addons, that we can use the fGARCH print method:
    class(fit) = "list"
    fit$params$cond.dist = "dnorm"
    fit$par = fit$coef
    fit$se.coef = fit$asy.se.coef
    fit$tval = fit$coef/fit$se.coef
    fit$matcoef = cbind(fit$coef, fit$se.coef, fit$tval, 
        2*(1-pnorm(abs(fit$tval))))
    dimnames(fit$matcoef) = list(names(fit$tval), c(" Estimate", 
        " Std. Error", " t value", "Pr(>|t|)"))
    #  llh = -(fit$n.likeli + length(series)*log(2*pi)/2)
    
    # Add Title and Description:
    if (is.null(title)) title = "tseries: GARCH Modelling"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fGARCH",     
        call = as.call(match.call()),
        formula = list(formula.mean = ~arma(0, 0), formula.var = formula.var),
        method = "tseries: Max Log-Likelihood Estimation", 
        data = list(x = series),
        fit = fit,        
        residuals = fit$residuals,
        fitted = series - fit$residuals,
        h = numeric(),
        sigma.t = numeric(),
        title = as.character(title),
        description = as.character(description) 
    )
}


################################################################################
                

print.fGARCH = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print method for an object of class "fGARCH"
    
    # Arguments:
    #   object - an object of class 'fGARCH'
    
    # FUNCTION:
    
    # object:
    object = x
     
    # Title:
    cat("\nTitle:\n ")
    cat(object@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), "\n")
    
    # Mean Equation:
    cat("\nMean and Variance Equation:\n ")
    cat(as.character(object@formula[1]), "+", 
        as.character(object@formula[2]), "\n")
        
    # Conditional Distribution:
    cat("\nConditional Distribution:\n ")
    cat(object@fit$params$cond.dist, "\n")
  
    # Coefficients:
    cat("\nCoefficient(s):\n")
    digits = max(6, getOption("digits") - 4)
    print.default(format(object@fit$par, digits = digits), print.gap = 2, 
         quote = FALSE)    
    
    # Error Analysis:
    digits = max(4, getOption("digits") - 5)
    fit = object@fit 
    signif.stars = getOption("show.signif.stars")
    cat("\nError Analysis:\n")
    printCoefmat(fit$matcoef, digits = digits, signif.stars = signif.stars) 
    
    # Log Likelihood:
    cat("\nLog Likelihood:\n ")
    LLH = object@fit$value
    N = length(object@data$x)
    cat(LLH, "   normalized: ", LLH/N, "\n")
        
    # Description:
    cat("\nDescription:\n ")
    cat(object@description, "\n")

    # Return Value:
    cat("\n")
    invisible()
}


# ------------------------------------------------------------------------------


summary.fGARCH = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print method for an object of class "fGARCH"
    
    # Arguments:
    #   object - an object of class 'fGARCH'
    
    # FUNCTION:
     
    # Title:
    cat("\nTitle:\n ")
    cat(object@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), "\n")
    
    # Mean Equation:
    cat("\nMean and Variance Equation:\n ")
    cat(as.character(object@formula[1]), "+", 
        as.character(object@formula[2]), "\n")
        
    # Conditional Distribution:
    cat("\nConditional Distribution:\n ")
    cat(object@fit$params$cond.dist, "\n")
  
    # Coefficients:
    cat("\nCoefficient(s):\n")
    digits = max(6, getOption("digits") - 4)
    print.default(format(object@fit$par, digits = digits), print.gap = 2, 
         quote = FALSE)    
    
    # Error Analysis:
    digits = max(4, getOption("digits") - 5)
    fit = object@fit 
    # fit$cvar = solve(fit$hessian)
    # fit$se.coef = sqrt(diag(fit$cvar))
    # fit$tval = fit$coef/fit$se.coef
    # fit$matcoef = cbind(fit$coef, fit$se.coef, 
    #     fit$tval, 2*(1-pnorm(abs(fit$tval))))
    # dimnames(fit$matcoef) = list(names(fit$tval), c(" Estimate", 
    #    " Std. Error", " t value", "Pr(>|t|)"))
    signif.stars = getOption("show.signif.stars")
    cat("\nError Analysis:\n")
    printCoefmat(fit$matcoef, digits = digits, signif.stars = signif.stars) 
    
    # Log Likelihood:
    cat("\nLog Likelihood:\n ")
    LLH = object@fit$value
    N = length(object@data$x)
    cat(LLH, "   normalized: ", LLH/N, "\n")
     
    # If we used '.garchTsFit' then ...
    if (as.character(object@call[1]) == ".garchTsFit") {
        # Description:
        cat("\nDescription:\n ")
        cat(object@description, "\n")
        # Return Value:
        cat("\n")
        return(invisible())
    }
        
    # Lagged Series:
    .tslagGarch = function (x, k = 1) {
        ans = NULL
        for (i in k) ans = cbind(ans, .tslag1Garch(x, i))
        indexes = (1:length(ans[, 1]))[!is.na(apply(ans, 1, sum))]
        ans = ans[indexes, ]
        if (length(k) == 1) ans = as.vector(ans)
        ans }
    .tslag1Garch = function (x, k) {
        c(rep(NA, times = k), x[1:(length(x) - k)]) }
        
    # Statistical Tests:
    cat("\nStandadized Residuals Tests:\n")
    r.s = object@residuals/sqrt(object@h.t)
    ans = NULL
    # Normality Tests:
    jbtest = jarqueberaTest(r.s)@test
    ans = rbind(ans, c(jbtest[1], jbtest[2]))
    if (length(r.s) < 5000) {
        swtest = shapiro.test(r.s)
        if (swtest[2] < 2.6e-16) swtest[2] = 0
        ans = rbind(ans, c(swtest[1], swtest[2]))
    } else {
        ans = rbind(ans, c(NA, NA))
    }
    # Ljung-Box Tests:
    box10 = Box.test(r.s, lag = 10, type = "Ljung-Box")
    box15 = Box.test(r.s, lag = 15, type = "Ljung-Box")
    box20 = Box.test(r.s, lag = 20, type = "Ljung-Box")
    ans = rbind(ans, c(box10[1], box10[3]))
    ans = rbind(ans, c(box15[1], box15[3]))
    ans = rbind(ans, c(box20[1], box20[3]))
    box10 = Box.test(r.s^2, lag = 10, type = "Ljung-Box")
    box15 = Box.test(r.s^2, lag = 15, type = "Ljung-Box")
    box20 = Box.test(r.s^2, lag = 20, type = "Ljung-Box")
    ans = rbind(ans, c(box10[1], box10[3]))
    ans = rbind(ans, c(box15[1], box15[3]))
    ans = rbind(ans, c(box20[1], box20[3]))
    # Ljung-Box Tests - tslag required 
    lag.n = 12
    x.s = as.matrix(r.s)^2
    n = nrow(x.s)
    tmp.x = .tslagGarch(x.s[, 1], 1:lag.n)
    tmp.y = x.s[(lag.n + 1):n, 1]
    fit = lm(tmp.y ~ tmp.x)
    stat = (n-lag.n) * summary.lm(fit)$r.squared
    ans = rbind(ans, c(stat, p.value = 1 - pchisq(stat, lag.n)) )
    # Add Names:
    rownames(ans) = c(
        " Jarque-Bera Test   R    Chi^2 ",
        " Shapiro-Wilk Test  R    W     ",
        " Ljung-Box Test     R    Q(10) ",
        " Ljung-Box Test     R    Q(15) ",
        " Ljung-Box Test     R    Q(20) ",
        " Ljung-Box Test     R^2  Q(10) ",
        " Ljung-Box Test     R^2  Q(15) ",
        " Ljung-Box Test     R^2  Q(20) ",
        " LM Arch Test       R    TR^2  ")
    colnames(ans) = c("Statistic", "p-Value")
    print(ans)
    
    # Information Criterion Statistics:
    cat("\nInformation Criterion Statistics:\n")
    print(object@fit$ics)
        
    # Description:
    cat("\nDescription:\n ")
    cat(object@description, "\n")

    # Return Value:
    cat("\n")
    invisible()
}


# ------------------------------------------------------------------------------


plot.fGARCH =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class 'fGARCH'
    
    # Note:
    #   This method can also be used for plotting graphs fitted by 
    #   the function 'garch' from the contributed R package 'tseries'.
    
    # FUNCTION:

    # If we used '.garchTsFit' then ...
    if (as.character(x@call[1]) == ".garchTsFit") {
        fit = x@fit
        class(fit) = "garch"
        plot(fit)
        # Return Value:
        return(invisible())
    }
    
    # If we used 'garchFit' then ...
    plot.1 <<- function(x, ...) {
        # 1. Time Series:
        xseries = x@data$x
        plot(xseries, type = "l", col = "steelblue", ylab = "x",
            main = "Time Series")
        abline(h = 0, col = "grey", lty = 3)
        grid()
    }       
    plot.2 <<- function(x, ...) {
        # 2. Conditional SD:
        xcsd = x@sigma.t
        plot(xcsd, type = "l", col = "steelblue", ylab = "x",
            main = "Conditional SD")
        abline(h = 0, col = "grey", lty = 3)
        grid()
    }   
    plot.3 <<- function(x, ...) {           
        # 3. Series with 2 Conditional SD Superimposed:
        xseries = x@data$x
        xcsd = x@sigma.t
        ci = 2
        plot(xseries, type = "l", col = "steelblue", ylab = "x",
            main = "Series with 2 Conditional SD Superimposed")
        lines(+ci * xcsd, col = "grey")
        lines(-ci * xcsd, col = "grey")
        abline(h = 0, col = "grey", lty = 3)
        grid()
    }           
    plot.4 <<- function(x, ...) {        
        # 4. ACF of the Observations:
        xseries = x@data$x
        n = length(xseries)
        lag.max = as.integer(10*log10(n))
        acf(xseries, lag.max = lag.max, xlab = "Lags", col = "steelblue", 
            main = "ACF of Observations", plot = TRUE)
    }   
    plot.5 <<- function(x, ...) {       
        # 5. ACF of the Squared Observations:
        xseries = x@data$x
        xseries2 = xseries^2
        n = length(xseries)
        lag.max = as.integer(10*log10(n))
        acf(xseries2, lag.max = lag.max, xlab = "Lags", col = "steelblue", 
            main = "ACF of Squared Observations", plot = TRUE)
    }           
    plot.6 <<- function(x, ...) {
        # 6. Cross Correlation between x^2 and x:
        xseries = x@data$x
        xseries2 = xseries^2
        n = length(xseries)
        lag.max = as.integer(10*log10(n))
        ccf(xseries2, xseries, lag.max = lag.max, xlab = "Lags", 
            main = "Cross Correlation", plot = TRUE, col = "steelblue")
    }
    plot.7 <<- function(x, ...) {
        # 7. Residuals:
        res = residuals(x, standardize = FALSE)
        plot(res, type = "l", main = "Residuals", col = "steelblue")
        abline(h = 0, lty = 3)
        grid()
    }   
    plot.8 <<- function(x, ...) {
        # 8. Conditional SDs:
        xcsd = x@sigma.t
        plot(xcsd, type = "l", main = "Conditional SD's", 
            col = "steelblue")
        abline(h = 0, lty = 3)
        grid()
    }   
    plot.9 <<- function(x, ...) {
        # 9. Standardized Residuals:
        sres = residuals(x, standardize = FALSE)
        plot(sres, type = "l", main = "Standardized Residuals", 
            col = "steelblue")
        abline(h = 0, lty = 3)
        grid()
    }       
    plot.10 <<- function(x, ...) {
        # 10. ACF of Standardized Residuals:
        sres = residuals(x, standardize = FALSE)
        n = length(sres)
        lag.max = as.integer(10*log10(n))
        acf(sres, lag.max = lag.max, xlab = "Lags", col = "steelblue", 
            main = "ACF of Standardized Residuals", plot = TRUE)
    }           
    plot.11 <<- function(x, ...) {
        # 11. ACF of Squared Standardized Residuals:
        sres2 = residuals(x, standardize = FALSE)^2
        n = length(sres2)
        lag.max = as.integer(10*log10(n))
        acf(sres2, lag.max = lag.max, xlab = "Lags", col = "steelblue", 
            main = "ACF of Standardized Residuals", plot = TRUE)
    }           
    plot.12 <<- function(x, ...) {      
        # 12. Cross Correlation between r^2 and r:
        sres = residuals(x, standardize = FALSE)
        sres2 = sres^2
        n = length(sres)
        lag.max = as.integer(10*log10(n))
        ccf(sres2, sres, lag.max = lag.max, xlab = "Lags", 
            main = "Cross Correlation", plot = TRUE, col = "steelblue")
    }   
    plot.13 <<- function(x, ...) {
        # 13. QQ-Plot of Standardized Residuals:
        sres = residuals(x, standardize = FALSE)
        cond.dist = x@fit$params$cond.dist
        nc = nchar(x@fit$params$cond.dist)
        cond.dist = paste("q", substr(cond.dist, 2, nc), sep = "")
        skew = x@fit$params$skew
        shape = x@fit$params$shape
        if (cond.dist == "qnorm")
            .qqDist(sres, dist = cond.dist)
        if (cond.dist == "qstd" | cond.dist == "qged")
            .qqDist(sres, dist = cond.dist, nu = shape)
        if (cond.dist == "qsnorm")
            .qqDist(sres, dist = cond.dist, xi = skew)
        if (cond.dist == "qsstd" | cond.dist == "qsged")
            .qqDist(sres, dist = cond.dist, xi = skew, nu = shape)
    }
    
    # Plot:
    .interactiveGarchPlot(
        x,
        choices = c(
            "Time Series",
            "Conditional SD",
            "Series with 2 Conditional SD Superimposed",
            "ACF of Observations",
            "ACF of Squared Observations",
            "Cross Correlation",
            "Residuals",
            "Conditional SDs",
            "Standardized Residuals",
            "ACF of Standardized Residuals",
            "ACF of Squared Standardized Residuals",
            "Cross Correlation between r^2 and r",
            "QQ-Plot of Standardized Residuals"),
        plotFUN = c(
            "plot.1",  "plot.2",  "plot.3", "plot.4", "plot.5",
            "plot.6",  "plot.7",  "plot.8", "plot.9", "plot.10",
            "plot.11", "plot.12", "plot.13"),
        which = which) 
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


.qqDist = 
function (y, dist = "qnorm", ylim = NULL, main = paste(dist, "- QQ Plot"), 
xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", doplot = TRUE, 
datax = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description
    #   QQ Plot for arbitray distribution
    
    # FUNCTION:
    # print(dist)
    
    # Match Function :
    qDist = match.fun(dist)
    
    # Check Arguments:
    if (substr(dist, 1, 1) != "q") stop("dist is misspecified")
    # test = class(test = try(qDist(0.5, ...), silent = TRUE))
    # if (test == "try-error") stop("dist does not exist")
    
    # Transform to Vector Mode:
    y = as.vector(y)
    
    # Compute Data:
    if (has.na <- any(ina <- is.na(y))) {
        yN = y
        y = y[!ina]
    }
    if (0 == (n <- length(y))) stop("y is empty or has only NAs")
    x <- qDist(ppoints(n,), ...)[order(order(y))]
    if (has.na) {
        y = x
        x = yN
        x[!ina] = y
        y = yN
    }
    
    # Create QQ Plot:
    if (doplot) { 
        if (is.null(ylim)) ylim = range(y)
        if (datax) {
            plot(y, x, main = main, xlab = ylab, ylab = xlab, xlim = ylim,
                col = "steelblue", cex = 0.7)
        } else {
            plot(x, y, main = main, xlab = xlab, ylab = ylab, ylim = ylim,
                col = "steelblue", cex = 0.7)
        }
        .qqLine(y = y, dist = dist, datax = datax, ...)
        grid()
    }
    
    # Return Value:
    invisible(if (datax) list(x = y, y = x) else list(x = x, y = y))
}


# ------------------------------------------------------------------------------


.qqLine = 
function (y, dist = "qnorm", datax = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description
    #   Add slope to QQ Plot for arbitray distribution
    
    # FUNCTION:
    
    # Match Function :
    qDist = match.fun(dist)
    
    # Check Arguments:
    if (substr(dist, 1, 1) != "q") stop("dist is misspecified")
    # test = class(test = try(qDist(0.5, ...), silent = TRUE))
    # if (test == "try-error") stop("dist does not exist")
    
    # Transform to Vector Mode:
    y = as.vector(y)
    
    # Compute Data:
    y = quantile(y[!is.na(y)], c(0.25, 0.75))
    x = qDist(c(0.25, 0.75), ...)
    
    # Add Slope:
    if (datax) {
        slope <- diff(x)/diff(y)
        int <- x[1] - slope * y[1]
    } else {
        slope <- diff(y)/diff(x)
        int <- y[1] - slope * x[1]
    }
    
    # Return Value:
    abline(int, slope)
}


# ------------------------------------------------------------------------------


.interactiveGarchPlot = 
function(x, choices = paste("Plot", 1:19), 
plotFUN = paste("plot.", 1:19, sep = ""), which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "template".
    
    # Arguments:
    #   x - an object to be plotted
    #   choices - the character string for the choice menu
    #   plotFUN - the names of the plot functions
    #   which - plot selection, which graph should be 
    #     displayed. If a character string named "ask" the 
    #     user is interactively asked which to plot, if
    #     a logical vector of length N, those plots which
    #     are set "TRUE" are displayed, if a character string
    #     named "all" all plots are displayed.
    
    # Note:
    #   At maximum 19 plots are supported.

    # FUNCTION:
    
    # Some cecks:
    if (length(choices) != length(plotFUN)) 
        stop("Arguments choices and plotFUN must be of same length.")
    if (length(which) > length(choices)) 
        stop("Arguments which has incorrect length.")
    if (length(which) > length(plotFUN)) 
        stop("Arguments which has incorrect length.")
    if (length(choices) > 19)
        stop("Sorry, only 19 plots at max are supported.")
    
    # Internal "askPlot" Function:                
    multPlot = function (x, choices, ...) 
    {
        # Selective Plot:
        selectivePlot = function (x, choices, FUN, which){
            # Internal Function:
            askPlot = function (x, choices, FUN) {
                # Pick and Plot:
                pick = 1; n.plots = length(choices)
                while (pick > 0) { pick = menu (
                    choices = paste("plot:", choices), 
                    title = "\nMake a plot selection (or 0 to exit):")
                    if (pick > 0) match.fun(FUN[pick])(x) } }                   
            if (as.character(which[1]) == "ask") {
                askPlot(x, choices = choices, FUN = FUN, ...) }
            else { 
                for (i in 1:n.plots) if (which[i]) match.fun(FUN[i])(x) }
            invisible() }  
        # match Functions, up to nine ...
        if (length(plotFUN) < 19) plotFUN = 
            c(plotFUN, rep(plotFUN[1], times = 19 - length(plotFUN)))
        plot.1  = match.fun(plotFUN[1]);  plot.2  = match.fun(plotFUN[2]) 
        plot.3  = match.fun(plotFUN[3]);  plot.4  = match.fun(plotFUN[4]) 
        plot.5  = match.fun(plotFUN[5]);  plot.6  = match.fun(plotFUN[6]) 
        plot.7  = match.fun(plotFUN[7]);  plot.8  = match.fun(plotFUN[8]) 
        plot.9  = match.fun(plotFUN[9]);  plot.10 = match.fun(plotFUN[10])
        plot.11 = match.fun(plotFUN[11]); plot.12 = match.fun(plotFUN[12]) 
        plot.13 = match.fun(plotFUN[13]); plot.14 = match.fun(plotFUN[14]) 
        plot.15 = match.fun(plotFUN[15]); plot.16 = match.fun(plotFUN[16]) 
        plot.17 = match.fun(plotFUN[17]); plot.18 = match.fun(plotFUN[18]) 
        plot.19 = match.fun(plotFUN[19])        
        pick = 1
        while (pick > 0) { pick = menu (
            ### choices = paste("plot:", choices),
            choices = paste(" ", choices), 
            title = "\nMake a plot selection (or 0 to exit):")
            # up to 19 plot functions ...
            switch (pick, 
                plot.1(x),  plot.2(x),  plot.3(x),  plot.4(x),  plot.5(x), 
                plot.6(x),  plot.7(x),  plot.8(x),  plot.9(x),  plot.10(x),
                plot.11(x), plot.12(x), plot.13(x), plot.14(x), plot.15(x), 
                plot.16(x), plot.17(x), plot.18(x), plot.19(x)) 
        } 
    }
                              
    # Plot:
    if (is.numeric(which)) {
        Which = rep(FALSE, times = length(choices))
        Which[which] = TRUE
        which = Which
    }
    if (which[1] == "all") {
        which = rep(TRUE, times = length(choices))
    }
    if (which[1] == "ask") {
        multPlot(x, choices, ...) 
    } else {
        for ( i in 1:length(which) ) {
            FUN = match.fun(plotFUN[i])
            if (which[i]) FUN(x) 
        } 
    }
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


residuals.fGARCH = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   S3 Residuals method for an object of class fGARCH

    # FUNCTION:
    
    # Return Value:
    .residuals.fGARCH(object = object, ...) 
}


# ------------------------------------------------------------------------------


.residuals.fGARCH = 
function(object, standardize = FALSE) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   S3 Residuals method for an object of class fGARCH

    # FUNCTION:
    
    # Residuals:
    if (standardize) {
        ans = object@residuals/object@sigma.t
    } else {
        ans = object@residuals
    }
    
    # Return Value:
    ans
    
}

    
# ------------------------------------------------------------------------------


fitted.fGARCH = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:  
    #   S3 Fitted values method for an object of class fGARCH
    
    # FUNCTION:
    
    # Fitted Values:
    ans = object@fitted
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


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


# ------------------------------------------------------------------------------


.truePersistence =
function(fun = "dnorm", alpha = 1, gamma = 0, beta = 0, delta = 1, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes persistence for an APARCH process
    
    # Arguments:
    #   fun - name of density functions of APARCH innovations
    #   alpha, gamma - numeric value or vector of APARCH coefficients,
    #       must be of same length  
    #   beta - numeric value or vector of APARCH coefficients
    #   delta - numeric value of APARCH exponent
    
    # Note:
    #   fun is one of: dnorm, dsnorn, dstd, dsstd, dged, dsged
    
    # FUNCTION:
    
    # Match Density Function:
    fun = match.fun(fun)
    
    # Persisgtence Function: E(|z|-gamma z)^delta
    e = function(x, gamma, delta, ...) {
        (abs(x)-gamma*x)^delta * fun(x, ...)
    }
        
    # Compute Persistence by Integration:
    persistence = sum(beta)
    for (i in 1:length(alpha)) {
        I = integrate(e, -Inf, Inf, subdivisions = 1000, 
            rel.tol = .Machine$double.eps^0.5, 
            gamma = gamma[i], delta = delta, ...)
        persistence = persistence + alpha[i] * I[[1]]
    }
    
    # Warning:
    if (persistence >= 1) {  
        p = as.character(round(persistence, digits = 3))
        warning(paste("Divergent persistence p =", p))
    }
    
    # Return Value:
    persistence
}


################################################################################
# PART IV: Forecasting


garchKappa = 
function(cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
gamma = 0, delta = 2, skew = NA, shape = NA)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Expection for APARCH Models
    
    # FUNCTION:
    
    # Compute kappa:
    kappa = integrate(.funE, lower = -Inf, upper = Inf, cond.dist = 
        cond.dist[1], gamma = gamma, delta = delta, skew = skew, shape = 
        shape)[[1]] 
    names(kappa) = "kappa"
    attr(kappa, "control") = 
        c(gamma = gamma, delta = delta, skew = skew, shape = shape)
    attr(kappa, "cond.dist") = cond.dist[1]
    
    # Return Value:
    kappa
}


# ------------------------------------------------------------------------------


.funE = 
function(x, cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
gamma = 0, delta = 2, skew = NA, shape = NA)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Internal function used by kappa()

    # FUNCTION:
    
    # Compute Expectation Value for ...
    funcE = (abs(x) - gamma*x)^delta
    
    # Select Appropriate Conditional Density:
    cond.dist = cond.dist[1]
    if (cond.dist == "dnorm") {
        fun = funcE * dnorm(x)
    }
    if (cond.dist == "dged") {
        fun = funcE * dged(x, nu = shape) 
    }
    if (cond.dist == "dstd") {
        fun = funcE * dstd(x, nu = shape) 
    }
    if (cond.dist == "dsnorm") {
        fun = funcE * dsnorm(x, xi = skew)
    }
    if (cond.dist == "dsged") {
        fun = funcE * dsged(x, nu = shape, xi = skew) 
    }
    if (cond.dist == "dsstd") {
        fun = funcE * dsstd(x, nu = shape, xi = skew) 
    }
    
    # Return Value:
    fun
} 


################################################################################

