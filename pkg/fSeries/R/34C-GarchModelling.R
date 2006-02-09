
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
#  .garchInitSeries        Initializes Series
#  .garchInitParameters    Initializes Parameters
#  .garchSetCondDist       Selects conditional density function
#   .garchDist              Defines conditional density function
#  .garchOptimizeLLH       Opimizes log-likelihood function
#   .garchLLH               Computes log-likelihood function
# METHODS:                DESCRIPTION:
#  print.fGARCH            S3 Print method for an object of class fGARCH
#  plot.fGARCH             S3 Plot method for an object of class fGARCH
#  summary.fGARCH          S3 Summary method for an object of class fGARCH
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
        distribution = "character")  
)
        
        
# ------------------------------------------------------------------------------


garchSpec =
function (model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), 
presample = NULL, cond.dist = c("rnorm", "rged", "rstd", "rsnorm", 
"rsged", "rsstd"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a "garchSpec" object from scratch.
    
    # Arguments:
    #   model - a list with the model parameters as entries
    #  	  omega - the variance value for GARCH/APARCH 
    #		specification,
    #     alpha - a vector of autoregressive coefficients 
    #    	of length p for the GARCH/APARCH specification,
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
    # 	    distributional parameters.
    #   presample - either a multivariate "timeSeries", a 
    #       multivariate "ts", a "data.frame" object or a numeric 
    #       "matrix" with 3 columns and at least max(m,n,p,q) 
    #       rows. The first culumn are the innovations, the second
    #		the conditional variances, and the last the time series.
    #   condd.dist - a character string naming the distribution 
    #       function.
    
    # Slots:
    #   call - the function call.
    #   formula - a formula object describing the model, e.g. 
    #       ARMA(m,n) + GARCH(p,q). ARMA can be missing or 
    #       specified as AR(m) or MA(n) in the case of pure 
    #       autoregressive or moving average models. GARCH may 
    #       alternatively specified as ARCH(p) or APARCH(p,q).
    #       If formula is set to "NA", the formula is constructed
    #       from the "model" list.
    #	model - as declared in the input.
   
    # FUNCTION:
    
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
    if (is.null(model$dist)) {                      
        if (distribution == "rnorm")  model$dist = NULL
        if (distribution == "rged")   model$dist = c(nu = 2)
        if (distribution == "rstd")   model$dist = c(nu = 4)
        if (distribution == "rsnorm") model$dist = c(xi = 1.5)
        if (distribution == "rsged")  model$dist = c(nu = 2, xi = 1.5)
        if (distribution == "rssdt")  model$dist = c(nu = 4, xi = 1.5) 
    } else { 
        model$dist = model$dist 
        if (distribution == "rged")   names(model$dist) = "nu"
        if (distribution == "rstd")   names(model$dist) = "nu"
        if (distribution == "rsnorm") names(model$dist) = "xi"
        if (distribution == "rsged")  names(model$dist) = c("nu", "xi")
        if (distribution == "rssdt")  names(model$dist) = c("nu", "xi")
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
	        	dist = model$dist), 
	        presample = as.matrix(presample),
	        distribution = as.character(distribution)
        )    
        
    # Return Value:
    ans     
}


# ------------------------------------------------------------------------------


print.garchSpec =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #	S3 Print Method for objects of class garchSpec
    
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
            cat(" xi =", x@model$dist[1])
        }
        if (x@distribution == "rged" | x@distribution == "rstd") {
            cat("\nDistributional Parameter: \n")
            cat(" nu =", x@model$dist) 
        }
        if (x@distribution == "rsged" | x@distribution == "rsstd") {
            cat("\nDistributional Parameters: \n")
            cat(" nu =", x@model$dist[1], " xi =", x@model$dist[2])
        }
    }
    
    # Presample:
    cat("\nPresample: \n")
    n = -(length(x@presample[,1])-1)
    time = 0:n
    print(data.frame(cbind(time, x@presample)))
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.garchSpecRUnit = 
function()
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	R Unit Testing
	
	# FUNCTION:
	
	# Internal print Function:
	Print = function(x) {
		cat("RUnit Test:\n ")
		print(x@call)
		print(x)
		cat("\n")
	}
	
	# ARCH(1) using default omega and default alpha[1]
	Print(garchSpec(model = list())) 
	
	# ARCH(1) # using default omega
	Print(garchSpec(model = list(alpha = 0.2))) 
	
	# ARCH(1)
	Print(garchSpec(model = list(omega = 2.0e-6, alpha = 0.3)))
	
	# AR(1)-ARCH(1) using default omega and alpha[1]
	Print(garchSpec(model = list(ar = 0.5))) 
	
	# AR([1,5])-ARCH(1) using default omega
	Print(garchSpec(model = list(ar = c(0.5,0,0,0,0.1), alpha = 0.25)))
	
	# ARMA(1,2)-ARCH(1) using default omega and default alpha[1]
	Print(garchSpec(model = list(ar = 0.5, ma = c(0.3,-0.3))))
	
	# ARMA(2,2)-ARCH(1) using default omega and default alpha[1]
	Print(garchSpec(model = list(ar = c(0.5, -0.5), ma = c(0.3,-0.3))))
	
	# ARCH(2)
	Print(garchSpec(model = list(alpha = c(0.12, 0.04))))
	
	# GARCH(1,1) using defaults
	Print(garchSpec())
	
	# GARCH(1,1) using default omega
	Print(garchSpec(model = list(alpha = 0.2, beta = 0.7)))
	
	# GARCH(1,1) 
	Print(garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8)))
	
	# GARCH(1,2) using default omega
	Print(garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4))))
	
	# GARCH(2,1) using default omega
	Print(garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08)))
	
	# rsnorm-ARCH(1) using default omega and alpha[1]
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
presample = NULL, cond.dist = c("rnorm", "rged", "rstd", "rsnorm", "rsged", 
"rsstd"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates a time series process from the GARCH family
    
    # Arguments:
    #   model - either a specification object of class 'garchSpec' 
    #     or a list with the model parameters as entries
    #     ar - a vector of autoregressive coefficients of 
    #     	length m for the ARMA specification,
    #     ma - a vector of moving average coefficients of 
    #    	length n for the ARMA specification,
    #     omega - the variance value for GARCH/APARCH 
    #     	specification,
    #     alpha - a vector of autoregressive coefficients 
    #     	of length p for the GARCH/APARCH specification,
    #     gamma - a vector of leverage coefficients of 
    #     	length p for the APARCH specification,
    #     beta - a vector of moving average coefficients of 
    #     	length q for the GARCH/APARCH specification,
    #     mu - the mean value for ARMA specification,
    #     delta - the exponent value used in the variance 
    #     	equation.
    #     dist - a vector with the distributional parameters.
    #   n - an integer, the length of the series
    #   presample - either a multivariate "timeSeries", a 
    #       multivariate "ts", a "data.frame" object or a numeric 
    #       "matrix" with 3 columns and at least max(m,n,p,q) 
    #       rows. The first culumn ...
    #   cond.dist - a character string naming the conditional distribution 
    #       function. Valid strings are: "rnorm", "rged", "rstd", "rsnorm", 
    #		"rsged", and "rsstd".
    
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
	    	cond.dist = cond.dist)
	    ans = .garchSim(n = n, spec = spec)
 	} else if (class(model) == "garchSpec") {
		ans = .garchSim(n = n, spec = model)
	} else {
		stop("model must be an object of class list or garchSpec")
	}

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.garchSim =
function(n = 1000, spec = garchSpec())
{   # A function implemented by Diethelm Wuertz
    
    # Determine Orders:
    order.ar = order.ma = order.alpha = order.gamma = order.beta = 1    
    if (sum(abs(spec@model$ar)) != 0) {  
        model.ar = spec@model$ar
        order.ar = length(spec@model$ar) 
	} else {
		model.ar = 0
	}
    if (sum(abs(spec@model$ma)) != 0) {
        model.ma = spec@model$ar
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
        z = rged(n, nu = spec@model$dist[1])
    if (spec@distribution == "rstd")       
        z = rstd(n, nu = spec@model$dist[1])
    if (spec@distribution == "rsnorm") 
        z = rsnorm(n, xi = spec@model$dist[1])
    if (spec@distribution == "rsged")  
        z = rsged(n, nu = spec@model$dist[1], xi = spec@model$dist[2])
    if (spec@distribution == "rsstd")   
        z = rsstd(n, nu = spec@model$dist[1], xi = spec@model$dist[2])
    
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
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Unit Testing
	
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
#  .garchInitSeries        Initializes Series
#  .garchInitParameters    Initializes Parameters
#  .garchSetCondDist       Selects conditional density function
#   .garchDist              Defines conditional density function
#  .garchOptimizeLLH       Opimizes log-likelihood function
#   .garchLLH               Computes log-likelihood function
# METHODS:                DESCRIPTION:
#  print.fGARCH            S3 Print method for an object of class fGARCH
#  plot.fGARCH             S3 Plot method for an object of class fGARCH
#  summary.fGARCH          S3 Summary method for an object of class fGARCH
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
        fitted.values = "numeric",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------
      

garchFit =
function(formula.mean = ~arma(0, 0), formula.var = ~garch(1, 1), 
series = x, presample =  NULL, 
cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
symmetric = TRUE, trace = TRUE, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description
    #   Fit parameters to a ARMA-GARCH model
    
    # Arguments:
    #   formula.mean - ARMA(u,v) specification, not yet implemented
    #   formula.var - GARCH(p,q) specification
    #   series - names time series "x" 
    #   presample - presample specification
    #   cond.dist - conditional distribution of innovations
    #    init.mean - initial values for the mean equation
    #    init.var - initial values for the variance equation
    #    init.delta - initial value of exponent delta
    #    init.dist - initial distributional parameters
    #    fix.mean - which values of the mean equation should be fixed?
    #    fix.var - which values of the variance equation should be fixed?
    #    fix.delta - should the delta be fixed?
    #    fix.dist -which distributional parameters should be fixed?
    #    h.start - start value for recursion of the mean-variance equation
    #    llh.start - start value for the valuation of the log-likelihood
    #   symmetric - should the model be symmetric?
    #   trace - should the optimization be traced?
    #   title - an optional title string
    #   description - an optional project description
        
    # FUNCTION:
    
    # Implemented But Not Yet Fully Tested:
    # Individual Start Values - Parameter Fixing - Conditioning Upon Start
    init.mean = NULL; init.var = NULL; init.delta = NULL; init.dist = NULL
	fix.mean = NULL; fix.var = NULL; fix.delta = NULL; fix.dist = NULL
	h.start = NULL; llh.start = NULL  
    
    # Trace:
    .trace <<- trace  
    
    # Initialize Time Series Information:            
    if (.trace) cat("\nSeries Initialization:\n")
    .series <<- .garchInitSeries(formula.mean = formula.mean, 
        formula.var = formula.var, series = series, 
        presample = presample, h.start = h.start, llh.start = llh.start)
        
    # Initialize Model Parameters:
    if (.trace) cat("Parameter Initialization:\n")
    .params <<- .garchInitParameters(formula.mean = formula.mean, 
        formula.var = formula.var, cond.dist = cond.dist[1], 
        init.mean = init.mean, init.var = init.var, 
        init.delta = init.delta , init.dist = init.dist,
        fix.mean = fix.mean, fix.var = fix.var, fix.delta = fix.delta, 
        fix.dist = fix.dist, symmetric = symmetric)
        
    # Conditional Distribution Function:
    .garchDist <<- .garchSetCondDist(cond.dist[1]) 
    
    # Optimize: 
    .llh <<- 1.0e99        
    fit = .garchOptimizeLLH(...)  
    fit$series = .series
    fit$params = .params
     
    # Delta Parameter:
    if (is.na(fit$par["delta"])) {
	    fit$delta = 1/2
    } else {
	    fit$delta = 1/fit$par["delta"]
    }
        
    # Residuals and Fitted Values: 
    MAX = max(.series$order) 
    residuals = .series$z[-(1:MAX)]
  	fitted.values = .series$x[-(1:MAX)] - residuals

    # Add Title and Description:
    if (is.null(title)) title = "GARCH Modelling"
    if (is.null(description)) description = as.character(date())
   
    # Return Value:
    new("fGARCH",     
        call = as.call(match.call()),
        formula = list(formula.mean = formula.mean, formula.var = formula.var), 
        method = "Max Log-Likelihood Estimation", 
        data = list(x = series),
        fit = fit,        
        residuals = residuals,
        fitted.values = fitted.values,
        title = as.character(title),
        description = as.character(description) 
    )
}


# ------------------------------------------------------------------------------


.garchInitSeries = 
function(formula.mean, formula.var, series, 
presample = NULL, h.start = NULL, llh.start = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Initialize time series
    
    # Note:
    #   The variable '.trace' must be declared globally.
    
    # FUNCTION:
    
    # ARMA - Check Mean Formula:
    mm = length(formula.mean)
    if (mm != 2) stop("Mean Formula misspecified") 
    end = regexpr("\\(", as.character(formula.mean[mm])) - 1
    model.mean = substr(as.character(formula.mean[mm]), 1, end)
    if (!any( c("ar", "ma", "arma") == model.mean))
        stop("formula.mean must be one of: ar, ma, arma")
    
    # GARCH - Check Variance Formula:
    mv = length(formula.var)
    if (mv != 2) top("Variance Formula misspecified")
    end = regexpr("\\(", as.character(formula.var[mv])) - 1
    model.var = substr(as.character(formula.var[mv]), 1, end)
    if (!any( c("garch", "aparch") == model.var))
        stop("formula.mean must be one of: garch, aparch") 
    
    # ARMA - Determine Mean Order:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.mean), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    u = model.order[1]
    v = 0
    if (length(model.order) == 2) v = model.order[2]
    maxuv = max(u, v)
    
    # GARCH - Determine Variance Order:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.var), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    p = model.order[1]
    q = 0
    if (length(model.order) == 2) q = model.order[2]
    maxpq = max(p, q)
    
    # PURE ARMA MODEL:
    if (p+q == 0)
    	stop("Misspecified GARCH Model: Both Orders are zero!")
    
    # Maximum Order:
    max.order = max(maxuv, maxpq)
        
    # Presample:
    if (is.null(presample)) {
        mu = mean(series)
        pre.x = rep(mean(series), max.order)
        pre.h = rep(mean((series - mu)^2), max.order)
        pre.z = sqrt(pre.h)
        presample = data.frame(z = pre.z, h = pre.h, x = pre.x)
    }
        
    # Save Augmented Series:
    z = c(presample[, 1], rep(0, times = length(series)))
    h = c(presample[, 2], rep(var(series), times = length(series)))
    x = c(presample[, 3], series)
    
    # Start Position of Series "h":
    if (is.null(h.start)) h.start = max.order + 1
      
    # llh Start Value, Previous LLH, and Counter:
    if (is.null(llh.start)) llh.start = h.start
      
    # Presample Values:
    timeIndex = 1:max.order
    presample = data.frame(
        z = z[timeIndex], 
        h = h[timeIndex],
        x = x[timeIndex])
    rowIndex = -rev(timeIndex) + 1
    rownames(presample) = paste("   ", rowIndex)
    
    # Trace:
    if (.trace) {
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
        cat("\n Presample:                ", "\n")
        print(presample)
        cat("\n")
    }

    # Result:
    ans  = list(
        model = c(model.mean, model.var), 
        order = c(u = u, v = v, p = p, q = q), 
        z = z, h = h, x = x, presample = presample, 
        h.start = h.start, llh.start = llh.start)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------
      

.garchInitParameters = 
function(formula.mean, formula.var, cond.dist = "dnorm", 
init.mean = NULL, init.var = NULL, init.delta = NULL,init.dist = NULL, 
fix.mean = NULL, fix.var = NULL, fix.delta = NULL, fix.dist = NULL, 
symmetric = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Note:
    #   The variable '.trace' must be declared globally.
    
    # FUNCTION:
    
    # DEBUG:
    .DEBUG = FALSE
    
    # ARMA - Determine Mean Order:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.mean), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    u = model.order[1]
    v = 0
    if (length(model.order) == 2) v = model.order[2]
    
    # GARCH - Determine Variance Order:
    model.order = as.numeric(strsplit(strsplit(strsplit(as.character(
        formula.var), "\\(")[[2]][2], "\\)")[[1]], ",")[[1]])
    p = model.order[1]
    q = 0
    if (length(model.order) == 2) q = model.order[2]
     
    # Names for Initial Parameters:
    Names = c(
        "mu", 
        if (u > 0) paste("ar", 1:u, sep = ""),
        if (v > 0) paste("ma", 1:v, sep = ""),   
        "omega",
        if (p > 0) paste("alpha", 1:p, sep = ""),
        if (p > 0 & !symmetric) paste("gamma", 1:p, sep = ""),
        if (q > 0) paste("beta",  1:q, sep = ""),
        "delta",
        if (cond.dist == "dged") "nu", 
        if (cond.dist == "dstd") "nu",  
        if (cond.dist == "dsnorm") "xi",  
        if (cond.dist == "dsged") c("nu", "xi"),  
        if (cond.dist == "dsstd") c("nu", "xi") ) 
    if (.DEBUG) { cat("\nDEBUG - Names: \n"); print(Names) }    
    
    # Set Lower and Upper Limits:
    BIG = 99
    SMALL = 1.0e-6
    x = .series$x[-(1:max(u, v, p, q))]     
    U = c(
        -10*abs(mean(x)), 
        if (u > 0) rep(-1, times = u),
        if (v > 0) rep(-1, times = v),
        1.0e-6*var(x), 
        if (p > 0) rep( SMALL, times = p),
        if (p > 0 & !symmetric) rep(-1+SMALL, times = p),
        if (q > 0) rep( SMALL, times = q),
        0,
        if (cond.dist == "dged") c(nu = SMALL),
        if (cond.dist == "dstd") c(nu = 2 + SMALL),  
        if (cond.dist == "dsnorm") xi = 1/BIG,  
        if (cond.dist == "dsged") c(nu = SMALL, xi = 1/BIG),   
        if (cond.dist == "dsstd") c(nu = SMALL, xi = 1/BIG)  )           
    V = c(
        10*abs(mean(x)), 
        if (u > 0) rep(1, times = u),
        if (v > 0) rep(1, times = v),
        10*var(x), 
        if (p > 0) rep(1-SMALL, times = p),
        if (p > 0 & !symmetric) rep(1-SMALL, times = p),
        if (q > 0) rep(1-SMALL, times = q),
        2,
        if (cond.dist == "dged") c(nu = BIG),
        if (cond.dist == "dstd") c(nu = BIG),  
        if (cond.dist == "dsnorm") c(xi = BIG),  
        if (cond.dist == "dsged") c(nu = BIG, xi = BIG),   
        if (cond.dist == "dsstd") c(nu = BIG, xi = BIG)  )      
    names(U) = names(V) = Names
    if (.DEBUG) { cat("\nDEBUG - U: \n"); print(U) }
    if (.DEBUG) { cat("\nDEBUG - V: \n"); print(V) }
    
    # Initialize All Model Parameters:
    if (is.null(init.mean)) {
        INIT.ARMA = TRUE
        if (INIT.ARMA) {
            # ARMA Parameter Estimation:
            fit = arima(.series$x, order = c(u, 0, v))$coef
            # Assign ARMA Coefficients:
            mu = fit[length(fit)]
            if (u > 0) {
                ar.fitted = fit[1:u]
                fit = fit[-(1:u)]
            }
            if (v > 0) ma.fitted = fit[1:v]
            init.mean = c(
                mu = mu, 
                if (u > 0) ar.fitted, 
                if (v > 0) ma.fitted)   
        } else {    
            # Assign Zero ARMA Coefficients:
            mu = mean(.series$x[!is.na(.series$x)]) 
            init.mean = c(
                mu = mu, 
                if (u > 0) rep(0, u), 
                if (v > 0) rep(0, v))
        }
    }
    if (is.null(init.var)) {
        # GARCH Coefficients:
        alpha.start = 0.1
        beta.start = 0.8
        omega = var(x, na.rm = TRUE) * (1 - alpha.start - beta.start)
        init.var = c(
            omega, 
            if (p > 0) rep(alpha.start/p, p),
            if (p > 0 & !symmetric) rep(0, p), 
            if (q > 0) rep(beta.start/q, q))
    }
    if (is.null(init.delta)) { 
        # Exponent:
        init.delta = 2
    }
    if (is.null(init.dist)) { 
        # Distributional Parameters:
        if (cond.dist == "dnorm") init.dist = NULL
        if (cond.dist == "dged") init.dist = c(nu = 2)
        if (cond.dist == "dstd") init.dist = c(nu = 4)
        if (cond.dist == "dsnorm") init.dist = c(xi = 1)
        if (cond.dist == "dsged") init.dist = c(nu = 2, xi = 1)  
        if (cond.dist == "dsstd") init.dist = c(nu = 4, xi = 1)    
    }
    params = c(init.mean, init.var, init.delta, init.dist)
    names(params) = Names
    if (.DEBUG) { cat("\nDEBUG - params: \n"); print(params) }
        
    # Which of the Parameters Should be Fixed?
    if (is.null(fix.mean)) 
        fix.mean = rep(FALSE, 1+u+v) else fix.mean = fix.mean
    if (is.null(fix.var)) 
        fix.var = rep(FALSE, 1+(1+!symmetric)*p+q) else fix.var = fix.var
    if (is.null(fix.delta)) {
        if (.series$model[2] == "garch") 
        	fix.delta = TRUE
        else 
        	fix.delta = FALSE 
	} else {
    	fix.delta = fix.delta
	}
    if (is.null(fix.dist)) {
        if (cond.dist == "dged") fix.dist = FALSE 
        if (cond.dist == "dstd") fix.dist = FALSE 
        if (cond.dist == "dsnorm") fix.dist = FALSE  
        if (cond.dist == "dsged") fix.dist = c(FALSE, FALSE)  
        if (cond.dist == "dsstd") fix.dist = c(FALSE, FALSE)    
	} else {
		fix.dist = fix.dist
	} 
	fixed = c(fix.mean, fix.var, fix.delta, fix.dist)
    names(fixed) = Names 
    if (.DEBUG) { cat("\nDEBUG - fixed: \n"); print(fixed) }
    
    # Index List of Parameters to be Optimized:
    index = (1:length(params))[fixed == FALSE]
    names(index) = names(params)[fixed == FALSE]
    if (.DEBUG) { cat("\nDEBUG - fixed: \n"); print(fixed) }
      
    # Trace:
    if (.trace) {
        cat("\n Initial Parameters:          $params")    
        cat("\n Limits of Transformations:   $U, $V ")
        cat("\n Which Parameters are Fixed?  $fixed \n\n")
        ans = data.frame(U, V, params, fixed)
        rownames(ans) = paste("   ", names(params))
        print(ans)
        cat("\nIndex List of Parameters to be Optimized:\n")
        print(index)
        cat("\n")
    }

    # Return Value:
    list(params = params, U = U, V = V, fixed = fixed, index = index,
        cond.dist = cond.dist, symmetric = symmetric)
}
    

# ------------------------------------------------------------------------------

      
.garchSetCondDist =
function(cond.dist = "dnorm") 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Select Conditional Density Function:

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
         .garchDist = function(z, hh, parm) {
            dnorm(x = z/hh, mean = 0, sd = 1) / hh 
        }
    }
    if (cond.dist == "dsnorm") { 
        .garchDist = function(z, hh, parm) {
            dsnorm(x = z/hh, mean = 0, sd = 1, xi = parm) / hh 
        }
    }
    
    # Standardized Student-t:
    if (cond.dist == "dstd") { 
        .garchDist = function(z, hh, parm) {
            dstd(x = z/hh, mean = 0, sd = 1, nu = parm) / hh
        }
    }
    if (cond.dist == "dsstd") { 
        .garchDist = function(z, hh, parm) {
            dsstd(x = z/hh, mean = 0, sd = 1, nu = parm[1], xi = parm[2]) / hh
        }
    }
      
    # Generalized Error Distribution:
    if (cond.dist == "dged") {
        .garchDist = function(z, hh, parm) {
            dged(x = z/hh, mean = 0, sd = 1, nu = parm) / hh
        }
    }
    if (cond.dist == "dsged") {
        .garchDist = function(z, hh, parm) {
            dsged(x = z/hh, mean = 0, sd = 1, nu = parm[1], xi = parm[2]) / hh
        }
    }
                       
    # Trace:
    if (.trace) {
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
    
    # Note:
    #   The variables '.series' and '.params' must be global available
    
    # FUNCTION:
    
    # DEBUG:
    .DEBUG = FALSE
    
    # Retrieve From Initialized Series:
    x = .series$x
    h = .series$h
    z = .series$z
    
    # Get Order:
    u = .series$order[1]
    v = .series$order[2]
    p = .series$order[3]
    q = .series$order[4]
    max.order = max(u, v, p, q)
    
    # Get Start Conditions:
    h.start = .series$h.start
    llh.start = .series$llh.start  
    
    # Retrieve From Initialized Parameters:
    cond.dist = .params$cond.dist
    Names = names(.params$params)
    names(params) = Names[.params$index]
    symmetric = .params$symmetric
         
    # Extracting the parameters by name ...
    mu = params[substr(Names, 1, 2) == "mu"]
    if (u > 0) ar = params[substr(Names, 1, 2) == "ar"]
    if (v > 0) ma = params[substr(Names, 1, 2) == "ma"]
    omega = params[substr(Names, 1, 5) == "omega"]
    if (p > 0) alpha = params[substr(Names, 1, 5) == "alpha"] 
    if (p > 0 & !symmetric) gamma = params[substr(Names, 1, 5) == "gamma"]
    if (q > 0) beta  = params[substr(Names, 1, 4) == "beta"] 
    delta = params[substr(Names, 1, 5) == "delta"]
    if (is.na(delta)) delta = 2
    nu = params["nu"]
    xi = params["xi"]
    dist = c(nu = nu, xi = xi)
    dist = dist[!is.na(dist)]
    
    # Iterate z: 
    N = length(x)
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
    
    # Initialize h and z:
    eps = z  
    deltainv = 1/delta
    # TODO: APARCH(p,q)
    h[1:(max.order)] = mean( abs(( .series$x[-(1:(max.order))]-mu ))^delta )
    eps[1:(max.order)] = (h[1:(max.order)])^deltainv
      
    # Iterate Conditional Variances h: 
    if (p == 0) { 
		alpha = 0 
        p = 1
    }
    if (q == 0) {
        beta = 0
        q = 1
    }
    if (FALSE) {
	    # Old and Slow Version for Testing, only:
	    if (symmetric) {
	        for (i in (h.start):N) {
	            h[i] = omega + 
	                sum(alpha * ( abs(eps[i-(1:p)])) ^ delta ) + 
	                sum(beta*h[i-(1:q)])  
	        }
	    } else {    
	        for (i in (h.start):N) {
	            h[i] = omega + 
	                sum(alpha * ( abs(eps[i-(1:p)]) - 
	                gamma * eps[i-(1:p)]) ^ delta ) + sum(beta*h[i-(1:q)]) 
	        }
	    } 
   	} else {
        # Note, sometimes one of the beta's can become undefined 
        # during optimization.
        beta[is.na(beta)] = 0.8/length(beta)
        if (symmetric) gamma = rep(0, length(p))
        pq = max(p, q)
	    edelta = abs((abs(eps) - gamma*eps))^delta
        edeltat = filter(edelta, filter = c(0, alpha), sides = 1)
        c = omega/(1-sum(beta))
        h = c( h[1:pq], c + filter(edeltat[-(1:pq)], filter = beta, 
            method = "recursive", init = h[pq:1]-c))
   	}
    
    # Save h and eps:
   	.series$h <<- h
    .series$z <<- eps
    
    # Calculate Log Likelihood:     
    hh = (abs(h[(llh.start):N]))^deltainv
    llh = -sum(log(.garchDist(z = z[(llh.start):N], hh = hh, parm = dist)))
    names(params) = names(.params$params[.params$index])
    if (is.na(llh)) llh = 1.0e99
    if (!is.finite(llh)) llh = 1.0e99
    
    # Print:
    if (llh < .llh) {
	    .llh <<- llh
	    cat("LLH:   ", llh, "\n")
	    # cat("DIST:  ", dist, "\n")
		print(params)
	}
    
    # Return Value:
    c(LogLikelihood = llh) 
}


# ------------------------------------------------------------------------------


.garchOptimizeLLH =
function(...) 
{
    # Description:
    #   Opimize Log-Likelihood Function
    
    # FUNCTION:
       
    # DEBUG:
    .DEBUG = FALSE
    
    # Initialization:
    index = .params$index
    if (.DEBUG) print(.params$params[index])
      
    # Optimize:
    if (.trace) cat("\nIteration Path:\n\n") 
    
    # Unconstrained Optimization:
    fit = nlminb(
        start = .params$params[index],
        objective = .garchLLH, 
        lower = .params$U[index],
        upper = .params$V[index], 
        control = list(iter.max = 5000, x.tol = 1e-12, trace = 0), ...)
  
    # Experimental - Don't use it:
    if (FALSE) {
	    fit = list()
	    fit$par = .params$params[index]
	    fit = optim(
	        par = fit$par,
	        fn = .garchLLH,
	        control = list(maxit = 5000))
	} 
	
	# Add Names:
    names(fit$par) = names(.params$params[index])    
    if (.trace)cat("\n\n")
    names(fit$par) = names(.params$params[index])
      
    # Coefficients:
    fit$coef = fit$par
    
    # Compute Hessian Matrix Numerically:
    eps = 0.0001 * fit$par
    n = length(fit$par)
    H = matrix(0, ncol = n, nrow = n)
    for (i in 1:n) {
        for (j in 1:n) {
            x1 = x2 = x3 = x4 = fit$par
            x1[i] = x1[i] + eps[i]
            x1[j] = x1[j] + eps[j]
            x2[i] = x2[i] + eps[i]
            x2[j] = x2[j] - eps[j]
            x3[i] = x3[i] - eps[i]
            x3[j] = x3[j] + eps[j]
            x4[i] = x4[i] - eps[i]
            x4[j] = x4[j] - eps[j]
            H[i, j] = ( 
                .garchLLH(x1) - .garchLLH(x2) -
                .garchLLH(x3) + .garchLLH(x4) ) / (4*eps[i]*eps[j])
        }
    }
    colnames(H) = rownames(H) = names(.params$params[index])
    fit$hessian = H
    
    # Print Hessian Matrix:
    if (.trace) {
        cat("\nHessian Matrix:\n")
        print(fit$hessian)
    }
 
    # Return Value:
    fit 
}

    
################################################################################
                

print.fGARCH = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print method for an object of class "fGARCH"
    
    # FUNCTION:
    
    # Check object:
     
    # Title:
    cat("\nTitle:\n ")
    cat(x@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n")
    
    # Mean Equation:
    cat("\nMean Equation:\n ")
    cat(as.character(x@formula[1]), "\n")
    
    # Conditional Variance Equation: 
    cat("\nConditional Variance Equation:\n ")
    cat(as.character(x@formula[2]), "\n")
        
    # Conditional Distribution:
    cat("\nConditional Distribution:\n ")
    cat(x@fit$params$cond.dist, "\n")
  
    # Errors of Coefficients:
    cat("\nCoefficient(s):\n")
    digits = max(4, getOption("digits") - 4)
    print.default(format(x@fit$par, digits = digits), print.gap = 2, 
        quote = FALSE)    
   
    # Description:
    cat("\nDescription:\n ")
    cat(x@description, "\n\n")

    # Return Value:
    cat("\n")
    invisible()
}


# ------------------------------------------------------------------------------


plot.fGARCH = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Plot method for an object of class "fGARCH"

    # FUNCTION:
        
    # Plot Time Series"
    plot(x@fit$series$x, type = "l", 
    	ylab = "Time Series", main = "Time Series", col = "steelblue")
    grid()
    
    # Residuals:
    plot(x@residuals, type = "l", 
    	ylab = "Residuals", main = "Residuals", col = "steelblue")
    grid()
    	
    # Conditional Variances:
    deltainv = 1/x@fit$delta
    condSD = x@fit$series$h^deltainv
    plot(condSD, type = "l", 
    	ylab = "Conditional SD", main = "Standard Deviations", 
    	col = "steelblue")
    	
    # Quantile-Quantile Plot: 
    qqnorm(x@residuals, pch = 19, cex = 0.5, col = "steelblue")
    qqline(x@residuals)
    grid()
    
    # Autocorrelation Functions: 
    acf(x@residuals, ylab = "Residuals", main = "Residuals")
    acf(x@residuals^2, ylab = "Error Variances", main = "Error Variances") 
  
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


summary.fGARCH = 
function(object, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Summary method for an object of class "fGARCH"

    # Requirements:
    #   Requires R's tseries Package!
    
    # FUNCTION:
    
	# Fit Slot:
    fit = object@fit
    
    # Residuals: 
    fit$cvar = solve(fit$hessian)
    fit$se.coef = sqrt(diag(fit$cvar))
    fit$tval = fit$coef/fit$se.coef
    fit$matcoef = cbind(fit$coef, fit$se.coef, 
        fit$tval, 2*(1-pnorm(abs(fit$tval))))
    dimnames(fit$matcoef) = list(names(fit$tval), c(" Estimate", 
        " Std. Error", " t value", "Pr(>|t|)"))
    
    # Tests:
    fit$jbtest = jarqueberaTest(object@residuals)
    fit$lbtest = Box.test(object@residuals^2, type = "Ljung-Box")
    
    # Add to object
    object@fit =  fit
    
    # Print Title:
    cat("\nTitle:\n ")
    cat(object@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), "\n")
    
    # Mean Equation:
    cat("\nMean Equation:\n ")
    cat(as.character(object@formula[1]), "\n")
    
    # Conditional Variance Equation: 
    cat("\nConditional Variance Equation:\n ")
    cat(as.character(object@formula[2]), "\n")
        
    # Conditional Distribution:
    cat("\nConditional Distribution:\n ")
    cat(object@fit$params$cond.dist, "\n")
    
    # Residuals:
    cat("\nResiduals:\n")
    digits = max(4, getOption("digits") - 4)
    rq = structure(quantile(object@residuals), names = c("Min", "1Q", 
        "Median", "3Q", "Max"))
    print(rq, digits = digits)
  
    # Coefficients:
    # cat("\nCoefficient(s):\n")
    # digits = max(4, getOption("digits") - 4)
    # print.default(format(object@fit$par, digits = digits), print.gap = 2, 
    #    quote = FALSE)
        
    # Coefficients and Errors of Coefficients:
    signif.stars = getOption("show.signif.stars")
    cat("\nCoefficient(s):\n")
    printCoefmat(fit$matcoef, digits = digits, 
        signif.stars = signif.stars) 
        
    # 1. Diagnostic Test:
    cat("\nJarque Bera Test of Residuals:\n")
    out1 = paste(" ",
        names(fit$jbtest@test$statistic), " = ", 
        format(round(fit$jbtest@test$statistic, 4)),
        ", ", sep = "")
    out2 = paste(
        "p-value =", 
        format.pval(fit$jbtest@test$p.value, digits = 4))
    cat(out1, out2, "\n")  
    
    # 2. Diagnostic Test:
    cat("\nLjung-Box Test of Squared Residuals:\n")
    out1 = paste(" ", 
    	names(fit$lbtest$statistic), " = ", 
        format(round(fit$lbtest$statistic, 4)), 
        ", ", sep="")
    out2 = paste("p-value =", format.pval(fit$lbtest$p.value, digits = 4))
    cat(out1, out2, "\n")
    
    # Return Value:
    invisible(object)
}


################################################################################

