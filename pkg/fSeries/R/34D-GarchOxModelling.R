
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA




################################################################################
# FUNCTION:					DESCRIPTION:
#  garchOxFit				 Fits parameters of a garch model			
#  print.garchOx			 S3 Print Method
#  summary.garchOx			 S3 Summary Method
#  plot.garchOx				 S3 Plot Method
################################################################################


garchOxFit = 
function(formula.mean = ~ arma(0, 0), formula.var = ~ garch(1, 1), 
series = x, cond.dist = c("gaussian", "t", "ged", "skewed-t"), 
include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL,
description = NULL)
{	# A function implemented by Diethelm Wuertz
	
	# Description:
	#	A simple interface to fit a time series by an ARMA-GARCH model.
	
	# Arguments:
	
	# FUNCTION:
	
	# Fit:
	fit = list()
	fit$x = series
	
	# Include Constants:
	include.var = TRUE
	fit$csts = c(include.mean, include.var)	
	
	# Select Distribution:
	# 0 : Gaussian
	# 1 : Student-t
	# 2 : GED
	# 3 : Skewed-Student-t
	distris = 0:3
	names(distris) = c("gaussian", "t", "ged", "skewed-t")
	distri = distris[cond.dist[1]]
	fit$cond.dist = cond.dist[1]
	
	# Determine ARMA Order:
	if (missing(formula.mean)) {
		# if missing use ARMA(0, 0) ...
		fit$formula.mean = ~ arma(0, 0)
		fit$arma.orders = c(0, 0) 
	} else {
	   	# otherwise determine orders "u" and "v" ...
		fit$arma.orders = as.numeric(strsplit(strsplit(strsplit(
			as.character(formula.mean), 
			"\\(")[[2]][2], "\\)")[[1]], ",")[[1]]) 
	}	
	
	# ARFIMA wanted?
	arfima = FALSE
	fit$arfima = as.integer(arfima)
	
	# Determine GARCH Order:
	arch = substring(as.character(formula.var)[2], 1, 4)
	if (arch == "arch") {
		arch.order = as.numeric(strsplit(strsplit(
			as.character(formula.var)[2], "\\(")[[1]][2], "\\)")[[1]])
		formula.var = as.formula(paste("~garch(0,", arch.order, ")", sep = ""))
	}	
	if (missing(formula.var)) {
		# if missing use GARCH(1, 1) ...
		fit$formula.var = ~ garch(1, 1)
		fit$garch.orders = c(1, 1) 
	} else {
		# otherwise determine orders "p" and "q" ...
		fit$garch.orders = as.numeric(strsplit(strsplit(strsplit(
		as.character(formula.var), 
			"\\(")[[2]][2], "\\)")[[1]], ",")[[1]]) 
		
	}
	# Note: We use GARCH(p,q) order with alpha(p) and beta(q) !!!
	# DW: 2005-05-16
	# fit$garch.orders = rev(fit$garch.orders)	
	
	# ARCH-IN-MEAN ?
	arch.in.mean = 0
	fit$arch.in.mean = arch.in.mean
	
	# Selected Model:
	models = 1:11
	names(models) = c("garch", "egarch", "gjr", "aparch", "igarch", 
		"figarch.bbm", "figarch.chung", "fiegarch", "fiaparch.bbm", 
		"fiaparch.chung", "hygarch")
	selected = strsplit(as.character(formula.var), "\\(")[[2]][1]
	fit$model = models[selected]	
	
	# Length of Time Series:
	nt = length(series)
	
	# Temporary File:
	ident = paste(selected, as.character(floor(runif(1)*10000)), sep = "")
											
	# Write parameters to file - OxParameter.txt:
	parameters = c(csts = fit$csts, distri = distri, arma = fit$arma.orders, 
		arfima = fit$arfima, garch = fit$garch.orders, model = fit$model, 
		inmean = fit$arch.in.mean, trunc = truncation, nt = nt)	
	write(x = parameters, file = "OxParameter.txt")	
	
	# Write data to file - OxSeries:
	write(x = "X", file = "OxSeries.csv", ncolumns = 1)
	write(x, file = "OxSeries.csv", ncolumns = 1, append = TRUE)						
	
	# Calculate:	
	command = "C:\\Ox\\bin\\oxl.exe C:\\Ox\\lib\\GarchOxModelling.ox"
	fit$ox = system(command, show.output.on.console = trace, invisible = TRUE)
	fit$model = selected
	fit$call = match.call()
	fit$residuals = scan("OxResiduals.csv", skip = 1, quiet = TRUE)
	fit$condvars = scan("OxCondVars.csv", skip = 1, quiet = TRUE)
	fit$coef = matrix(scan("OxParameters.csv", skip = 1, quiet = TRUE), 
	  	byrow = TRUE, ncol = 3)
	  		  	
	# Add Title and Description:
	fit$title = title
	if (is.null(title)) fit$title = "GARCH Ox Modelling"
	fit$description = description
	if (is.null(description)) fit$description = as.character(date())
		
	# Return Value:
	class(fit) = "garchOx"
	fit
}


# ------------------------------------------------------------------------------


print.garchOx =  
function(x, digits = max(3, getOption("digits") - 3), ...) 
{	# A function implemented by Diethelm Wuertz
    
	# Description:
	#	Print method for an object of class "garchOx".
	
	# FUNCTION:
	
	# Check object:
	object = x
	if (!inherits(object, "garchOx")) 
        stop("method is only for garchOx objects")
    
    # Call:
    cat("\nTitle:\n")
    cat(object$title, "\n")
    
    # Note: We use GARCH(p,q) order with alpha(p) and beta(q) !!!
	# DW: 2005-05-16 
    # object$garch.orders = rev(object$garch.orders)
    
    # Mean and variance Equation:
    cat("\nMean Equation:\n")
    cat(" ~arma(", object$arma.orders[1], ", ",
    	object$arma.orders[2], ")\n", sep = "")
    cat("\nConditional Variance Equation:\n")
    cat(" ~", object$model, "(", object$garch.orders[1], ", ", 
    	object$garch.orders[2], ")\n", sep = "")
    	
    # Conditional Distribution:
    cat("\nConditional Distribution:\n")
    cat(" ", object$cond.dist, "\n", sep = "")
    
    # Coefficients:
    cat("\nCoefficient(s):\n")
    Value = object$coef[, 1]
    Std.Error = object$coef[, 2]
    t.value = object$coef[, 3]
    coef.names = NULL
    
    # Constant mu:
    if (object$csts[1]) {
    	coef.names = c(coef.names, "Cst(M)")
	}
    
	# ARFIMA:
    if(object$arfima == 1) {
	    coef.names = c(coef.names, "d-arfima")
    }
    
    # AR:
    if (object$arma.orders[1] > 0) {
    	for (i in 1:object$arma.orders[1])
    	coef.names = c(coef.names, paste("AR(", as.character(i), ")", 
    		sep = ""))
   	}
   	
   	# MA:
    if (object$arma.orders[2] > 0) {
    	for (i in 1:object$arma.orders[2])
    	coef.names = c(coef.names, paste("MA(", as.character(i), ")", 
    		sep = ""))
   	}
    
   	# Constant Omega:
   	if (object$csts[2]) {
    	coef.names = c(coef.names, "Cst(V)")
	}
    
    # ARCH:
    if (object$garch.orders[1] > 0) {
    	for (i in 1:object$garch.orders[1])
    	coef.names = c(coef.names, paste("ARCH(", as.character(i), ")", 
    		sep = ""))
    }
    
    # GARCH:
    if (object$garch.orders[2] > 0) {
    	for (i in 1:object$garch.orders[2])
    	coef.names = c(coef.names, paste("GARCH(", as.character(i), ")", 
    		sep = ""))
   	}
   	
   	# EGARCH:
   	if (object$model == "egarch") {
	   	if (object$garch.orders[1] + object$garch.orders[2] > 0) {
	    	for (i in 1:(object$garch.orders[1]+object$garch.orders[1]))
	    	coef.names = c(coef.names, paste("EGARCH(", as.character(i), ")", 
	    		sep = ""))
	    }
   	}
   	  	
   	# Model GJR:
   	if (object$model == "gjr") {
	   	if (object$garch.orders[2] > 0) {
	    	for (i in 1:object$garch.orders[1])
	    	coef.names = c(coef.names, paste("GJR(", as.character(i), ")", 
	    		sep = ""))
	    }
   	}
   	
   	# Model APARCH:
   	if (object$model == "aparch") {
	   	if (object$garch.orders[2] > 0) {
	    	for (i in 1:object$garch.orders[1])
	    	coef.names = c(coef.names, paste("APARCH(", as.character(i), ")", 
	    		sep = ""))
	    }
   	}
   	
   	# Model APARCH:
   	if (object$model == "aparch") {
	   	if (object$garch.orders[2] > 0) {
	    	for (i in 1:object$garch.orders[1])
	    	coef.names = c(coef.names, paste("DELTA(", as.character(i), ")", 
	    		sep = ""))
	    }
   	}
   	  	
   	# Student-t	
    if (object$cond.dist == "t") {
	    coef.names = c(coef.names, "Student(DF)")
	}
	
	# GED:
    if (object$cond.dist == "ged") {
	    coef.names = c(coef.names, "GED(DF)")
    }
    
    # Skewed Student-t:
    if (object$cond.dist == "skewed-t") {
    	coef.names = c(coef.names, "Asymmetry", "Tail")  
    }
    
    # -in-mean:
    if (object$arch.in.mean == 1) {
    	coef.names = c(coef.names, "ARCH-in-mean(var)") 
    }
    
    coef = data.frame(cbind(Value, Std.Error, t.value), row.names = coef.names)
    print(coef)		
    cat("\n")
      
    # return Value:
    invisible()
}


# ------------------------------------------------------------------------------


summary.garchOx = 
function(object, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Summary method for an object of class "garchOx".
	
	# FUNCTION:
	
	# Check object:
	if (!inherits(object, "garchOx")) 
        stop("method is only for garchOx objects")
    
    print(object)
    plot(object)

    # Return Value:
    invisible(object)
}


# ------------------------------------------------------------------------------


plot.garchOx = 
function(x, ...) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Plot method for an object of class "garchOx".
	
	# FUNCTION:
	
	# Check Object:
	object = x
	if (!inherits(object, "garchOx")) 
        stop("method is only for garchOx objects")
        
    # Plot Time Series"
    plot(object$x, type = "l", main = "Time Series")
    
    # Conditional Variances:
    plot(object$condvars, type = "l", main = "Conditional Variances")
    
    # Autocorrelation Functions: 
    acf(object$x)
    acf(object$x^2)
    
    # CCF   
}


# ------------------------------------------------------------------------------

	