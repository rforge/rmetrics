
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
# FUNCTION:             GEV SIMULATION:
#  gevSim                Simulates a GEV distributed process
#  gumbelSim             Simulates a Gumbel distributed process
# FUNCTION:             GEV PARAMETER ESTIMATION:
#  'fGEVFIT'             S4 class representation
#  gevFit                Fits Parameters of GEV distribution
#  gumbelFit             Fits Parameters of Gumbel distribution
#   .gumpwmFit            Fits Gumbel with probability weighted moments
#   .gevpwmFit            Fits GEV with probability weighted moments
#   .gummleFit            Fits Gumbel with max log-likelihood approach
#    .gumLLH               Computes Gumbel log-likelihood function
#   .gevmleFit            Fits GEV with max log-likelihood approach
#    .gevLLH               Computes GEV log-likelihood function
# METHODS:              PRINT, PLOT, AND SUMMARY:
#  show.fGEVFIT          S4 Show method for object of class "fGEVFIT"
#  plot.fGEVFIT          S3 Plot method for object of class "fGEVFIT"
#   .gev1Plot             Block Maxima Plot            
#   .gev2Plot             Scatterplot of Residuals
#   .gev3Plot             Histogram of Residuals
#   .gev4Plot             Quantile-Quantile Plot
#  summary.fGEVFIT       S3 Summary Method for object of class "fGEVFIT"
################################################################################


gevSim = 
function(model = list(xi = -0.25, mu = 0, beta = 1), n = 1000, seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates random variates from a GEV distribution
    
    # Arguments:
    
    # Examples:
    #   gevSim(n = 100)
    #   gevSim(n = 100, seed = 4711)
    #   gevSim(model = list(xi = -0.15, mu = 0, beta = 0.02))
    
    # FUNCTION:
    
    # Seed:
    if (is.null(seed)) seed = NA else set.seed(seed)
    
    # Simulate:
    ans = rgev(n = n, xi = model$xi, mu = model$mu, beta = model$beta)
    ans = as.ts(ans)
    
    # Control:
    attr(ans, "control") = 
        data.frame(t(unlist(model)), seed = seed, row.names = "control")
        
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


gumbelSim = 
function(model = list(mu = 0, beta = 1), n = 1000, seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates random variates from a GEV distribution
    
    # Arguments:
    
    # Examples:
    #   gumbelSim(n = 100)
    #   gumbelSim(n = 100, seed = 4711)
    
    # FUNCTION:
    
    # Simulate:
    ans = gevSim(model = list(xi = 0, mu = model$mu, beta = model$beta), 
        n = n, seed = seed)
        
    # Return Value:
    ans 
}


################################################################################


setClass("fGEVFIT", 
    representation(
        call = "call",
        method = "character",
        parameter = "list",
        data = "list",
        fit = "list",
        residuals = "numeric",
        title = "character",
        description = "character"
    )  
)


# ------------------------------------------------------------------------------
    

gevFit =
function(x, block = 1, type = c("mle", "pwm"), 
title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    
    # Examples:
    
    # FUNCTION:
    
    # Match Call:
    call = match.call()
    
    # Match Arguments:
    type = match.arg(type)
    
    # Fit:
    ans = .gevFit(x = x, block = block, type = type, gumbel = FALSE, 
        title = title, description = description, ...)
    ans@call = call
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


gumbelFit =
function(x, block = 1, type = c("mle", "pwm"), 
title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    
    # Examples:
    
    # FUNCTION:
    
    # Match Call:
    call = match.call()
    
    # Match Arguments:
    type = match.arg(type)
    
    # Fit:
    ans = .gevFit(x = x, block = block, type = type, gumbel = TRUE, 
        title = title, description = description, ...)
    ans@call =  call
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.gevFit =
function(x, block = 1, type = c("mle", "pwm"), 
gumbel = FALSE, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits parameters to a GEV distribution
    
    # Arguments:
    #   x - a numeric vector of Block Maxima
    
    # Examples:
    #   fit = gevFit(gevSim(), type = "mle", gumbel = FALSE); print(fit)
    #   fit = gevFit(gevSim(), type = "pwm", gumbel = FALSE); print(fit)
    #   fit = gevFit(gevSim(), type = "mle", gumbel = TRUE); print(fit)
    #   fit = gevFit(gevSim(), type = "pwm", gumbel = TRUE); print(fit)
    #   x = rnorm(500);  block = 20; type="mle"; gumbel=FALSE
    #   x = as.ts(rnorm(500));  block = 20; type = "mle"; gumbel = FALSE
    #   x = dummyDailySeries(rnorm(500)); block = 20; type = "mle"; gumbel=FALSE
    
    # Note:
    #   Argument named "method is already used for the selection
    #   of the MLE optimization algorithm, therfore we use here
    #   "type".
    
    # FUNCTION:
    
    # Match Call:
    call = match.call()
    
    # Match Arguments:
    type = match.arg(type)
    
    # Check Type and Convert:
    X = x
    xClass = class(x)
    x = as.timeSeries(x)
    stopifnot(isUnivariate(x))
    
    # Block Maxima:
    if (is.numeric(block)) {
        if (block == 1) {
            blockmaxima = x
            Names = paste(1:dim(blockmaxima)[1])
        } else {
            blockmaxima = blockMaxima(x, block, doplot = FALSE)
            Names = blockmaxima@recordIDs[, 3]
        }
    } else {
        blockmaxima = blockMaxima(x, block, doplot = FALSE)
        Names = rownames(blockmaxima@Data)
    }

    if (xClass == "numeric") {
        blockmaxima = as.vector(blockmaxima)
        names(blockmaxima) = Names
    }
    if (xClass == "ts") {
        blockmaxima = as.ts(blockmaxima)
        names(blockmaxima) = Names
    }
    x = as.vector(blockmaxima)
    
    # Estimate Parameters:
    if (gumbel) {   
        # GUMBEL: Add Call and Type
        if (length(type) > 1) type = type
        # Probability Weighted Moment Estimation:
        if (type == "pwm") {
            fit = .gumpwmFit(data = x, ...) 
        }
        # Maximum Log Likelihood Estimation:
        # Use Alexander McNeils EVIS from evir Package ...
        if (type == "mle") { 
            fit = .gummleFit(data = x, ...) 
        } 
    } else {
        # GEV: Add Call and Type
        if (length(type) > 1) type = type
        # Probability Weighted Moment Estimation:
        if (type == "pwm") { 
            fit = .gevpwmFit(data = x, ...) 
        }
        # Maximum Log Likelihood Estimation:
        # Use Alexander McNeils EVIS from evir Package ...
        if (type == "mle") { 
            fit = .gevmleFit(data = x, ...) 
        }    
    }
    class(fit) = "list"
            
    # Compute Residuals:
    if (gumbel) {
        # GUMBEL:
        xi = 0
        beta = fit$par.ests["xi"]
        mu = fit$par.ests["mu"] 
        residuals = exp( - exp( - (x - mu)/beta)) 
    } else {
        # GEV:
        xi = fit$par.ests["xi"]
        beta = fit$par.ests["beta"]
        mu = fit$par.ests["mu"]
        residuals = (1 + (xi * (x - mu))/beta)^(-1/xi) 
    }  
        
    # Make Unique:
    fit$llh = fit$nllh.final
    
    # Add title and description:
    if (is.null(title)) {
        if (gumbel) {
            title = "Gumbel Parameter Estimation"
        } else {
            title = "GEV Parameter Estimation"
        }
    }
    if (is.null(description)) {
        description = as.character(date())
    }
    
    # Add Counts to x:
    
    # Return Value:
    new("fGEVFIT",
        call = match.call(),
        method = c(if (gumbel) "gum" else "gev", type[1]),
        parameter = list(block = block, type = type[1], gumbel = gumbel),
        data = list(x = X, blockmaxima = blockmaxima),
        fit = fit,
        residuals = residuals,
        title = title,
        description = description)
}


# ------------------------------------------------------------------------------


.gumpwmFit = 
function(data, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description # FUNCTION:
    
    # Arguments:
    
    # FUNCTION:
    
    # "Probability Weighted Moment" method.
    data = as.numeric(data)
    n = length(data)
    
    # Sample Moments:
    x = rev(sort(data))
    lambda = c(mean(x), 0)
    for (i in 1:n) {
        weight = (n-i)/(n-1)/n
        lambda[2] = lambda[2] + weight*x[i] 
    } 
        
    # Calculate Parameters:
    xi = 0
    beta = lambda[2]/log(2)
    mu = lambda[1] - 0.5772*beta
    
    # Output:
    fit = list(
        n = n, 
        data = data, 
        par.ests = c(mu = mu, beta = beta), 
        par.ses = c(mu = NA, beta = NA),
        varcov = matrix(rep(NA, 4), 2, 2), 
        converged = NA, 
        nllh.final = NA, 
        call = match.call(), 
        selected = "pwm")
    class(fit) = "gev" # not gumbel!
    
    # Return Value:
    fit 
}


# ------------------------------------------------------------------------------


.gevpwmFit = 
function(data, block = NA, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Probability Weighted Moment method.
    data = as.numeric(data)
    n = length(data)    
    
    # Internal Function:
    y = function(x, w0, w1, w2) { 
        (3^x-1)/(2^x-1) - (3*w2 - w0)/(2*w1 - w0) 
    }       
   
    # Moments:
    nmom = 3
    x = rev(sort(data))
    moments = rep(0, nmom)
    moments[1] = mean(x)
    n = length(x)
    for (i in 1:n) {
        weight = 1/n
        for (j in 2:nmom) {
            weight = weight*(n-i-j+2)/(n-j+1)
            moments[j] = moments[j] + weight*x[i] 
        } 
    }
    w0 = moments[1]
    w1 = moments[2]
    w2 = moments[3]
    
    # Parameters:      
    xi = uniroot(f = y, interval = c(-5,+5), 
        w0 = w0, w1 = w1, w2 = w2)$root
    beta = (2*w1-w0)*xi / gamma(1-xi) / (2^xi-1)
    mu = w0 + beta*(1-gamma(1-xi))/xi
    
    # Output:
    fit = list(
        n = n, 
        data = data, 
        par.ests = c(xi = xi, mu = mu, beta = beta), 
        par.ses = c(xi = NA, mu = NA, beta = NA),
        varcov = matrix(rep(NA, 9), 3, 3), 
        converged = NA, 
        nllh.final = NA, 
        call = match.call(), 
        selected = "pwm")
    class(fit) = "gev"  
    
    # Return Value:
    fit 
}

    
# ------------------------------------------------------------------------------


.gummleFit =
function(data, block = NA, ...)
{   # A copy from evir

    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Data:
    data = as.numeric(data)
    n = length(data)
    
    # Generate EVIR Start Values:
    # beta0 = sqrt(6 * var(data))/pi
    # mu0 = mean(data) - 0.57722 * beta0
    # theta = c(mu = mu0, beta = beta0)  
    # We use PWM Start Values:
    theta = .gumpwmFit(data)$par.ests
    
    # Fit:
    fit = optim(theta, .gumLLH, hessian = TRUE, ..., tmp = data)
    if( fit$convergence) warning("optimization may not have succeeded")
    par.ests = fit$par
    varcov = solve(fit$hessian)
    par.ses = sqrt(diag(varcov))
    
    # Result:
    ans = list(
        n = n, 
        data = data,  
        par.ests = par.ests, 
        par.ses = par.ses, 
        varcov = varcov, 
        converged = fit$convergence, 
        nllh.final = fit$value)
    class(ans) = "gev"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.gumLLH = 
function(theta, tmp)
{   # A copy from evir

    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Gumbel Log-Likelihood:
    y = (tmp - theta[1])/theta[2]
    if(theta[2] < 0) {
        ans = 1.0e+6
    } else {
        term1 = length(tmp) * logb(theta[2])
        term2 = sum(y)
        term3 = sum(exp( - y))
        ans = term1 + term2 + term3
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.gevmleFit =
function(data, block = NA, ...)
{   # A copy from evir

    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Data:
    data = as.numeric(data)
    n = length(data)
    
    # EVIR Start Values:
    beta0 = sqrt(6 * var(data))/pi
    mu0 = mean(data) - 0.57722 * beta0
    xi0 = 0.1
    
    # We use PWM Start Values:
    theta = .gevpwmFit(data)$par.ests
    
    # Fit:
    fit = optim(theta, .gevLLH, hessian = TRUE, ..., tmp = data)
    if (fit$convergence) warning("optimization may not have succeeded")
    par.ests = fit$par
    varcov = solve(fit$hessian)
    par.ses = sqrt(diag(varcov))
    
    # Result:
    ans = list(
        n = n, 
        data = data,
        par.ests = par.ests, 
        par.ses = par.ses, 
        varcov = varcov, 
        converged = fit$convergence, 
        nllh.final = fit$value)
    class(ans) = "gev"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.gevLLH = 
function(theta, tmp)
{   # A copy from evir

    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # GEV Log-likelihood:
    y = 1 + (theta[1] * (tmp - theta[2]))/theta[3]
    if((theta[3] < 0) || (min(y) < 0)) {
        ans = 1e+06
    } else {
        term1 = length(tmp) * logb(theta[3])
        term2 = sum((1 + 1/theta[1]) * logb(y))
        term3 = sum(y^(-1/theta[1]))
        ans = term1 + term2 + term3
    }
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


show.fGEVFIT = 
function(object) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for an object of class "gevFit".
    
    # Arguments:
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n" , object@title, "\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", 
        collapse = "\n"), "\n", sep = "")
            
    # Estimation Type:
    cat("\nEstimation Type:\n ", object@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(object@fit$par.ests)
    
    # Desription:
    cat("\nDescription\n ", object@description, "\n\n")
    
    # Return Value:
    invisible(object)
}
    

setMethod("show", "fGEVFIT", show.fGEVFIT)


# ------------------------------------------------------------------------------


plot.fGEVFIT =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "gevFit".
    
    # Arguments:
    
    # Details:
    #   plot.gev:
    #   Data are converted to unit exponentially distributed residuals
    #   under null hypothesis that GEV fits. Two diagnostics for iid
    #   exponential data are offered:
    #   "Scatterplot of Residuals" and "QQplot of Residuals"
    
    # Example:
    #   fit = gevFit(gevSim(), type = "mle", gumbel = FALSE)
    #   par(mfrow = c(2, 2)); plot(fit)
    #   par(mfrow = c(1, 1)); plot(fit, which = "ask")
    #
    #   fit = gevFit(gevSim(), type = "mle", gumbel = TRUE)
    #   par(mfrow = c(1, 1)); plot(fit, which = "ask")
    #
    #   fit = gevFit(gevSim(), type = "pwm", gumbel = FALSE)
    #   par(mfrow = c(1, 1)); plot(fit, which = "ask")
    #
    #   fit = gevFit(gevSim(), type = "pwm", gumbel = TRUE)
    #   par(mfrow = c(1, 1)); plot(fit, which = "ask")

    # FUNCTION: 
          
    # Plot:
    interactivePlot(
        x = x,
        choices = c(
            "Block Maxima Plot", 
            "Scatterplot of Residuals", 
            "Histogram of Residuals",
            "Quantile Quantile Plot"),
        plotFUN = c(
            ".gev1Plot", 
            ".gev2Plot",
            ".gev3Plot", 
            ".gev4Plot"),
        which = which)
                    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


.gev1Plot =
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Time Series Plot of Block Maxima
    
    # Arguments:
    
    # FUNCTION:
    
    # Data:
    data = x@data$blockmaxima
    
    # Labels:
    if (labels) {
        main = "Block Maxima"
        xlab = "Index"
        ylab = "Data"
    } else {
        main = xlab = ylab = ""
    }
        
    # Plot:
    plot(data, type = "h", 
        main = main, xlab = xlab, ylab = ylab,
        col = "steelblue", ...) 
        
    # Add Grid:
    if (labels) grid()
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.gev2Plot =
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Scatterplot of Residuals:
    
    # Arguments:
    
    # FUNCTION:
    
    # Data:
    residuals = x@residuals
    
    # Labels:
    if (labels) {
        main = "Scatterplot of Residuals"
        xlab = "Ordering"
        ylab = "Residuals"
    } else {
        main = xlab = ylab = ""
    }
        
    # Plot:
    plot(residuals, 
        main = main, xlab = xlab, ylab = ylab,
        pch = 19, col = "steelblue", ...) 
    lines(lowess(1:length(residuals), residuals), col = "brown")
    
    # Add Grid:
    if (labels) grid()
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.gev3Plot =
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Histogram Plot of Residuals with Gaussian Fit:
    
    # Arguments:
    
    # FUNCTION:
    
    # Data:
    residuals = x@residuals
    
    # Labels:
    if (labels) {
        if (x@method[1] == "gev") {
            dist = "GEV" 
        } else if (x@method[1] == "gum") {
            dist = "Gumbel"
        }
        main = paste(dist, "Residual Histogram")
        xlab = "Residuals"
        ylab = "Density"
    } else {
        main = xlab = ylab = ""
    }
        
    # Plot:
    hist(residuals, probability = TRUE, breaks = "FD",
        main = main, xlab = xlab, ylab = ylab,
        col = "steelblue", border = "white", ...)
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.gev4Plot =
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Quantile-Quantile Plot:
    
    # Arguments:
    
    # FUNCTION:
    
    # Data:
    data = x@residuals
    sorted = sort(data)
    y <- qexp(ppoints(data))
    
    # Labels:
    if (labels) {
        main = "QQ Plot of Residuals"
        xlab = "Ordered Data"
        ylab = "Exponential Quantiles"
    } else {
        main = xlab = ylab = ""
    }
        
    # Plot:ata, type = "h", 
    plot(sorted, y, 
        main = main, xlab = xlab, ylab = ylab,
        pch = 19, col = "steelblue", ...) 
    abline(lsfit(sorted, y))  
    
    # Add Grid:
    if (labels) grid()
    
    # Return Value:
    invisible()
}    


# ------------------------------------------------------------------------------
  

summary.fGEVFIT =
function(object, doplot = TRUE, which = "all", ...) 
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Summary method for an object of class "gevFit".

    # Arguments:
    
    # Example:
    #   fit = gevFit(gevSim(), type = "mle", gumbel = FALSE)
    #   par(mfrow = c(2, 2)); summary(fit)
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n", object@title, "\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", 
        collapse = "\n"), "\n", sep = "")
            
    # Estimation Type:
    cat("\nEstimation Type:\n ", object@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(object@fit$par.ests)
    
    # Summary:
    if (object@method[2] == "mle") {
        cat("\nStandard Deviations:\n "); 
        print(object@fit$par.ses)
        cat("\nLog-Likelihood Value:\n ", object@fit$llh, "\n")
        cat("\nType of Convergence:\n ", object@fit$converged, "\n") } 
    
    # Desription:
    cat("\nDescription\n ", object@description, "\n\n")
    
    # Plot:
    if (doplot) {
        plot(object, which = which, ...)
    }
    
    # Return Value:
    invisible(object)
}


################################################################################

