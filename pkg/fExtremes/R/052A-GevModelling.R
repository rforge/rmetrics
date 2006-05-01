
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
# FUNCTION:             MLE AND PWM ESTIMATORS:
#  gevSim                Simulates GEV including Gumbel rvs [EVIS/EVIR]
#  fGEVFIT               S4 class representation
#  gevFit                Fits Parameters of GEV distribution
#   .gumpwmFit             Gumbel with probability weighted moments
#   .gevpwmFit             GEV with probability weighted moments
#   .sam.pwm                Computes sample weights
#   .gummleFit             Gumbel with max log-likelihood approach
#   .gumLLH                Gumbel log-likelihood function
#   .gevmleFit             GEV with max log-likelihood approach
#   .gevLLH                GEV log-likelihood function
#   print.fGEVFIT            Print Method for object of class "gevFit"
#   plot.fGEVFIT             Plot Method for object of class "gevFit"
#   .gev1Plot
#   .gev2Plot
#   .gev3Plot
#   .gev4Plot
#   summary.fGEVFIT          Summary Method for object of class "gevFit"
#  gevrlevelPlot         Calculates Return Levels Based on GEV Fit
#  .rlevel.gev.evir       Internal Function
################################################################################
# FUNCTION:             MDA ESTIMATORS:
#  hillPlot              Plot Hill's estimator
#  shaparmPlot           Pickands, Hill & Decker-Einmahl-deHaan Estimator
#   shaparmPickands      Auxiliary function called by shaparmPlot
#   shaparmHill           ... called by shaparmPlot
#   shaparmDehaan         ... called by shaparmPlot
################################################################################


gevSim = 
function(model = list(shape = -0.25, location = 0, scale = 1), 
n = 1000, seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates random variates from a GEV distribution
    
    # Examples:
    #   gevSim(n = 100)
    #   gevSim(n = 100, seed = 4711)
    #   gevSim(model = list(shape = -0.15, location = 0, scale = 0.02))
    
    # FUNCTION:
    
    # Seed:
    if (is.null(seed)) seed = NA else set.seed(seed)
    
    # Simulate:
    ans = rgev(n = n, xi = model$shape, mu = model$location, 
        sigma = model$scale)
    ans = as.ts(ans)
    
    # Control:
    attr(ans, "control") = 
        data.frame(t(unlist(model)), seed = seed, row.names = "control")
        
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


gumbelSim = 
function(model = list(location = 0, scale = 1), 
n = 1000, seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates random variates from a GEV distribution
    
    # Examples:
    #   .gumbelSim(n = 100)
    #   .gumbelSim(n = 100, seed = 4711)
    
    # FUNCTION:
    
    # Seed:
    if (is.null(seed)) seed = NA else set.seed(seed)
    
    # Simulate:
    ans = rgev(n = n, xi = 0, mu = model$location, 
        sigma = model$scale)
    ans = as.ts(ans)
    
    # Control:
    attr(ans, "control") = 
        data.frame(t(unlist(model)), seed = seed, row.names = "control")
        
    # Return Value:
    ans 
}


################################################################################
# Class Representation


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
{
    # Fit:
    ans = .gevFit(x, block, type, gumbel = FALSE, title, description, ...)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


gumbelFit =
function(x, block = 1, type = c("mle", "pwm"), 
title = NULL, description = NULL, ...)
{
    # Fit:
    ans = .gevFit(x, block, type, gumbel = TRUE, title, description, ...)
    
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
    
    #   x=rnorm(500);  block=20; type="mle"; gumbel=FALSE
    #   x=as.ts(rnorm(500));  block=20; type="mle"; gumbel=FALSE
    #   x = dummyDailySeries(rnorm(500)); block=20; type="mle"; gumbel=FALSE
    
    # Note:
    #   Argument named "method is already used for the selection
    #   of the MLE optimization algorithm, therfore we use here
    #   "type".
    
    # FUNCTION:
    
    # Settings:
    call = match.call()
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
        sigma = fit$par.ests[1]
        mu = fit$par.ests[2] 
        residuals = exp( - exp( - (x - mu)/sigma)) 
    } else {
        # GEV:
        xi = fit$par.ests[1]
        sigma = fit$par.ests[2]
        mu = fit$par.ests[3]
        residuals = (1 + (xi * (x - mu))/sigma)^(-1/xi) 
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
    sigma = beta = lambda[2]/log(2)
    mu = lambda[1] - 0.5772*beta
    
    # Output:
    fit = list(n = n, data = data, 
        par.ests = c(sigma, mu), par.ses = rep(NA, 2),
        varcov = matrix(rep(NA, 4), 2, 2), converged = NA, 
        nllh.final = NA, call = match.call(), selected = "pwm")
    names(fit$par.ests) = c("sigma", "mu")
    names(fit$par.ses) = c("sigma", "mu")
    class(fit) = "gev" # not gumbel!
    
    # Return Value:
    fit 
}


# ------------------------------------------------------------------------------


.gevpwmFit = 
function(data, block = NA, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Probability Weighted Moment method.
    data = as.numeric(data)
    n = length(data)    
    
    # Internal Function:
    y = function(x, w0, w1, w2) { 
        (3^x-1)/(2^x-1) - (3*w2 - w0)/(2*w1 - w0) 
    }       
    
    # Calculate:
    w = .sam.pwm(data, nmom = 3)
    w0 = w[1]
    w1 = w[2]
    w2 = w[3]      
    xi = uniroot(f = y, interval = c(-5,+5), 
        w0 = w[1], w1 = w[2], w2 = w[3])$root
    sigma = beta = (2*w1-w0)*xi / gamma(1-xi) / (2^xi-1)
    mu = w0 + beta*(1-gamma(1-xi))/xi
    
    # Output:
    fit = list(n = n, data = data, 
        par.ests = c(xi, sigma, mu), par.ses = rep(NA, 3),
        varcov = matrix(rep(NA, 9), 3, 3), converged = NA, 
        nllh.final = NA, call=match.call(), selected = "pwm")
    names(fit$par.ests) = c("xi", "sigma", "mu")
    names(fit$par.ses) = c("xi", "sigma", "mu")
    class(fit) = "gev"  
    
    # Return Value:
    fit 
}


# ------------------------------------------------------------------------------


.sam.pwm = 
function(x, nmom) 
{   # A function implemented by Diethelm Wuertz

    # Description
    #   Computes sample probability weighted moments
    
    # FUNCTION:
    
    # a = 0, b = 0, kind = 1
    x = rev(sort(x))
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
    
    # Return Value:
    return(moments) 
}
    
    
# ------------------------------------------------------------------------------


.gummleFit =
function(data, block = NA, ...)
{   # A copy from evir

    data = as.numeric(data)
    n = length(data)
    sigma0 = sqrt(6 * var(data))/pi
    mu0 = mean(data) - 0.57722 * sigma0
    theta = c(sigma0, mu0)
    
    fit = optim(theta, .gumLLH, hessian = TRUE, ..., tmp = data)
    if( fit$convergence) {
        warning("optimization may not have succeeded")
    }
    par.ests = fit$par
    varcov = solve(fit$hessian)
    par.ses = sqrt(diag(varcov))
    out = list(n = n, data = data,  
        par.ests = par.ests, par.ses = par.ses, varcov = varcov, 
        converged = fit$convergence, nllh.final = fit$value)
    names(out$par.ests) = c("sigma", "mu")
    names(out$par.ses) = c("sigma", "mu")
    class(out) = "gev"
    
    # Return Value:
    out
}


# ------------------------------------------------------------------------------


.gumLLH = 
function(theta, tmp)
{   # A copy from evir

    y = (tmp - theta[2])/theta[1]
    if(theta[1] < 0) {
        out = 1.0e+6
    } else {
        term1 = length(tmp) * logb(theta[1])
        term2 = sum(y)
        term3 = sum(exp( - y))
        out = term1 + term2 + term3
    }
    
    # Return Value:
    out
}

# ------------------------------------------------------------------------------


.gevmleFit =
function(data, block = NA, ...)
{   # A copy from evir

    data = as.numeric(data)
    n = length(data)
    sigma0 = sqrt(6 * var(data))/pi
    mu0 = mean(data) - 0.57722 * sigma0
    xi0 = 0.1
    theta = c(xi0, sigma0, mu0)
    fit = optim(theta, .gevLLH, hessian = TRUE, ..., tmp = data)
    if (fit$convergence) {
        warning("optimization may not have succeeded")
    }
    par.ests = fit$par
    varcov = solve(fit$hessian)
    par.ses = sqrt(diag(varcov))
    out = list(n = n, data = data,
        par.ests = par.ests, par.ses = par.ses, varcov = varcov, 
        converged = fit$convergence, nllh.final = fit$value)
    names(out$par.ests) = c("xi", "sigma", "mu")
    names(out$par.ses) = c("xi", "sigma", "mu")
    class(out) = "gev"
    
    # Return Value:
    out
}

# ------------------------------------------------------------------------------


.gevLLH = 
function(theta, tmp)
{   # A copy from evir

    y = 1 + (theta[1] * (tmp - theta[3]))/theta[2]
    if((theta[2] < 0) || (min(y) < 0)) {
        out = 1e+06
    } else {
        term1 = length(tmp) * logb(theta[2])
        term2 = sum((1 + 1/theta[1]) * logb(y))
        term3 = sum(y^(-1/theta[1]))
        out = term1 + term2 + term3
    }
    
    # Return Value:
    out
}
    

# ------------------------------------------------------------------------------


print.fGEVFIT =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for an object of class "gevFit".
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n" , x@title, "\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n", sep = "")
            
    # Estimation Type:
    cat("\nEstimation Type:\n ", x@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(x@fit$par.ests)
    
    # Desription:
    cat("\nDescription\n ", x@description, "\n\n")
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.fGEVFIT =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "gevFit".
    
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
    if (is.timeSeries(data)) {
        index = as.numeric(names(data))
        plot(index, as.vector(data), type = "h", 
            main = main, xlab = xlab, ylab = ylab,
            col = "steelblue", ...) 
    } else {
        plot(data, type = "h", 
            main = main, xlab = xlab, ylab = ylab,
            col = "steelblue", ...) 
    }
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


# ------------------------------------------------------------------------------


gevrlevelPlot =
function(object, kBlocks = 20,  ci = c(0.90, 0.95, 0.99), 
plottype = c("plot", "add"), labels = TRUE,...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates Return Levels Based on GEV Fit
    
    # Arguments:
    #   object - an object of class "fGEVFIT" as returned by the 
    #       function gevFit().
    #   kBlocks - specifies the particular return level to be 
    #       estimated; default set arbitrarily to 20

    # Note:
    #   Partial copy from R package evir
    
    # Examples:
    #   ans = gevFit(gevSim(), type = "mle", gumbel = FALSE)
    #   ans = gevrlevelPlot(ans); ans@fit$rlevel
    #   ans = gevFit(.gumbelSim(), type = "mle", gumbel = TRUE)
    #   ans = gevrlevelPlot(ans); ans@fit$rlevel
    #
    #   BMW annual (12 month) Return Level: 
    #   ans = gevFit(as.timeSeries(data(bmwRet)), "m"); gevrlevelPlot(ans, 12)
    
    # FUNCTION:
    
    # Check:
    stopifnot(object@method[1] == "gev")
    stopifnot(object@method[2] == "mle")
    stopifnot(kBlocks > 1)
    stopifnot(max(ci) < 1)
    stopifnot(min(ci) > 0)
    
    # Settings:
    out = object@fit
    conf = ci[1]
    plottype = plottype[1]
    
    # Data:
    par.ests = out$par.ests
    mu = par.ests["mu"]
    sigma = par.ests["sigma"]
    xi = par.ests["xi"]
    pp = 1/kBlocks
    v = qgev((1 - pp), xi, mu, sigma)
    if (plottype[1] == "add") abline(h = v)
    data = out$data
    overallmax = out$llh # DW: out$nllh.final
    sigma0 = sqrt(6 * var(data))/pi
    xi0 = 0.01
    theta = c(xi0, sigma0)
    
    # Return Levels:
    parmax = NULL
    rl = v * c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 1, 1.1, 1.2,
        1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 4.5)
    for (i in 1:length(rl)) {
        fit = optim(theta, .gevrlevelLLH, tmp = data, pp = pp, rli = rl[i])
        parmax = rbind(parmax, fit$value)
    }
    parmax = -parmax
    overallmax = -overallmax
    crit = overallmax - qchisq(0.9999, 1)/2
    cond = parmax > crit
    rl = rl[cond]
    parmax = parmax[cond]
    smth = spline(rl, parmax, n = 200)
    aalpha = qchisq(conf[1], 1)
    
    # Labels:
    if (labels) {
        main = paste(kBlocks, "Blocks Return Level")
        xlab = "rl"
        ylab = "parmax"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot ?
    if (plottype[1] == "plot") {
        plot(rl, parmax, type = "p", pch = 19, col = "steelblue",
            main = main, xlab = xlab, ylab = ylab, ...)
        h = overallmax - aalpha/2
        abline(h = h, lty = 3, col = "brown")
        abline(v = v, lty = 3, col = "brown")
        lines(smth, ...)
        if (labels) {
            ciText = paste(as.character(100*conf[1]), "%", sep = "")
            span = 0.025*abs(max(parmax)-min(parmax))
            text(max(rl), h+span, ciText)
        }
        if (length(ci) > 1) {
            for ( i in 2:length(ci) ) {
                gevrlevelPlot(object = object, kBlocks = kBlocks, 
                    ci = ci[i], plottype = c("nextconf"), 
                    labels = labels, ...)
            }
        }
    }
    
    # Internally used to add furter confidence level lines ...
    if (plottype[1] == "nextconf") {
        h = overallmax - aalpha/2
        abline(h = h, lty = 3, col = "brown")
        abline(v = v, lty = 3, col = "brown")
        lines(smth, ...)
        if (labels) {
            ciText = paste(as.character(100*conf[1]), "%", sep = "")
            span = 0.025*abs(max(parmax)-min(parmax))
            text(max(rl), h+span, ciText)
        }
    }
    
    # Or Add ?
    ind = smth$y > overallmax - aalpha/2
    ci = range(smth$x[ind])
    if (plottype[1] == "add") {
        abline(v = ci[1], lty = 2, col = "orange")
        abline(v = ci[2], lty = 2, col = "orange")
    }
    
    # Result:
    ans = as.numeric(c(ci[1], v, ci[2]))
    ans = data.frame(cbind(min = ans[1], v = ans[2], max = ans[3], 
        kBlocks = kBlocks), row.names = "GEV Return Level")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.gevrlevelLLH = 
function(theta, tmp, pp, rli)
{   # A copy from evir

    # FUNCTION:
    
    # LLH:
    mu = rli + (theta[2]*(1-(-log(1-pp))^(-theta[1])))/theta[1]
    y = 1 + (theta[1]*(tmp-mu))/theta[2]
    if ((theta[2] < 0) | (min(y) < 0)) {
        ans = NA
    } else {
        term1 = length(tmp) * log(theta[2])
        term2 = sum((1 + 1/theta[1]) * log(y))
        term3 = sum(y^(-1/theta[1]))
        ans = term1 + term2 + term3
    }
    
    # Return Value:
    ans
}


################################################################################
# MDA ESTIMATORS:


hillPlot = 
function(x, start = 15, ci = 0.95, 
doplot = TRUE, plottype = c("alpha", "xi"), labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
 
    # Description:
    #   Plots the results from the Hill Estimator.
    
    # Note:
    #   Code partly adapted from R package evir
    
    # Examples:
    #   par(mfrow = c(2, 2))
    #   hillPlot(gevSim(n=1000), "alpha")
    #   hillPlot(gevSim(n=1000), "xi")
    #   hillPlot(gevSim(n=1000), "alpha", reverse = TRUE)
    #   hillPlot(gevSim(n=1000), "xi", reverse = TRUE)
    #   hillPlot(gevSim(n=1000), "alpha", doplot = FALSE)
    #   hillPlot(gevSim(n=1000), "xi", doplot = FALSE)
    
    # Check Type:
    if (class(x) == "zoo") x = as.timeSeries(x)
    if (class(x) == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    
    # Settings:
    reverse = FALSE
    option = match.arg(plottype)
    data = x
    
    # MDA:
    ordered = rev(sort(data))
    ordered = ordered[ordered > 0]
    n = length(ordered)
    k = 1:n
    loggs = log(ordered)
    avesumlog = cumsum(loggs)/(1:n)
    xihat = c(NA, (avesumlog - loggs)[2:n])
    y = switch(option, 
        alpha = 1/xihat, 
        xi = xihat,
        quantile = ordered * ((n * (1-p))/k)^(-xihat))
    ses = y / sqrt(k)
    x = trunc(seq(from = min(n, length(data)), to = start))
    y = y[x]
    qq <- qnorm(1 - (1 - ci)/2)
    u <- y + ses[x] * qq
    l <- y - ses[x] * qq
    yrange <- range(u, l)
    if (reverse) index = -x else index = x

    # Plot:
    if (doplot) {
        plot(index, y, ylim = yrange, type = "l", xlab = "", ylab = "",
            axes = FALSE, ...)
        pos = floor(seq(1, length(index), length = 10))
        axis(1, at = index[pos], lab = paste(x[pos]), tick = TRUE)
        axis(2)
        threshold = signif(findThreshold(data, x), 3)
        axis(3, at = index[pos], lab = paste(format(threshold[pos])), 
            tick = TRUE)
        box()
        lines(index, u, lty = 2, col = "steelblue")
        lines(index, l, lty = 2, col = "steelblue")
        if (labels) {
            title(xlab = "Order Statistics", ylab = option)
            mtext("Threshold", side = 3, line = 3)
        }
    }
    
    # Result:
    ans = list(x = index, y = y)
    control = data.frame(plottype = option[1], start = start, ci = ci, 
        reverse = FALSE, row.names = "control")
    attr(ans, "control") = control
    
    # Return Value:
    if (doplot) return(invisible(ans)) else ans
}


# ------------------------------------------------------------------------------


shaparmPlot = 
function(x, p = 0.01*(1:10), xiRange = NULL, alphaRange = NULL,
doplot = TRUE, plottype = c("both", "upper"))
{   # A function written by Diethelm Wuertz 
    
    # Description:
    #   Displays Pickands, Einmal-Decker-deHaan, and Hill estimators
    
    # Example:
    #   par(mfcol=c(3,2)); shaparmPlot(as.timeSeries(data(daxRet)))
    #   shaparmPlot(as.timeSeries(data(daxRet)), doplot = FALSE)
    #   shaparmPlot(as.timeSeries(data(daxRet)), 0.005*(1:20))
    
    # FUNCTION:
    
    # Settings:
    x = as.vector(x)
    tails = p
    if (is.null(xiRange)) xiRange = c(-0.5, 1.5)
    if (is.null(alphaRange)) alphaRange = c(0, 10)
    plottype = match.arg(plottype)
    if (plottype == "both") bothTails = TRUE else bothTails = FALSE
    
    # Median Plot:
    index = which.min(abs(tails-median(tails)))
    DOPLOT = rep(FALSE, length(tails))
    DOPLOT[index] = TRUE
    selected.tail = tails[index]
    if (!doplot) DOPLOT[index] = FALSE
    
    # Which estimator ?
    which = c(TRUE, TRUE, TRUE)
  
    # Settings:
    select.doplot = which
    ylim1 = xiRange
    ylim2 = alphaRange
    z = rep(mean(ylim2), length(tails))
    ylim1 = xiRange
    ylim2 = alphaRange
    
    # Estimates:
    p1 = p2 = h1 = h2 = d1 = d2 = m1 = m2 = rep(0, length(tails))
    for ( i in (1:length(tails)) ) {
        tail = tails[i]
        # Plotting Staff:
        if (select.doplot[1]) {
            xi = shaparmPickands(x, tail, ylim1, doplot = FALSE, 
                plottype = plottype) 
            p1[i] = xi$xi[1]
            p2[i] = xi$xi[3] 
        }
        if (select.doplot[2]) { 
            xi = shaparmHill(x, tail, ylim1, doplot = FALSE, 
                plottype = plottype)  
            h1[i] = xi$xi[1]
            h2[i] = xi$xi[3] 
        }
        if (select.doplot[3]) {
            xi = shaparmDEHaan(x, tail, ylim1, doplot = FALSE, 
                plottype = plottype) 
            d1[i] = xi$xi[1]
            d2[i] = xi$xi[3] 
        }      
    } 
    
    # Plot Pickands' Summary:
    if (select.doplot[1] & doplot) { 
        plot (tails, z, type = "n", xlab = "tail depth", ylab = "alpha",
            ylim = ylim2, main = "Pickands Summary")
        grid()
        abline(v = selected.tail, lty = 3)
        y1 = 1/p1
        x1 = tails [y1 > ylim2[1] & y1 < ylim2[2]]
        y1 = y1[y1 > ylim2[1] & y1 < ylim2[2]]
        points (x1, y1, col = "steelblue")
        lines(x1, y1, col = "steelblue")
        if (bothTails) { 
            y1 = 1/p2
            x1 = tails [y1 > ylim2[1] & y1 < ylim2[2]]
            y1 = y1 [y1 > ylim2[1] & y1 < ylim2[2]]
            points (x1, y1, col = "brown")
            lines(x1, y1, col = "brown")
        } 
    }
    
    # Plot Hill Summary:
    if (select.doplot[2] & doplot) { 
        plot (tails, z, type = "n", xlab = "tail depth", ylab = "alpha", 
            ylim = ylim2, main = "Hill Summary")
        grid()
        abline(v = selected.tail, lty = 3)
        y1 = 1/h1
        x1 = tails [y1 > ylim2[1] & y1 < ylim2[2]]
        y1 = y1 [y1 > ylim2[1] & y1 < ylim2[2]]
        points (x1, y1, col = "steelblue")
        lines(x1, y1, col = "steelblue")
        if (bothTails) { 
            y1 = 1/h2
            x1 = tails [y1 > ylim2[1] & y1 < ylim2[2]]
            y1 = y1 [y1 > ylim2[1] & y1 < ylim2[2]]
            points (x1, y1, col = "brown")
            lines(x1, y1, col = "brown")
        } 
    }
    
    # Plot Deckers-Einmahl-deHaan Summary
    if (select.doplot[3] & doplot) { 
        plot (tails, z, type = "n", xlab = "tail depth", ylab = "alpha", 
            ylim = ylim2, main = "Deckers-Einmahl-deHaan Summary")
        grid()
        abline(v = selected.tail, lty = 3)
        y1 = 1/d1
        x1 = tails [y1>ylim2[1] & y1<ylim2[2]]
        y1 = y1 [y1>ylim2[1] & y1<ylim2[2]]
        points (x1, y1, col = "steelblue")
        lines(x1, y1, col = "steelblue") 
        if (bothTails) { 
            y1 = 1/d2
            x1 = tails [y1 > ylim2[1] & y1 < ylim2[2]]
            y1 = y1 [y1 > ylim2[1] & y1 < ylim2[2]]
            points (x1, y1, col = "brown")
            lines(x1, y1, col = "brown")
        } 
    }
    
    # Plot Estimates:
    resultUpper = resultLower = NULL
    for ( i in (1:length(tails)) ) {
        tail = tails[i]
        # Plotting Staff:
        if (select.doplot[1]) {
            xi = shaparmPickands(x, tail, ylim1, doplot = DOPLOT[i], 
                plottype = plottype)  
            p1[i] = xi$xi[1]
            p2[i] = xi$xi[3] 
        }
        if (select.doplot[2]) { 
            xi = shaparmHill(x, tail, ylim1, doplot = DOPLOT[i], 
                plottype = plottype)  
            h1[i] = xi$xi[1]
            h2[i] = xi$xi[3] 
        }
        if (select.doplot[3]) {
            xi = shaparmDEHaan(x, tail, ylim1, doplot = DOPLOT[i], 
                plottype = plottype) 
            d1[i] = xi$xi[1]
            d2[i] = xi$xi[3] 
        }      
        resultUpper = rbind(resultUpper, c(tails[i], p1[i], h1[i], d1[i]))
        if (bothTails) 
            resultLower = rbind(resultLower, c(tails[i], p2[i], h2[i], d2[i]))
    } 
    colnames(resultUpper) = c("Upper", "Pickands", "Hill", "DEHaan")
    resultUpper = data.frame(resultUpper)
    if (bothTails) {
        colnames(resultLower) = c("Lower", "Pickands", "Hill", "DEHaan")
        resultLower = data.frame(resultLower)
    }
    
    # Result:
    ans = list(Upper = resultUpper)
    if (bothTails) ans$Lower = resultLower
        
    # Return Value:
    if (doplot) return(invisible(ans)) else ans
}


# ------------------------------------------------------------------------------


shaparmPickands = 
function(x, p = 0.05, xiRange = NULL,  
doplot = TRUE, plottype = c("both", "upper"), labels = TRUE, ...)     
{   # A function written by Diethelm Wuertz
    
    # FUNCTION:
    
    # Order Residuals:
    x = as.vector(x)
    tail = p
    if (is.null(xiRange)) xiRange = c(-0.5, 1.5)
    yrange = xiRange
    plottype = match.arg(plottype)
    if (plottype == "both") bothTails = TRUE else bothTails = FALSE
    ordered1 = rev(sort(abs(x[x < 0])))
    if (bothTails) ordered2 = rev(sort(abs(x[x > 0])))
    n1 = length(ordered1)
    if (bothTails) n2 = length(ordered2) 
    ordered1 = ordered1[1:floor(tail*n1)]    
    if (bothTails) ordered2 = ordered2[1:floor(tail*n2)]
    n1 = length(ordered1)
    if (bothTails) n2 = length(ordered2)
    
    # Pickands Estimate:    
    k1 = 1:(n1%/%4)
    if (bothTails) k2 = 1:(n2%/%4) 
    pickands1 = log ((c(ordered1[k1])-c(ordered1[2*k1])) /
        (c(ordered1[2*k1])-c(ordered1[4*k1]))) / log(2)
    if (bothTails) pickands2 = log ((c(ordered2[k2])-c(ordered2[2*k2])) /
        (c(ordered2[2*k2])-c(ordered2[4*k2]))) / log(2)
    
    # Prepare Plot:
    y1 = pickands1[pickands1 > yrange[1] & pickands1 < yrange[2]]
    x1 = log10(1:length(pickands1))[pickands1 > yrange[1] & 
        pickands1 < yrange[2]]
    if (bothTails) {
        y2 = pickands2[pickands2 > yrange[1] & pickands2 < yrange[2]]
        x2 = log10(1:length(pickands2))[pickands2 > yrange[1] & 
            pickands2 < yrange[2]]
    }
    # Labels:
    if (labels) {
        main = "Pickands Estimator"
        xlab = "log scale"
        ylab = "xi"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot:
    if (doplot) { 
        par(err = -1)
        plot (x1, y1, xlab = xlab, ylab = ylab, ylim = yrange, 
            main = main, type = "n")  
        title(sub = paste("tail depth:", as.character(tail)))
            lines(x1, y1, type = "p", pch = 2, col = "steelblue")
        if (bothTails) lines(x2, y2, type = "p", pch = 6, col = "brown") 
        if (labels) grid()
    }
    
    # Calculate invers "xi":
    my1 = mean(y1, na.rm = TRUE)
    if (bothTails) my2 = mean(y2, na.rm = TRUE)
    sy1 = sqrt(var(y1, na.rm = TRUE))
    if (bothTails) sy2 = sqrt(var(y2, na.rm = TRUE))
    
    # Plot:
    if (doplot) {
        par(err = -1)
        lines(c(x1[1], x1[length(x1)]), c(my1,my1), type = "l", 
            lty = 1, col = "steelblue")
        if (bothTails) lines(c(x2[1], x2[length(x2)]), c(my2, my2), 
            type = "l", lty = 1, col = "brown") 
    }
    
    # Result: 
    result = list(xi = c(my1, sy1))   
    if (bothTails) result = list(xi = c(my1, sy1, my2, sy2))
    
    # Return Result:
    result
}
 

# ------------------------------------------------------------------------------

   
shaparmHill = 
function(x, p = 0.05, xiRange = NULL, 
doplot = TRUE, plottype = c("both", "upper"), labels = TRUE, ...)     
{   # A Function written by Diethelm Wuertz
    
    # ORDER RESIDUALS:
    x = as.vector(x)
    tail = p
    if (is.null(xiRange)) xiRange = c(-0.5, 1.5)
    yrange = xiRange
    plottype = match.arg(plottype)
    if (plottype == "both") bothTails = TRUE else bothTails = FALSE
    ordered1 = rev(sort(abs(x[x < 0])))
    if (bothTails) ordered2 = rev(sort(abs(x[x > 0])))
    n1 = length(ordered1)
    if (bothTails) n2 = length(ordered2) 
    ordered1 = ordered1[1:floor(tail*n1)]    
    if (bothTails) ordered2 = ordered2[1:floor(tail*n2)]
    n1 = length(ordered1)
    if (bothTails) n2 = length(ordered2)
    
    # HILLS ESTIMATE:
    hills1 = c((cumsum(log(ordered1))/(1:n1)-log(ordered1))[2:n1])     
    if (bothTails) hills2 = c((cumsum(log(ordered2))/(1:n2) -
        log(ordered2))[2:n2])
        
    # PREPARE PLOT:
    y1 = hills1[hills1 > yrange[1] & hills1 < yrange[2]]
    x1 = log10(1:length(hills1))[hills1 > yrange[1] & hills1 < yrange[2]]
    if (bothTails) {
        y2 = hills2[hills2 > yrange[1] & hills2 < yrange[2]]
        x2 = log10(1:length(hills2))[hills2 > yrange[1] & hills2 < yrange[2]]
    }
    
    # Labels:
    if (labels) {
        main = "Hill Estimator"
        xlab = "log scale"
        ylab = "xi"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot:
    if (doplot) {
        par(err = -1)
        plot (x1, y1, xlab = xlab, ylab = ylab, ylim = yrange, 
            main = main, type="n")
        if (labels) title(sub = paste("tail depth:", as.character(tail)))
        lines(x1, y1, type = "p", pch = 2, col = "steelblue")
        if (bothTails) lines(x2, y2, type = "p", pch = 6, col = "brown") 
        if (labels) grid()
    }
    
    # CALCULATE INVERSE XI:
    my1 = mean(y1, na.rm = TRUE)
    if (bothTails) my2 = mean(y2, na.rm = TRUE)
    sy1 = sqrt(var(y1, na.rm = TRUE))
    if (bothTails) sy2 = sqrt(var(y2, na.rm = TRUE))
    if (doplot) {
        par(err=-1)
        lines(c(x1[1], x1[length(x1)]), c(my1,my1), type="l",
            lty = 1, col = "steelblue")
        if (bothTails) lines(c(x2[1], x2[length(x2)]), c(my2,my2), 
            type = "l",lty = 1, col = "brown") 
    }
    
    # Result:
    result = list(xi = c(my1, sy1))   
    if (bothTails) result = list(xi = c(my1, sy1, my2, sy2))
    
    # Return Result:
    result       
}
       

# ------------------------------------------------------------------------------


shaparmDEHaan = 
function(x, p = 0.05, xiRange = NULL,  
doplot = TRUE, plottype = c("both", "upper"), labels = TRUE, ...)     
{   # A function written by Diethelm Wuertz
    
    # ORDER RESIDUALS:
    x = as.vector(x)
    tail = p
    if (is.null(xiRange)) xiRange = c(-0.5, 1.5)
    yrange = xiRange
    plottype = match.arg(plottype)
    if (plottype == "both") bothTails = TRUE else bothTails = FALSE
    ordered1 = rev(sort(abs(x[x < 0])))
    if (bothTails) ordered2 = rev(sort(abs(x[x > 0])))
    n1 = length(ordered1)
    if (bothTails) n2 = length(ordered2) 
    ordered1 = ordered1[1:floor(tail*n1)]    
    if (bothTails) ordered2 = ordered2[1:floor(tail*n2)]
    n1 = length(ordered1)
    if (bothTails) n2 = length(ordered2)
    
    # DECKERS-EINMAHL-deHAAN ESTIMATE:
    ns0 = 1
    n1m = n1-1; ns1 = ns0; ns1p = ns1+1
    bod1 = c( cumsum(log(ordered1))[ns1:n1m]/(ns1:n1m) -
            log(ordered1)[ns1p:n1] ) 
    bid1 = c( cumsum((log(ordered1))^2)[ns1:n1m]/(ns1:n1m) -
            2*cumsum(log(ordered1))[ns1:n1m]*log(ordered1)[ns1p:n1]/(ns1:n1m) +
            ((log(ordered1))^2)[ns1p:n1] )
    dehaan1 = ( 1.0 + bod1 + ( 0.5 / (  bod1^2/bid1 - 1 ) ))
    if (bothTails) {
    n2m = n2-1; ns2 = ns0; ns2p = ns2+1
    bod2 = c( cumsum(log(ordered2))[ns2:n2m]/(ns2:n2m) -
            log(ordered2)[ns2p:n2] ) 
    bid2 = c(  cumsum((log(ordered2))^2)[ns2:n2m]/(ns2:n2m) -
            2*cumsum(log(ordered2))[ns2:n2m]*log(ordered2)[ns2p:n2]/(ns2:n2m) +
            ((log(ordered2))^2)[ns2p:n2] )
    dehaan2 = ( 1.0 + bod2 + ( 0.5 / (  bod2^2/bid2 - 1 ) )) }
    
    # PREPARE PLOT:
    y1 = dehaan1[dehaan1 > yrange[1] & dehaan1 < yrange[2]]
    x1 = log10(1:length(dehaan1))[dehaan1 > yrange[1] & dehaan1 < yrange[2]]
    if (bothTails) {
        y2 = dehaan2[dehaan2 > yrange[1] & dehaan2 < yrange[2]]
        x2 = log10(1:length(dehaan2))[dehaan2 > yrange[1] & 
            dehaan2 < yrange[2]] 
    }
    
    # Labels:
    if (labels) {
        main = "Deckers - Einmahl - de Haan Estimator"
        xlab = "log scale"
        ylab = "xi"
    } else {
        main = xlab = ylab = ""
    }
    
    # Plot:
    if (doplot) { 
        par(err = -1)
        plot (x1, y1, xlab = xlab, ylab = ylab, ylim = yrange,
            main = main, type = "n")
        if (labels) title(sub = paste("tail depth:", as.character(tail)))
        lines(x1, y1, type = "p", pch = 2, col = "steelblue")
        if (bothTails) lines(x2, y2, type = "p", pch = 6, col = "brown") 
        if (labels) grid()
    }
            
    # CALCULATE INVERSE XI:
    my1 = mean(y1, na.rm = TRUE)
    if (bothTails) my2 = mean(y2, na.rm = TRUE)
    sy1 = sqrt(var(y1, na.rm = TRUE))
    if (bothTails) sy2 = sqrt(var(y2, na.rm = TRUE))
    if (doplot) {
        par(err = -1)
        lines(c(x1[1], x1[length(x1)]), c(my1,my1), type = "l", 
            lty = 1, col = "steelblue")
        if (bothTails) lines(c(x2[1], x2[length(x2)]), c(my2, my2), 
            type = "l", lty = 1, col = "brown") 
    }
        
    # Result:
    result = list(xi = c(my1, sy1))   
    if (bothTails) result = list(xi = c(my1, sy1, my2, sy2))
    
    # Return Result:
    result
}


################################################################################

