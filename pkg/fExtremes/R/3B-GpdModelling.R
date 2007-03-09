
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
# FUNCTION:               GPD SIMULATION:
#  gpdSim                  Simulates a GPD distributed process
# FUNCTION:               GPD PARAMETER ESTIMATION:
#  'fGPDFIT'               S4 class representation
#  gpdFit                  Fits Parameters of GPD distribution
#  .gpdpwmFit              Fits GPD with probability weighted moments
#  .gpdmleFit              Fits GPD with max log-likelihood approach
#   .gpdLLH                 Computes GPD log-likelihood function
# METHODS:                PRINT, PLOT, AND SUMMARY:
#  show.fGPDFIT            S4 Print Method for object of class "fGPDFIT"
#  plot.fGPDFIT            S3 Plot Method for object of class "fGPDFIT"
#  .gpd1Plot                Empirical Distribution Plot
#  .gpd2Plot                Tail of Underlying Distribution
#  .gpd3Plot                Scatterplot of GPD Residuals
#  .gpd4Plot                Quantile-Quantile Plot of GPD Residuals
#  summary.fGPDFIT         S3 Summary Method for object of class "fGPDFIT"
################################################################################


gpdSim = 
function(model = list(xi = 0.25, mu = 0, beta = 1), n = 1000, seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates random variates from a GPD distribution
    
    # FUNCTION:
    
    # Seed:
    if (is.null(seed)) seed = NA else set.seed(seed)
    
    # Simulate:
    ans = rgpd(n = n, xi = model$xi, mu = model$mu, beta = model$beta)  
    ans = as.ts(ans)

    # Control:
    attr(ans, "control") = 
        data.frame(t(unlist(model)), seed = seed, row.names = "")
        
    # Return Value:
    ans 
}


################################################################################


setClass("fGPDFIT", 
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


gpdFit =
function(x, u = quantile(x, 0.95), type = c("mle", "pwm"),
information = c("observed", "expected"), title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits a generalized Pareto model to excesses
    
    # Arguments:
    
    # Details:
    #   Returns an object of class "fGPDFIT" representing the fit of a
    #   generalized Pareto model to excesses over a high threshold
    
    # Notes:
    #   This is a wrapper to EVIR's 'gpd' function.

    # FUNCTION:
    
    # Settings:
    call = match.call()
    type = match.arg(type)
    information = match.arg(information)
    
    # Check Type and Convert:
    X = x  
    xClass = class(x)
    if (xClass == "timeSeries") stopifnot(isUnivariate(x))
    x = as.vector(x)
    N = length(x)
    
    # Compute Exceedances:
    exceedances = x[x > u]
    Names = as.character((1:N)[x > u])
    exceedances = as.vector(exceedances)
    names(exceedances) = Names

    # Estimate Parameters:
    if (type == "mle") {
        fit = .gpdmleFit(x, u, information)
        fit$llh = fit$fit$value
        fit$convergence = fit$fit$convergence
    } else if (type == "pwm") {
        fit = .gpdpwmFit(x, u)
        fit$llh = NA
        fit$convergence = NA
    }
    fit$p.less.thresh = fit$prob = 1 - length(x[x > u]) / length(x)
    fit$threshold = u
    fit$data = x
   
    # Compute Residuals:
    xi = fit$par.ests["xi"]
    beta = fit$par.ests["beta"]
    residuals = log(1 + (xi * (as.vector(exceedances)-u))/beta)/xi
    
    # Add title and description:
    if (is.null(title)) title = "GPD Parameter Estimation"
    if (is.null(description)) description = .description()
    
    # Compose Parameter List:
    parameter = list(u = u, type = type)
    if (information == "mle") parameter$information = information
    
    # Return Value:
    new("fGPDFIT",
        call = call,
        method = c("gpd", type), 
        parameter = parameter,
        data = list(x = X, exceedances = exceedances),
        fit = fit,
        residuals = residuals,
        title = title,
        description = description)
}


# ------------------------------------------------------------------------------


.gpdmleFit = 
function(x, u = quantile(x, 0.95), information = c("observed", "expected"), ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits GPD with max log-likelihood approach
    
    # FUNCTION:
    
    x = as.vector(x)
    excess = x[x > u] - u
    theta = .gpdpwmFit(x = x, u = u)$par.ests
    
    # Parameter Estimation:
    fit = optim(theta, .gpdLLH, hessian = TRUE, excess = excess)
    names(fit$par) = c("xi", "beta")
    
    # Error Estimates:
    if (information[1] == "observed") {
        varcov = solve(fit$hessian)
    }
    if (information[1] == "expected") {
        Nu = length(excess)
        one = (1 + fit$par[1])^2/Nu
        two = (2 * (1 + fit$par[1]) * fit$par[2]^2)/Nu
        cov = -((1 + fit$par[1]) * fit$par[2])/Nu
        varcov = matrix(c(one, cov, cov, two), 2)
    }
    par.ses = sqrt(diag(varcov))
    names(par.ses) = c("xi", "beta")  
    
    # Return Value:
    list(par.ests = fit$par, par.ses = par.ses, fit = fit, varcov = varcov)
}


# ------------------------------------------------------------------------------


.gpdLLH =
function(theta, excess)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes GPD log-likelihood function
    
    # FUNCTION:
    
    # LLH:
    xi = theta[1]
    beta = theta[2]
    cond = (beta <= 0) || ((xi <= 0) && (max(excess) > (-beta/xi)))
    if (cond) {
        func = NA
    } else {
        y = log(1+(xi*excess)/beta) / xi
        func = length(excess) * log(beta) + (1+xi)*sum(y)
    }
    
    # Return Value:
    func
}


# ------------------------------------------------------------------------------


.gpdpwmFit = 
function(x, u = quantile(x, 0.95)) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits GPD with probability weighted moments
    
    # FUNCTION:
    
    # PWM Fit:
    x = as.vector(x)
    excess = x[x > u] - u 
    Nu = length(excess)
    a0 = mean(excess)
    pvec = ((1:Nu) - 0.35)/Nu
    a1 = mean(sort(excess) * (1 - pvec))
    xi = 2 - a0/(a0 - 2 * a1)
    beta = (2 * a0 * a1)/(a0 - 2 * a1)
    par.ests = c(xi = xi, beta = beta)
    par.ses = c(xi = NA, beta = NA)
    
    # Return Value:
    list(par.ests = par.ests, par.ses = par.ses, fit = NA, varcov = NA)
}


################################################################################


show.fGPDFIT =
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for an object of class 'gpdFit'
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n ", object@title, "\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", 
        collapse = "\n"), "\n", sep = "") 
            
    # Estimation Type:
    cat("\nEstimation Method:\n ", object@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(object@fit$par.ests)
    
    # Desription:
    cat("\nDescription\n ", object@description, "\n\n")
    
    
    # Return Value:
    invisible(object)
}


# ------------------------------------------------------------------------------


setMethod("show", "fGPDFIT", show.fGPDFIT)


# ------------------------------------------------------------------------------


plot.fGPDFIT =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for objects of class 'fGPDFIT'

    # Example:
    #   x = as.timeSeries(danishClaims); plot(gpdFit(x, 4), "ask")
    
    # FUNCTION:
            
    # Plot:
    interactivePlot(
        x = x,
        choices = c(
            "Excess Distribution",
            "Tail of Underlying Distribution",
            "Scatterplot of Residuals", 
            "QQ-Plot of Residuals"),
        plotFUN = c(
            ".gpd1Plot", 
            ".gpd2Plot", 
            ".gpd3Plot",
            ".gpd4Plot"),
        which = which)
                    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


.gpd1Plot =
function(x, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Empirical Distribution Plot
    
    # Arguments:
    #   x - an object of class fGPDFIT
    #   labels - a logical flag. Should labels be printed?
    
    # FUNCTION:
    
    # Data:
    extend = 1.5
    u = x@parameter$u
    data = as.vector(x@data$exceedances)
    sorted = sort(data)
    shape = xi = x@fit$par.ests["xi"]
    scale = beta = x@fit$par.est["beta"]
    ypoints = ppoints(sorted)
    U = max(sorted)*extend
    z = qgpd(seq(0, 1, length = 1000), xi, u, beta)
    z = pmax(pmin(z, U), u)
    y = pgpd(z, xi, u, beta) 
    
    # Labels:
    if (labels) {
        xlab = "Fu(x-u)"
        ylab = "x [log Scale]"
        main = "Excess Distribution"
    } else {
        xlab = ylab = main = ""
    }
    
    # Plot:
    plot(x = sorted, y = ypoints, 
        xlim = range(u, U), ylim = range(ypoints, y, na.rm = TRUE), 
        main = main, xlab = xlab, ylab = ylab, 
        log = "x", axes = TRUE, 
        col = "steelblue", pch = 19, ...)
    lines(z[y >= 0], y[y >= 0], col = "brown") 
    
    # Addon:
    if (labels) {
        u = signif (x@parameter$u, 3)
        text = paste("u =", u)
        mtext(text, side = 4, adj = 0, cex = 0.7)
        grid()
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.gpd2Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tail of Underlying Distribution
    
    # Arguments:
    #   x - an object of class fGPDFIT
    #   labels - a logical flag. Should labels be printed?
    
    # FUNCTION:
    
    # Settings:
    extend = 1.5
    u = x@parameter$u
    data = as.vector(x@data$x)
    sorted = sort(data[data > u])
    prob = x@fit$prob
    shape = xi = x@fit$par.ests["xi"]
    beta = x@fit$par.ests["beta"]
    scale = beta * (1-prob)^xi
    location = u - (scale*((1 - prob)^(-xi)-1))/xi

    # Labels:
    if (labels) {
        xlab = "x [log scale]"
        ylab = "1-F(x) [log scale]"
        main = "Tail of Underlying Distribution"
    } else {
        xlab = ylab = main = ""
    }
    
    # Plot:
    U = max(data) * extend
    ypoints = ppoints(sorted)
    ypoints = (1 - prob) * (1 - ypoints)
    z = qgpd(seq(0, 1, length = 1000), xi, u, beta)
    z = pmax(pmin(z, U), u)
    y = pgpd(z, xi, u, beta)   
    y = (1 - prob) * (1 - y)
    plot(x = sorted, y = ypoints, 
        xlim = range(u, U), ylim = range(ypoints, y[y>0], na.rm = TRUE), 
        main = main, xlab = xlab, ylab = ylab, 
        log = "xy", axes = TRUE, 
        col = "steelblue", pch = 19, ...)
        
    # Line:
    lines(z[y >= 0], y[y >= 0], col = "brown")    
    if (labels) grid()
    
    # Return Value:
    invisible(list(x = sorted, y = ypoints))
}


# ------------------------------------------------------------------------------


.gpd3Plot =
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Scatterplot of GPD Residuals
    
    # Arguments:
    #   x - an object of class fGPDFIT
    #   labels - a logical flag. Should labels be printed?
    
    # FUNCTION:
    
    # Residuals:
    residuals = x@residuals
    
    # Labels:
    if (labels) {
        ylab = "Residuals"
        xlab = "Ordering"
        main = "Scatterplot of Residuals"
    } else {
        xlab = ylab = main = ""
    }
    
    # Plot:
    plot(residuals, 
       main = main, ylab = ylab, xlab = xlab, 
       col = "steelblue", pch = 19, ...)
    lines(lowess(1:length(residuals), residuals), col = "brown") 
    if (labels) grid()
    
    # Return Value:
    invisible(list(x = 1:(length(residuals)), y = residuals))
}


# ------------------------------------------------------------------------------


.gpd4Plot = 
function(x, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Quantile-Quantile Plot of GPD Residuals
    
    # Arguments:
    #   x - an object of class fGPDFIT
    #   labels - a logical flag. Should labels be printed?
    
    # FUNCTION:
    
    # Data:
    data = x@residuals
    sorted = sort(data)
    
    # Labels:
    if (labels) {
        xlab = "Ordered Data"
        ylab = "Exponential Quantiles"
        main = "QQ-Plot of Residuals"
    } else {
        xlab = ylab = main = ""
    }
    
    # Plot:
    y = qexp(ppoints(data))
    plot(x = sorted, y = y, 
        main = main, xlab = xlab, ylab = ylab, 
        col = "steelblue", pch = 19, ...)
    abline(lsfit(sorted, y), col = "brown")
    if (labels) grid()
    
    # Return Value:
    invisible(list(x = sorted, y = y))
}


# ------------------------------------------------------------------------------


summary.fGPDFIT = 
function(object, doplot = TRUE, which = "all", ...) 
{   # A function written by Diethelm Wuertz

    # Description:
    #   Summary method for objects of class "gpdFit"
    
    # FUNCTION:

    # Title:
    cat("\nTitle:\n" , object@title, "\n")
    
    # Function Call:
    cat("\nCall:\n")
    cat(paste(deparse(object@call), sep = "\n", 
        collapse = "\n"), "\n", sep = "") 
            
    # Estimation Type:
    cat("\nEstimation Type:\n ", object@method, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(object@fit$par.ests)
    
    # Summary - For MLE print additionally:
    if (object@method[2] == "mle") {
        cat("\nStandard Deviations:\n"); print(object@fit$par.ses)
        if (!is.na(object@fit$llh))
            cat("\nLog-Likelihood Value:\n ", object@fit$llh, "\n")
        if (!is.na(object@fit$convergence))
            cat("\nType of Convergence:\n ", object@fit$convergence, "\n") 
    }
    
    # Plot:
    if (doplot) {
        plot(object, which = which, ...)
    }
    
    # Desription:
    cat("\nDescription\n ", object@description, "\n\n")
    
    # Return Value:
    invisible(object)
}


################################################################################

