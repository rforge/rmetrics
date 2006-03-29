
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
# FUNCTION:             GEV DISTRIBUTION FAMILY: [USE FROM EVD]
#  devd                  Density for the GEV Distribution 
#   pevd                  Probability for the GEV Distribution
#   qevd                  Quantiles for the GEV Distribution
#   revd                  Random variates for the GEV Distribution
# FUNCTION:             GEV DISTRIBUTION FAMILY: [USE FROM EVIS]
#  dgev                  Density for the GEV Distribution 
#   pgev                  Probability for the GEV Distribution
#   qgev                  Quantiles for the GEV Distribution
#   rgev                  Random variates for the GEV Distribution
# FUNCTION:             MOMENTS:
#  .gevMoments            Computes true statistics for GEV distribution
################################################################################
# FUNCTION:             GEV MODELLING FROM EVIR:
#  gevSim                Simulates GEV including Gumbel rvs [EVIS/EVIR]
#  fGEV                  S4 Class Representation
#  gevFit                Fits GEV Distribution
#   print.fGEV            Print Method for object of class "gevFit"
#   plot.fGEV             Plot Method for object of class "gevFit"
#   summary.fGEV          Summary Method for object of class "gevFit"
#  gevrlevelPlot         Calculates Return Levels Based on GEV Fit
################################################################################
# FUNCTION:             MDA ESTIMATORS:
#  hillPlot              Plot Hill's estimator
#  shaparmPlot           Pickands, Hill & Decker-Einmahl-deHaan Estimator
#   shaparmPickands      Auxiliary function called by shaparmPlot
#   shaparmHill           ... called by shaparmPlot
#   shaparmDehaan         ... called by shaparmPlot
################################################################################


################################################################################
# GEV DISTRIBUTION FAMILY: [USE FROM EVD]


devd = 
function (x, loc = 0, scale = 1, shape = 0, log = FALSE) 
{
    # FUNCTION:
    
    if (min(scale) <= 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    x = (x - loc)/scale
    if (shape == 0) {
        d = log(1/scale) - x - exp(-x)
    } else {
        nn = length(x)
        xx = 1 + shape * x
        xxpos = xx[xx > 0 | is.na(xx)]
        scale = rep(scale, length.out = nn)[xx > 0 | is.na(xx)]
        d = numeric(nn)
        d[xx > 0 | is.na(xx)] = log(1/scale) - xxpos^(-1/shape) - 
            (1/shape + 1) * log(xxpos)
        d[xx <= 0 & !is.na(xx)] = -Inf
    }
    if (!log) {
        d = exp(d)
    }
    
    # Return Value:
    d
}


# ------------------------------------------------------------------------------


pevd =
function (q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE) 
{
    # FUNCTION:
    
    if (min(scale) <= 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    q = (q - loc)/scale
    if (shape == 0) {
        p = exp(-exp(-q))
    } else {
        p = exp(-pmax(1 + shape * q, 0)^(-1/shape))
    }
    if (!lower.tail) {
        p = 1 - p
    }
    
    # Return Value:
    p
}


# ------------------------------------------------------------------------------


qevd = 
function (p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE) 
{
    # FUNCTION:
    
    if (min(p, na.rm = TRUE) <= 0 || max(p, na.rm = TRUE) >= 
        1) 
        stop("`p' must contain probabilities in (0,1)")
    if (min(scale) < 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    if (!lower.tail) 
        p = 1 - p
    if (shape == 0) {
        return(loc - scale * log(-log(p)))
    } else {
        return(loc + scale * ((-log(p))^(-shape) - 1)/shape)
    }
}


# ------------------------------------------------------------------------------


revd =
function (n, loc = 0, scale = 1, shape = 0) 
{
    # FUNCTION:
    
    if (min(scale) < 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    if (shape == 0) {
        return(loc - scale * log(rexp(n)))
    } else {
        return(loc + scale * (rexp(n)^(-shape) - 1)/shape)
    }
}


################################################################################


dgev =
function(x, xi = 1, mu = 0, sigma = 1, log = FALSE)
{   # A function implemented from evd

    # Description:
    #   GEV Density Function
    #   Note: 1 + xi*(x-mu)/sigma > 0
    #   xi > 0 Frechet
    #   xi = 0 Gumbel
    #   xi < 0 weibl

    # FUNCTION:
    
    # Settings:
    loc = mu
    scale = sigma
    shape = xi
    
    # Density function:
    if (min(scale) <= 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    x = (x - loc)/scale
    if (shape == 0) {
        d = log(1/scale) - x - exp(-x)
    } else {
        nn = length(x)
        xx = 1 + shape * x
        xxpos = xx[xx > 0 | is.na(xx)]
        scale = rep(scale, length.out = nn)[xx > 0 | is.na(xx)]
        d = numeric(nn)
        d[xx > 0 | is.na(xx)] = log(1/scale) - xxpos^(-1/shape) - 
            (1/shape + 1) * log(xxpos)
        d[xx <= 0 & !is.na(xx)] = -Inf
    }
    if (!log) {
        d = exp(d)
    }
    
    # Return Value:
    d
}


# ------------------------------------------------------------------------------


pgev =
function(q, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
{   # A function implemented from evd
 
    # Description:
    #   GEV Probability Function
    #   Note: 1 + xi*(x-mu)/sigma > 0
    #   xi > 0 Frechet
    #   xi = 0 Gumbel
    #   xi < 0 Weibull

    # FUNCTION:
    
    # Settings:
    loc = mu
    scale = sigma
    shape = xi
    
    # Probability function:
    if (min(scale) <= 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    q = (q - loc)/scale
    if (shape == 0) {
        p = exp(-exp(-q))
    } else {
        p = exp(-pmax(1 + shape * q, 0)^(-1/shape))
    }
    if (!lower.tail) {
        p = 1 - p
    }
    
    # Return Value:
    p
}


# ------------------------------------------------------------------------------


qgev =
function (p, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
{   # A function implemented from evd

    # Description:
    #   GEV Quantile Function
    #   Note: 1 + xi*(x-mu)/sigma > 0
    #   xi > 0 Frechet
    #   xi = 0 Gumbel
    #   xi < 0 Weibull

    # FUNCTION:
    
    # Settings:
    loc = mu
    scale = sigma
    shape = xi
    
    # Return Value:
    if (min(p, na.rm = TRUE) < 0 || max(p, na.rm = TRUE) > 1) 
        stop("`p' must contain probabilities in (0,1)")
    if (min(scale) < 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    if (!lower.tail) 
        p = 1 - p
    if (shape == 0) {
        q = loc - scale * log(-log(p))
    } else {
        q = loc + scale * ((-log(p))^(-shape) - 1)/shape
    }
        
    # Return Value:
    q
}


# ------------------------------------------------------------------------------


rgev =
function (n, xi = 1, mu = 0, sigma = 1)
{   # A function implemented from evd

    # Description:
    #   GEV Random Variables
    #   Note: 1 + xi*(x-mu)/sigma > 0
    #   xi > 0 Frechet
    #   xi = 0 Gumbel
    #   xi < 0 Weibull

    # FUNCTION:
    
    # Settings:
    loc = mu
    scale = sigma
    shape = xi
    
    # Return Value:
    if (min(scale) < 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    if (shape == 0) {
        r = loc - scale * log(rexp(n))
    } else {
        r = loc + scale * (rexp(n)^(-shape) - 1)/shape
    }
    
    # Return Value:
    r
}


################################################################################


.gevMoments = 
function(xi, mu = 0, beta = 1)
{   # A function implemented by Diethelm Wuertz
  
    # Description:
    #   Compute true statistics for Generalized Extreme Value distribution
    
    # Value:
    #   Returns true mean for xi < 1 and variance for xi < 1/2
    #   of GEV distribution, otherwise NaN is returned

    # FUNCTION: 
    
    # MEAN: Returns for x >= 1 NaN:
    g = c(1, 0, NaN)
    xinv = 1/ ( xi + sign(abs(xi)) - 1 )
    # For xi = the result is eulers constant
    euler = 0.57721566490153286060651209008240243104    
    xi0 = c(0, mu+beta*euler, 0)
    
    # Supress warning for NaN's from Gamma Function:
    options(warn = -1)
    gevMean = mu + beta * xinv * (gamma(1-xi)-1) * g[sign(xi-1)+2] +
        xi0[(sign(xi)+2)]
    options(warn = 0)
    
    # VAR: Returns for x >= 1 NaN:
    g = c(1, 0, NaN)
    xinv = 1/ ( xi + sign(abs(xi)) - 1 )
    xi0 = c(0, (beta*pi)^2 / 6, 0)
    
    # Supress warning for NaN's from Gamma Function:
    options(warn=-1)
    gevVar = (beta*xinv)^2 * (gamma(1-2*xi) - gamma(1-xi)^2 ) * 
        g[sign(2*xi-1)+2] + xi0[(sign(xi)+2)]
    options(warn = 0)     

    # Return Value:
    list(mean = gevMean, var = gevVar)      
}
    
    
################################################################################
# GEV MODELLING:


gevSim = 
function(model = list(shape = 0.25, location = 0, scale = 1), 
n = 1000, seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates random variates from a GEV distribution
    
    # FUNCTION:
    
    # Seed:
    if (is.null(seed)) seed = NA else set.seed(seed)
    
    # Simulate:
    ans = rgev(n = n, xi = model$shape, mu = model$location, 
        sigma = model$scale)
    ans = as.ts(ans)
    
    # Control:
    attr(ans, "control") = 
        data.frame(t(unlist(model)), seed = seed, row.names = "")
        
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------
# Class Representation


setClass("fGEV", 
    representation(
        call = "call",
        data = "list",
        method = "character",
        fit = "list",
        title = "character",
        description = "character"
    )  
)


# ------------------------------------------------------------------------------
    

gevFit =
function(x, type = c("mle", "pwm"), gumbel = FALSE, title = NULL,
description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits parameters to a GEV distribution
    
    # Arguments:
    #   x - a numeric vector of Block Maxima
    
    # Note:
    #   Argument named "method is already used for the selection
    #   of the MLE optimization algorithm, therfore we use here
    #   "type".
    
    # FUNCTION:
    
    # Transform:
    x = as.vector(x)
    
    # Settings:
    call = match.call()
    type = type[1]
    block = NA
    
    # Estimate Parameters:
    if (gumbel) {   
        # GUMBEL: Add Call and Type
        if (length(type) > 1) type = type[1]
        # Probability Weighted Moment Estimation:
        if (type == "pwm") {
            fitted = .gumbel.pwm(data = x, block = block, ...) 
        }
        # Maximum Log Likelihood Estimation:
        # Use Alexander McNeils EVIS from evir Package ...
        if (type == "mle") { 
            fitted = gumbel(data = x, block = block, ...) 
        } 
    } else {
        # GEV: Add Call and Type
        if (length(type) > 1) type = type[1]
        # Probability Weighted Moment Estimation:
        if (type == "pwm") { 
            fitted = .gev.pwm(data = x, block = block, ...) 
        }
        # Maximum Log Likelihood Estimation:
        # Use Alexander McNeils EVIS from evir Package ...
        if (type == "mle") { 
            fitted = gev(data = x, block = block, ...) 
        }    
    }
            
    # Compute Residuals:
    if (gumbel) {
        # GUMBEL:
        xi = 0
        sigma = fitted$par.ests[1]
        mu = fitted$par.ests[2] 
        fitted$residuals = exp( - exp( - (fitted$data - mu)/sigma)) 
    } else {
        # GEV:
        xi = fitted$par.ests[1]
        sigma = fitted$par.ests[2]
        mu = fitted$par.ests[3]
        fitted$residuals = (1 + (xi * (fitted$data - mu))/sigma)^(-1/xi) 
    }  
        
    # Make Unique:
    fit = list()
    fit$fit = fitted
    fit$call = call
    fit$type = c(if (gumbel) "gum" else "gev", type[1])
    fit$par.ests = fitted$par.ests
    fit$par.ses = fitted$par.ses
    fit$residuals = fitted$residuals
    fit$fitted.values = fitted$data - fitted$residuals
    fit$llh = fitted$nllh.final
    fit$converged = fitted$converged
    class(fit) = c("list", "gevFit")
    
    # Add title and description:
    if (is.null(title)) title = "GEV Parameter Estimation"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fGEV",
        call = match.call(),
        data = list(x = x),
        method = fit$type,
        fit = fit,
        title = "character",
        description = "character")
}


# ------------------------------------------------------------------------------


.sampwm = 
function (x, nmom) 
{   # A function implemented by Diethelm Wuertz

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


.gev.pwm = 
function(data, block = NA, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Probability Weighted Moment method.
    data = as.vector(data)
    n = length(data)    
    
    # Internal Function:
    y = function(x, w0, w1, w2) { 
        (3^x-1)/(2^x-1) - (3*w2 - w0)/(2*w1 - w0) 
    }       
    
    # Calculate:
    w = .sampwm(data, nmom = 3)
    w0 = w[1]
    w1 = w[2]
    w2 = w[3]      
    xi = uniroot(f = y, interval = c(-5,+5), 
        w0 = w[1], w1 = w[2], w2 = w[3])$root
    sigma = beta = (2*w1-w0)*xi / gamma(1-xi) / (2^xi-1)
    mu = w0 + beta*(1-gamma(1-xi))/xi
    
    # Output:
    fit = list(n.all = n.all, n = n, data = data, bock = block, 
        par.ests = c(xi, sigma, mu), par.ses = rep(NA, 3),
        varcov = matrix(rep(NA, 9), 3, 3), converged = NA, 
        nllh.final = NA, call=match.call(), selected = "pwm")
    names(fit$par.ests) = c("xi", "sigma", "mu")
    names(fit$par.ses) = c("xi", "sigma", "mu") 
    
    # Return Value:
    class(fit) = "gev" 
    fit 
}
    
    
# ------------------------------------------------------------------------------


.gumbel.pwm = 
function(data, block = NA, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # "Probability Weighted Moment" method.
    data = as.vector(data)
    n = length(data)
    # Sample Moments:
    x = rev(sort(data))
    lambda = c(mean(x), 0)
    for (i in 1:n) {
        weight = (n-i)/(n-1)/n
        lambda[2] = lambda[2] + weight*x[i] } 
    # Calculate Parameters:
    xi = 0
    sigma = beta = lambda[2]/log(2)
    mu = lambda[1] - 0.5772*beta
    # Output:
    fit = list(n.all = n.all, n = n, data = data, block = block, 
        par.ests = c(sigma, mu), par.ses = rep(NA, 2),
        varcov = matrix(rep(NA, 4), 2, 2), converged = NA, 
        nllh.final = NA, call = match.call(), selected = "pwm")
    names(fit$par.ests) = c("sigma", "mu")
    names(fit$par.ses) = c("sigma", "mu")
    
    # Return Value:
    class(fit) = "gev" # not gumbel!
    fit 
}
    

# ------------------------------------------------------------------------------


print.fGEV =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for an object of class "gevFit".
    
    # FUNCTION:
    
    # @fit Slot:
    description = x@description
    x = x@fit
    class(x) = "gevFit"
    
    # Title:
    cat("\nTitle:\n GEV Fit\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
            
    # Estimation Type:
    cat("\nEstimation Type:\n ", x$type, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(x$par.ests)
    
    # Desription:
    cat("\nDescription\n ", description, "\n\n")
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.fGEV =
function(x, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "gevFit".
    
    # Details:
    #   plot.gev:
    #   Data are converted to unit exponentially distributed residuals
    #   under null hypothesis that GEV fits. Two diagnostics for iid
    #   exponential data are offered:
    #   "Scatterplot of Residuals" and "QQplot of Residuals"

    # FUNCTION: 
    
    # @fit Slot:
    x = x@fit
    class(x) = "gevFit"
    
    # Internal Plot Functions:
    plot.1 <<- function(x) {
        # Time Series Plot of Block Maxima:
        plot(x$fit$data, type = "h", col = "steelblue",  
            xlab = "Index",
            ylab = "Data", 
            main = "Block Maxima") 
    }
    plot.2 <<- function(x) {
        # Lowess Fit to Scatterplot of Residuals:
        plot(x$residuals, pch = 19, cex = 0.5,
            xlab = "Ordering",
            ylab = "Residuals",  
            main = "Scatterplot of Residuals")
        lines(lowess(1:length(x$residuals), x$residuals),  
            col = "steelblue") 
        grid()
    }
    plot.3 <<- function(x) {
        # Histogram Plot of Residuals with Gaussian Fit:
        hist(x$residuals, probability = TRUE, breaks = "FD",
            col = "steelblue", border = "white",
            xlab = "Residuals",
            ylab = "Density",  
            main = "GEV Fit and Residual Histrogram")
        # xi = x$par.ests[1]
        # sigma = x$par.ests[2]
        # mu = x$par.ests[3]
        # r = range(x$residuals)
    }
    plot.4 <<- function(x) {            
        # Quantile-Quantile Plot:
        # evir::qplot
        qplot(x$residuals, col = "steelblue", pch = 19, cex = 0.5,
            # xlab = "Ordered Data",
            # ylab = "Exponential Quantiles",
            main = "Quantile-Quantile Plot") 
        grid()
    }
            
    # Plot:
    interactivePlot(
        x = x,
        choices = c(
            "Block Maxima Plot", 
            "Scatterplot of Residuals", 
            "Histogram of Residuals",
            "Quantile Quantile Plot"),
        plotFUN = c(
            "plot.1", 
            "plot.2",
            "plot.3", 
            "plot.4"),
        which = which)
                    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------
  

summary.fGEV =
function(object, doplot = TRUE, which = "all", ...) 
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Summary method for an object of class "gevFit".

    # FUNCTION:
    
    # @fit Slot:
    description = object@description
    plotObject = object
    object = object@fit
    class(object) = "gevFit"
    
    # Title:
    cat("\nTitle:\n GEV Fit\n")
    
    # Function Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object$call), sep = "\n", 
        collapse = "\n"), "\n", sep = "")
            
    # Estimation Type:
    cat("\nEstimation Type:\n ", object$type, "\n") 
    
    # Estimated Parameters:
    cat("\nEstimated Parameters:\n")
    print(object$par.ests)
    
    # Summary:
    if (object$type[2] == "mle") {
        cat("\nStandard Deviations:\n "); 
        print(object$par.ses)
        cat("\nLog-Likelihood Value:\n ", object$llh, "\n")
        cat("\nType of Convergence:\n ", object$converged, "\n") } 
    
    # Plot:
    if (doplot) {
        plot(plotObject, which = which, ...)
    }
    
    # Desription:
    cat("\nDescription\n ", description, "\n\n")
    
    # Return Value:
    invisible(object)
}


# ------------------------------------------------------------------------------


gevrlevelPlot =
function(object, k.blocks = 20, add = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates Return Levels Based on GEV Fit
    
    # FUNCTION:
    
    # @fit Slot:
    object = object@fit
    class(object) = "gevFit"
    
    # Use "rlevel.gev":
    ans = rlevel.gev(out = object$fit, k.blocks = k.blocks, add = add, ...)
    object$rlevel = 
        c(min = ans[1], v = ans[2], max = ans[3], "k.blocks" = k.blocks)
    
    # Return Value:
    invisible(object)
}


################################################################################
# MDA ESTIMATORS:


hillPlot = 
function(x, option = c("alpha", "xi", "quantile"), start = 15, end = NA,
reverse = FALSE, p = NA, ci = 0.95, autoscale = TRUE, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz
 
    # Description:
    #   Plots the results from the Hill Estimator.
    
    # Note:
    #   Imported from R-package evir

    # Settings:
    data = as.numeric(x)
    ordered = rev(sort(data))
    ordered = ordered[ordered > 0]
    n = length(ordered)
    option = match.arg(option)
    if ((option == "quantile") && (is.na(p)))
        stop("Input a value for the probability p")
    if ((option == "quantile") && (p < 1 - start/n)) {
        cat("Graph may look strange !! \n\n")
        cat(paste("Suggestion 1: Increase `p' above",
            format(signif (1 - start/n, 5)), "\n"))
        cat(paste("Suggestion 2: Increase `start' above ",
            ceiling(length(data) * (1 - p)), "\n"))
    }
    k = 1:n
    loggs = logb(ordered)
    avesumlog = cumsum(loggs)/(1:n)
    xihat = c(NA, (avesumlog - loggs)[2:n])
    alphahat = 1/xihat
    y = switch(option,
        alpha = alphahat,
        xi = xihat,
        quantile = ordered * ((n * (1 - p))/k)^(-1/alphahat))
    ses = y/sqrt(k)
    if (is.na(end)) end = n
    x = trunc(seq(from = min(end, length(data)), to = start))
    y = y[x]
    ylabel = option
    yrange = range(y)
    if (ci && (option != "quantile")) {
            qq = qnorm(1 - (1 - ci)/2)
            u = y + ses[x] * qq
            l = y - ses[x] * qq
            ylabel = paste(ylabel, " (CI, p =", ci, ")", sep = "")
            yrange = range(u, l)
    }
    if (option == "quantile") {
        ylabel = paste("Quantile, p =", p)
    }
    index = x
    if (reverse) {
        index =  - x
    }
    if (autoscale) {
        plot(index, y, ylim = yrange, type = "l", xlab = "", ylab = "",
            axes = FALSE, ...)
    } else {
        plot(index, y, type = "l", xlab = "", ylab = "", 
            axes = FALSE, ...)
    }
    axis(1, at = index, lab = paste(x), tick = FALSE)
    axis(2)
    threshold = findThreshold(data, x)
    axis(3, at = index, lab = paste(format(signif (threshold, 3))),
        tick = FALSE)
    box()
    if (ci && (option != "quantile")) {
        lines(index, u, lty = 2, col = 2)
        lines(index, l, lty = 2, col = 2)
    }
    if (labels) {
        title(xlab = "Order Statistics", ylab = ylabel)
        mtext("Threshold", side = 3, line = 3)
    }
    
    # Return Value:
    invisible(list(x = index, y = y))
}


# ------------------------------------------------------------------------------


shaparmPlot = 
function (x, revert = FALSE, standardize = FALSE, tails = 0.01*(1:10), 
doplot = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 
which = c(TRUE, TRUE, TRUE), doprint = TRUE, both.tails = TRUE, 
xi.range = c(-0.5, 1.5), alpha.range = c(0, 10))
{   # A function written by D. Wuertz 
    
    # Description:
    #   Displays Pickands, Einmal-Decker-deHaan, and Hill
    #   estimators together with several plot variants.
    
    # FUNCTION:
    
    # Settings:
    select.doplot = which
    if (revert) x = -x
    if (standardize) x = (x-mean(x))/sqrt(var(x))
    ylim1 = xi.range
    ylim2 = alpha.range
    z = rep(mean(ylim2), length(tails))
    ylim1 = xi.range
    ylim2 = alpha.range
    p1 = p2 = h1 = h2 = d1 = d2 = m1 = m2 = rep(0,length(tails))
    for ( i in (1:length(tails)) ) {
        tail = tails[i]
        # Printing/Plotting Staff:
        if (doprint) cat("Taildepth: ", tail, "\n")
        if (select.doplot[1]) {
            xi = shaparmPickands (x, tail, ylim1, doplot=doplot[i], 
            both.tails, ) 
            p1[i] = xi$xi[1]; p2[i] = xi$xi[3] 
        }
        if (select.doplot[2]) { 
            xi = shaparmHill (x, tail, ylim1, doplot=doplot[i], 
            both.tails) 
            h1[i] = xi$xi[1]; h2[i] = xi$xi[3] 
        }
        if (select.doplot[3]) {
            xi = shaparmDEHaan (x, tail, ylim1, doplot=doplot[i], 
            both.tails)
            d1[i] = xi$xi[1]; d2[i] = xi$xi[3] 
        }      
        if (doprint) {
            cat("Pickands - Hill - DeckerEinmaalDeHaan: \n")
            print(c(p1[i], h1[i], d1[i]))
            if (both.tails) print(c(p2[i], h2[i], d2[i]))
        } 
            cat("\n") 
    }
    
    # Plot Pickands' Summary:
    if (select.doplot[1]) { 
        plot (tails, z, type="n", xlab="tail depth", ylab="alpha",
            ylim=ylim2, main="Pickands Summary")
            y1 = 1/p1
            x1 = tails [y1>ylim2[1] & y1<ylim2[2]]
            y1 = y1 [y1>ylim2[1] & y1<ylim2[2]]
            points (x1, y1, col=2); lines(x1, y1, col=2)
        if (both.tails) { 
            y1 = 1/p2
            x1 = tails [y1>ylim2[1] & y1<ylim2[2]]
            y1 = y1 [y1>ylim2[1] & y1<ylim2[2]]
            points (x1, y1, col=3); lines(x1, y1, col=3)
        } 
    }
    
    # Plot Hill Summary:
    if (select.doplot[2]) { 
        plot (tails, z, type="n", xlab="tail depth", ylab="alpha", 
            ylim=ylim2, main="Hill Summary")
            y1 = 1/h1
            x1 = tails [y1>ylim2[1] & y1<ylim2[2]]
            y1 = y1 [y1>ylim2[1] & y1<ylim2[2]]
            points (x1, y1, col=2); lines(x1, y1, col=2)
        if (both.tails) { 
            y1 = 1/h2
            x1 = tails [y1>ylim2[1] & y1<ylim2[2]]
            y1 = y1 [y1>ylim2[1] & y1<ylim2[2]]
            points (x1, y1, col=3); lines(x1, y1, col=3)
        } 
    }
    
    # Plot Deckers-Einmahl-deHaan Summary
    if (select.doplot[3]) { 
        plot (tails, z, type = "n", xlab = "tail depth", ylab = "alpha", 
            ylim = ylim2, main = "Deckers-Einmahl-deHaan Summary")
        y1 = 1/d1
        x1 = tails [y1>ylim2[1] & y1<ylim2[2]]
        y1 = y1 [y1>ylim2[1] & y1<ylim2[2]]
        points (x1, y1, col=2)
        lines(x1, y1, col = 2) 
        if (both.tails) { 
            y1 = 1/d2
            x1 = tails [y1>ylim2[1] & y1<ylim2[2]]
            y1 = y1 [y1>ylim2[1] & y1<ylim2[2]]
            points (x1, y1, col=3); lines(x1, y1, col = 3)
        } 
    }
    
    # Return Value:
    lower = list(pickand = p1, hill = h1, dehaan = d1)
    if (both.tails) {
        upper = list(pickands = p2, hill = h2, dehaan = d2)
        result = list(tails = tails, lower = lower, upper = upper) 
    } else {
        result = list(tails = tails, lower = lower) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


shaparmPickands = 
function (x, tail, yrange, doplot = TRUE, both.tails = TRUE, ...)     
{   # A function written by D. Wuertz
    
    # FUNCTION:
    
    # Order Residuals:
    ordered1 = rev(sort(abs(x[x < 0])))
    if (both.tails) ordered2 = rev(sort(abs(x[x > 0])))
    n1 = length(ordered1)
    if (both.tails) n2 = length(ordered2) 
    ordered1 = ordered1[1:floor(tail*n1)]    
    if (both.tails) ordered2 = ordered2[1:floor(tail*n2)]
    n1 = length(ordered1)
    if (both.tails) n2 = length(ordered2)
    
    # Pickands Estimate:    
    k1 = 1:(n1%/%4)
    if (both.tails) k2 = 1:(n2%/%4) 
    pickands1 = log ((c(ordered1[k1])-c(ordered1[2*k1])) /
        (c(ordered1[2*k1])-c(ordered1[4*k1]))) / log(2)
    if (both.tails) pickands2 = log ((c(ordered2[k2])-c(ordered2[2*k2])) /
        (c(ordered2[2*k2])-c(ordered2[4*k2]))) / log(2)
    
    # Prepare Plot:
        y1 = pickands1[pickands1 > yrange[1] & pickands1 < yrange[2]]
        x1 = log10(1:length(pickands1))[pickands1 > yrange[1] & 
            pickands1 < yrange[2]]
    if (both.tails) {
        y2 = pickands2[pickands2 > yrange[1] & pickands2 < yrange[2]]
        x2 = log10(1:length(pickands2))[pickands2 > yrange[1] & 
            pickands2 < yrange[2]] }
    if (doplot) { 
            par(err=-1)
        plot (x1, y1, xlab="log scale", ylab="xi", ylim=yrange, 
            main="Pickands Estimator", type="n")  
        title(sub=paste("tail depth:", as.character(tail)))
            lines(x1, y1, type="p", pch=2, col=2)
            if (both.tails) lines(x2, y2, type="p", pch=6, col=3) }
    
    # Calculate invers "xi":
    my1 = mean(y1, na.rm = TRUE)
    if (both.tails) my2 = mean(y2, na.rm = TRUE)
    sy1 = sqrt(var(y1, na.rm = TRUE))
    if (both.tails) sy2 = sqrt(var(y2, na.rm = TRUE))
    
    # Plot:
    if (doplot) {
        par(err = -1)
        lines(c(x1[1], x1[length(x1)]), c(my1,my1), type = "l", 
        lty=1, col=2)
        if (both.tails) lines(c(x2[1], x2[length(x2)]), c(my2, my2), 
            type = "l", lty = 1, col = 3) 
    }
    
    # Result: 
    result = list(xi=c(my1, sy1))   
    if (both.tails) result = list(xi=c(my1, sy1, my2, sy2))
    
    # Return Result:
    result
}
 

# ------------------------------------------------------------------------------

   
shaparmHill = 
function (x, tail, yrange, doplot = TRUE, both.tails = TRUE, ...)     
{   # A Function written by D. Wuertz
    
    # ORDER RESIDUALS:
    ordered1 = rev(sort(abs(x[x < 0])))
    if (both.tails) ordered2 = rev(sort(abs(x[x > 0])))
    n1 = length(ordered1)
    if (both.tails) n2 = length(ordered2) 
    ordered1 = ordered1[1:floor(tail*n1)]    
    if (both.tails) ordered2 = ordered2[1:floor(tail*n2)]
    n1 = length(ordered1)
    if (both.tails) n2 = length(ordered2)
    
    # HILLS ESTIMATE:
    hills1 = c((cumsum(log(ordered1))/(1:n1)-log(ordered1))[2:n1])     
    if (both.tails) hills2 = c((cumsum(log(ordered2))/(1:n2) -
        log(ordered2))[2:n2])
        
    # PREPARE PLOT:
    y1 = hills1[hills1 > yrange[1] & hills1 < yrange[2]]
    x1 = log10(1:length(hills1))[hills1 > yrange[1] & hills1 < yrange[2]]
    if (both.tails) {
        y2 = hills2[hills2 > yrange[1] & hills2 < yrange[2]]
        x2 = log10(1:length(hills2))[hills2 > yrange[1] & hills2 < yrange[2]]
    }
    if (doplot) {
        par(err=-1)
        plot (x1, y1, xlab="log scale", ylab="xi", ylim=yrange, 
            main="Hill Estimator", type="n")
        title(sub=paste("tail depth:", as.character(tail)))
        lines(x1, y1, type="p", pch=2, col=2)
        if (both.tails) lines(x2, y2, type="p", pch=6, col=3) 
    }
    
    # CALCULATE INVERSE XI:
    my1 = mean(y1, na.rm = TRUE)
    if (both.tails) my2 = mean(y2, na.rm = TRUE)
    sy1 = sqrt(var(y1, na.rm = TRUE))
    if (both.tails) sy2 = sqrt(var(y2, na.rm = TRUE))
    if (doplot) {
        par(err=-1)
        lines(c(x1[1], x1[length(x1)]), c(my1,my1), type="l",
        lty=1, col=2)
        if (both.tails) lines(c(x2[1], x2[length(x2)]), c(my2,my2), 
        type="l",lty=1, col=3) 
    }
    
    # Result:
    result = list(xi=c(my1, sy1))   
    if (both.tails) result = list(xi=c(my1, sy1, my2, sy2))
    
    # Return Result:
    result       
}
       

# ------------------------------------------------------------------------------


shaparmDEHaan = 
function (x, tail, yrange, doplot = TRUE, both.tails = TRUE, ...)     
{   # A function written by D. Wuertz
    
    # ORDER RESIDUALS:
    ordered1 = rev(sort(abs(x[x < 0])))
    if (both.tails) ordered2 = rev(sort(abs(x[x > 0])))
    n1 = length(ordered1)
    if (both.tails) n2 = length(ordered2) 
    ordered1 = ordered1[1:floor(tail*n1)]    
    if (both.tails) ordered2 = ordered2[1:floor(tail*n2)]
    n1 = length(ordered1)
    if (both.tails) n2 = length(ordered2)
    
    # DECKERS-EINMAHL-deHAAN ESTIMATE:
    ns0 = 1
    n1m = n1-1; ns1 = ns0; ns1p = ns1+1
    bod1 = c( cumsum(log(ordered1))[ns1:n1m]/(ns1:n1m) -
            log(ordered1)[ns1p:n1] ) 
    bid1 = c( cumsum((log(ordered1))^2)[ns1:n1m]/(ns1:n1m) -
            2*cumsum(log(ordered1))[ns1:n1m]*log(ordered1)[ns1p:n1]/(ns1:n1m) +
            ((log(ordered1))^2)[ns1p:n1] )
    dehaan1 = ( 1.0 + bod1 + ( 0.5 / (  bod1^2/bid1 - 1 ) ))
    if (both.tails) {
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
    if (both.tails) {
        y2 = dehaan2[dehaan2 > yrange[1] & dehaan2 < yrange[2]]
        x2 = log10(1:length(dehaan2))[dehaan2 > yrange[1] & 
            dehaan2 < yrange[2]] 
    }
    if (doplot) { 
        par(err=-1)
        plot (x1, y1, xlab="log scale", ylab="xi", ylim=yrange,
            main="Deckers - Einmahl - de Haan Estimator", type="n")
        title(sub=paste("tail depth:", as.character(tail)))
        lines(x1, y1, type="p", pch=2, col=2)
        if (both.tails) lines(x2, y2, type="p", pch=6, col=3) 
    }
            
    # CALCULATE INVERSE XI:
    my1 = mean(y1, na.rm = TRUE)
    if (both.tails) my2 = mean(y2, na.rm = TRUE)
    sy1 = sqrt(var(y1, na.rm = TRUE))
    if (both.tails) sy2 = sqrt(var(y2, na.rm = TRUE))
    if (doplot) {
        par(err=-1)
        lines(c(x1[1], x1[length(x1)]), c(my1,my1), type="l", 
            lty=1, col=2)
        if (both.tails) lines(c(x2[1], x2[length(x2)]), c(my2, my2), 
        type = "l", lty = 1, col = 3) 
    }
        
    # Result:
    result = list(xi = c(my1, sy1))   
    if (both.tails) result = list(xi=c(my1, sy1, my2, sy2))
    
    # Return Result:
    result
}


################################################################################

