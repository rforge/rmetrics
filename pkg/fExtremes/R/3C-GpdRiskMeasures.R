
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
# FUNCTION:               ADDITIONAL PLOTS:
#  gpdTailPlot             Plots Tail Estimate From GPD Model
#  gpdQPlot                Adds Quantile Estimates to gpdTailPlot()
#  .gpd.q                   Internal plot function called by gpdQPlot()
#  gpdSfallPlot            Adds Expected Shortfall Estimates to a GPD Plot
#  .gpd.sfall               Internal plot function called by gpdQPlot()
#  gpdQuantPlot            Plots of GPD Tail Estimate of a High Quantile
#  gpdShapePlot            Plots for GPD Shape Parameter
# FUNCTION:               ADDITIONAL FUNCTION:
#  gpdriskMeasures         Calculates Quantiles and Expected Shortfalls
################################################################################


gpdTailPlot =
function(object, plottype = c("xy", "x", "y", ""), extend = 1.5, 
labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots tail estimate from GPD model
    
    # Arguments:
    #   object - an object of class 'fGPDFIT'
    
    # Note:
    #   Code partly copied from R package evir
    
    # Example:
    #   object = gpdFit(as.timeSeries(data(danishClaims)), u = 10)
    #   gpdTailPlot(object)
    
    # FUNCTION:   
    
    # Settings:
    threshold = object@fit$threshold
    x = as.vector(object@data$x)
    data = x[x > threshold]
    xi = as.numeric(object@fit$par.ests["xi"])
    beta = as.numeric(object@fit$par.ests["beta"])
    
    # Points:
    plotmin = threshold
    plotmax = max(data) * max(1, extend)
    z = qgpd(seq(from = 0, to = 1, length = 501), xi, threshold, beta)
    z = pmax(pmin(z, plotmax), plotmin)    
    invProb = 1 - length(data)/length(x)
    ypoints = invProb*(1-ppoints(sort(data)))
    y = invProb*(1-pgpd(z, xi, threshold, beta))

    # Parameters:
    shape = xi
    scale = beta * invProb^xi
    location = threshold - (scale*(invProb^(- xi)-1))/xi
    
    # Plot:
    plot(sort(data), ypoints, xlim = range(plotmin, plotmax), 
         ylim = range(ypoints, y, na.rm = TRUE), col = "steelblue",
         pch = 19, xlab = "", ylab = "", log = plottype[1], axes = TRUE, ...)
    lines(z[y >= 0], y[y >= 0])
    grid()
    
    # Labels:
    alog = plottype[1]
    if (labels) {
        xLab = "x"
        if (alog == "x" || alog == "xy" || alog == "yx")
            xLab = paste(xLab, "(on log scale)")
        yLab = "1-F(x)"
        if (alog == "xy" || alog == "yx" || alog == "y")
            yLab = paste(yLab, "(on log scale)")
        title(xlab = xLab, ylab = yLab)
        title(main = "Tail Estimate Plot")
    }
    
    # Object:
    object@fit$n = length(x)
    object@fit$data = fit@data$exceedances
    object@fit$n.exceed = length(object@fit$data)
    if(fit@method[2] == "mle") object@fit$method = "ml" else object@fit$method = ""
    
    # Result:
    ans = list(lastfit = object@fit, type = "tail", dist = "gpd",
        plotmin = plotmin, plotmax = plotmax, alog = plottype[1], 
        location = location, shape = shape, scale = scale)
    
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------

gpdQPlot =
function(x, p = 0.99, ci = 0.95, type = c("likelihood", "wald"), like.num = 50)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Adds Expected Shortfall Estimates to a GPD Plot 
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   pp - the probability level
    
    # FUNCTION:

    # Return Value:
    .gpd.q(x = x, pp = p, ci.type = type[1], ci.p = ci, like.num = like.num)
}


# ------------------------------------------------------------------------------


.gpd.q = 
function (x, pp, ci.type = c("likelihood", "wald"), ci.p = 0.95, 
like.num = 50) 
{   # A copy from evir

    # FUNCTION:
    
    if (x$dist != "gpd") 
        stop("This function is used only with GPD curves")
    if (length(pp) > 1) 
        stop("One probability at a time please")
    threshold = x$lastfit$threshold
    par.ests = x$lastfit$par.ests
    xihat = par.ests["xi"]
    betahat = par.ests["beta"]
    varcov = x$lastfit$varcov
    p.less.thresh = x$lastfit$p.less.thresh
    lambda = 1
    if (x$type == "tail") lambda = 1/(1 - p.less.thresh)
    a = lambda * (1 - pp)
    gfunc = function(a, xihat) (a^(-xihat) - 1)/xihat
    gfunc.deriv = function(a, xihat) (-(a^(-xihat) - 1)/xihat - 
        a^(-xihat) * logb(a))/xihat
    q = threshold + betahat * gfunc(a, xihat)
    if (q < x$plotmax) abline(v = q, lty = 2)
    out = as.numeric(q)
    ci.type = match.arg(ci.type)
    if (ci.type == "wald") {
        if (class(x$lastfit) != "gpd") 
            stop("Wald method requires model be fitted with gpd (not pot)")
        scaling = threshold
        betahat = betahat/scaling
        xivar = varcov[1, 1]
        betavar = varcov[2, 2]/(scaling^2)
        covar = varcov[1, 2]/scaling
        term1 = betavar * (gfunc(a, xihat))^2
        term2 = xivar * (betahat^2) * (gfunc.deriv(a, xihat))^2
        term3 = 2 * covar * betavar * gfunc(a, xihat) * gfunc.deriv(a, xihat)
        qvar = term1 + term2 + term3
        if (qvar < 0) 
            stop("Negative estimate of quantile variance")
        qse = scaling * sqrt(qvar)
        qq = qnorm(1 - (1 - ci.p)/2)
        upper = q + qse * qq
        lower = q - qse * qq
        if (upper < x$plotmax) 
            abline(v = upper, lty = 2, col = 2)
        if (lower < x$plotmax) 
            abline(v = lower, lty = 2, col = 2)
        out = as.numeric(c(lower, q, qse, upper))
        names(out) = c("Lower CI", "Estimate", "Std.Err", "Upper CI")
    }
    if (ci.type == "likelihood") {
        parloglik = 
        function(theta, tmp, a, threshold, xpi) {
            beta = (theta * (xpi - threshold))/(a^(-theta) - 
                1)
            if ((beta <= 0) || ((theta <= 0) && (max(tmp) > (-beta/theta)))) 
                f = 1e+06
            else {
                y = logb(1 + (theta * tmp)/beta)
                y = y/theta
                f = length(tmp) * logb(beta) + (1 + theta) * sum(y)
            }
            if(is.na(f)) f = 1e+6
            f
        }
        theta = xihat
        parmax = NULL
        xp = exp(seq(from = logb(threshold), to = logb(x$plotmax), 
            length = like.num))
        excess = as.numeric(x$lastfit$data - threshold)
        for (i in 1:length(xp)) {
            fit2 = optim(theta, parloglik, method = "BFGS", 
                hessian = FALSE, tmp = excess, a = a, threshold = threshold, 
                xpi = xp[i])
            parmax = rbind(parmax, fit2$value)
        }
        parmax = -parmax
        overallmax = -parloglik(xihat, excess, a, threshold, q)
        crit = overallmax - qchisq(0.999, 1)/2
        cond = parmax > crit
        xp = xp[cond]
        parmax = parmax[cond]
        par(new = T)
        dolog = ""
        if (x$alog == "xy" || x$alog == "x") dolog = "x"
        plot(xp, parmax, type = "n", xlab = "", ylab = "", axes = F, 
            xlim = range(x$plotmin, x$plotmax), ylim = range(overallmax, 
                crit), log = dolog)
        axis(4, at = overallmax - qchisq(c(0.95, 0.99), 1)/2, 
            labels = c("95", "99"), tick = TRUE)
        aalpha = qchisq(ci.p, 1)
        abline(h = overallmax - aalpha/2, lty = 2, col = 2)
        cond = !is.na(xp) & !is.na(parmax)
        smth = spline(xp[cond], parmax[cond], n = 200)
        lines(smth, lty = 2, col = 2)
        ci = smth$x[smth$y > overallmax - aalpha/2]
        out = c(min(ci), q, max(ci))
        names(out) = c("Lower CI", "Estimate", "Upper CI")
    }
    
    # Return Value:
    out
}


# ------------------------------------------------------------------------------


gpdSfallPlot =
function(x, p = 0.99, ci = 0.95, like.num = 50)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Adds Expected Shortfall Estimates to a GPD Plot 
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   pp - the probability level
    
    # FUNCTION:

    # Return Value:
    .gpd.sfall(x = x, pp = p, ci.p = ci, like.num = like.num)
}


# ------------------------------------------------------------------------------


.gpd.sfall =  
function(x, pp = 0.99, ci.p = 0.95, like.num = 50)
{   # A copy from evir

    # Arguments:
    #   x - a list object returned by 'plot.gpd' or 'tailplot'
    #   pp - the desired probability for expected shortfall 
    #       estimate (e.g. 0.99 for the 99th percentile)
    #   ci.p - probability for confidence interval (must be 
    #       less than 0.999)
    #   like.num - number of times to evaluate profile likelihood

    # data(danish); gpd.sfall(tailplot(gpd(danish, 10)), 0.99)

    # FUNCTION:
    
    if(x$dist != "gpd")
        stop("This function is used only with GPD curves")
    if(length(pp) > 1)
        stop("One probability at a time please")
      
    object = gpdFit(as.timeSeries(danishClaims), 10)  
    threshold = object@fit$threshold
    par.ests = object@fit$par.ests
    xihat = par.ests["xi"]
    betahat = par.ests["beta"]
    varcov = object@fit$varcov
    
    p.less.thresh = object@fit$prob
    lambda = 1
    
    # if (x$type == "tail") 
    lambda = 1/(1 - p.less.thresh)
    a = lambda * (1 - pp)
    gfunc = function(a, xihat) (a^( - xihat) - 1) / xihat
    q = threshold + betahat * gfunc(a, xihat)
    s = q + (betahat + xihat * (q - threshold))/(1 - xihat)
    if (s < x$plotmax) abline(v = s, lty = 2)
    out = as.numeric(s)
    
    parloglik = function(theta, tmp, a, threshold, xpi)
    {
        beta = ((1 - theta) * (xpi - threshold)) /
            (((a^( - theta) - 1)/theta) + 1)
        if((beta <= 0) || ((theta <= 0) && (max(tmp) > ( - beta/theta)))) {
            f = 1e+06
        } else {
            y = log(1 + (theta * tmp)/beta)
            y = y/theta
            f = length(tmp) * log(beta) + (1 + theta) * sum(y)
        }
        f
    }
    
    theta = xihat
    parmax = NULL
    xp = exp(seq(from = log(threshold), to = log(x$plotmax),
        length = like.num))
    excess = as.numeric(x$lastfit$data - threshold)
    
    for (i in 1:length(xp)) {
        fit2 = optim(theta, parloglik, method = "BFGS", hessian = FALSE,
            tmp = excess, a = a, threshold = threshold, xpi = xp[i])
        parmax = rbind(parmax, fit2$value)
    }
    
    parmax =  - parmax
    overallmax =  - parloglik(xihat, excess, a, threshold, s)
    crit = overallmax - qchisq(0.999, 1)/2
    cond = parmax > crit
    xp = xp[cond]
    parmax = parmax[cond]
    par(new = TRUE)
    dolog = ""
    
    if(x$alog == "xy" || x$alog == "x") dolog = "x"
    
    plot(xp, parmax, type = "n", xlab = "", ylab = "", axes = FALSE, 
        xlim = range(x$plotmin, x$plotmax), 
        ylim = range(overallmax, crit), log = dolog)
    axis(4, at = overallmax - qchisq(c(0.95, 0.99), 1)/2,
        labels = c("95", "99"), tick = TRUE)
        
    aalpha = qchisq(ci.p, 1)
    abline(h = overallmax - aalpha/2, lty = 2, col = 2)
    cond = !is.na(xp) & !is.na(parmax)
    smth = spline(xp[cond], parmax[cond], n = 200)
    lines(smth, lty = 2, col = 2)
    ci = smth$x[smth$y > overallmax - aalpha/2]
    
    out = c(min(ci), s, max(ci))
    names(out) = c("Lower CI", "Estimate", "Upper CI")
    
    # Return Value:
    out
}


# ------------------------------------------------------------------------------


gpdQuantPlot =
function(x, p = 0.99, ci = 0.95, models = 30, start = 15, end = 500,
doplot = TRUE, plottype = c("normal", "reverse"), labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots of GPD Tail Estimate of a High Quantile
    
    # Example:
    #   Danish = as.timeSeries(data(danishClaims))
    #   gpdquantPlot(Danish)
    
    # Note:
    #   Code partly copied from R package evir
    
    # FUNCTION:

    # Settings:
    data = as.vector(x)
    n = length(data)
    exceed = trunc(seq(from = min(end, n), to = start, length = models))
    p = max(p, 1 - min(exceed)/n)
    start = max(start, ceiling(length(data) * (1 - p)))

    # Internal Function:
    .quantFit = function(nex, data) {
        prob = 1 - nex/length(as.vector(data))
        fit = gpdFit(data, u = quantile(data, prob))@fit
        c(fit$threshold, fit$par.ests, fit$par.ses, 
            as.vector(fit$varcov)[c(1,4,2)])
    }
    
    # Compute:
    mat = apply(as.matrix(exceed), 1, .quantFit, data = data)
    thresh = mat[1, ]
    xihat = mat[2, ]
    betahat = mat[3, ]
    lambda = length(data)/exceed
    a = lambda * (1 - p)
    gfunc = function(a, xihat) (a^( - xihat) - 1) / xihat
    qest = thresh + betahat * gfunc(a, xihat)
    l = u = qest
    yrange = range(qest)
    
    # Add Confidence Intervals:
    if (ci) {
        qq = qnorm(1 - (1 - ci)/2)
        xivar = mat[4, ]
        betavar = mat[5,  ]
        covar = mat[6,  ]
        scaling = thresh
        betahat = betahat/scaling
        betavar = betavar/(scaling^2)
        covar = covar/scaling
        gfunc.deriv = function(a, xihat)
            (-(a^(-xihat)-1)/xihat-a^(-xihat)*log(a))/xihat
        term1 = betavar * (gfunc(a, xihat))^2
        term2 = xivar * (betahat^2) * (gfunc.deriv(a, xihat))^2
        term3 = 2 * covar * betavar * gfunc(a, xihat) * gfunc.deriv(a, xihat)
        qvar = term1 + term2 + term3
        if (min(qvar) < 0)
            stop(paste(
                "Conditioning problems lead to estimated negative",
                "quantile variance", sep = "\n"))
        qse = scaling * sqrt(qvar)
        u = qest + qse * qq
        l = qest - qse * qq
        yrange = range(qest, u, l)
    }
    
    # Result matrix:
    mat = rbind(thresh, qest, exceed, l, u)
    rownames(mat) = c("threshold", "qest", "exceedances", "lower", "upper")
    colnames(mat) = paste(1:dim(mat)[2])
    
    # Plot:
    if (doplot) {
        if (plottype[1] == "normal") {
            index = exceed
        } else if (plottype[1] == "reverse") {
            index =  -exceed
        }
        plot(index, qest, ylim = yrange, type = "l", xlab = "", ylab = "",
            axes = FALSE)
        axis(1, at = index, lab = paste(exceed))
        axis(2)
        axis(3, at = index, lab = paste(format(signif (thresh, 3))))
        box()
        if (ci) {
            lines(index, l, lty = 2, col = "steelblue")
            lines(index, u, lty = 2, col = "steelblue")
        }
        if (labels) {
            title(xlab = "Exceedances",
                ylab = paste("Quantiles:", substitute(x)))
            mtext("Threshold", side = 3, line = 3)
        }
        p = round(p, 3)
        ci = round(ci, 3)
        text = paste("p =", p, "| ci =", ci, "| start =", 
            start, "| end =", end )
        mtext(text, side = 4, adj = 0, cex = 0.7)
    }
    
    # Add Attributes:
    mat = t(mat)
    attr(mat, "control") = data.frame(cbind(p = p, ci = ci, 
        start = start, end = end), row.names = "")
    
    # Return Value:
    invisible(mat)
}


# ------------------------------------------------------------------------------


gpdShapePlot =
function(x, ci = 0.95, models = 30, start = 15, end = 500,  
doplot = TRUE, plottype = c("normal", "reverse"), labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots for GPD Shape Parameter
    
    # Example:
    
    # FUNCTION:

    # Settings:
    data = as.vector(x)
    X = trunc(seq(from = min(end, length(data)), to = start, length = models))
    
    # Internal Function:
    .shapeFit = function(nex, data) {
        prob = 1 - nex/length(as.vector(data))
        fit = gpdFit(data, u = quantile(data, prob), 
            information = "expected")@fit
        c(fit$threshold, fit$par.ests[1], fit$par.ses[1]) 
    }
    
    # Result Matrix:
    mat = apply(as.matrix(X), 1, .shapeFit, data = data)
    mat = rbind(mat, X)
    rownames(mat) = c("threshold", "shape", "se", "exceedances")
    colnames(mat) = paste(1:dim(mat)[2])

    # Plot:
    if (doplot) {
        thresh = mat[1, ]
        y = mat[2, ]
        yrange = range(y)
        if (plottype[1] == "normal") {
            index = X
        } else if (plottype == "reverse") {
            index =  -X
        }
        if (ci) {
            sd = mat[3, ] * qnorm(1 - (1 - ci)/2)
            yrange = range(y, y + sd, y - sd)
        }
        plot(index, y, ylim = yrange, type = "l", xlab = "", ylab = "",
            axes = FALSE)
        axis(1, at = index, lab = paste(X))
        axis(2)
        axis(3, at = index, lab = paste(format(signif(thresh, 3))))
        box()
        grid()
        if (ci) {
            sd = mat[3, ] * qnorm(1 - (1 - ci)/2)
            yrange = range(y, y + sd, y - sd)
            lines(index, y + sd, lty = 2, col = "steelblue")
            lines(index, y - sd, lty = 2, col = "steelblue")
        }
        if (labels) {
            title(xlab = "Exceedances",
                ylab = paste("Shape:", substitute(x)))
            mtext("Threshold", side = 3, line = 3)
        }
        text = paste("ci =", ci, "| start =", start, "| end =", end )
        mtext(text, side = 4, adj = 0, cex = 0.7)
    }
    
    # Add Attributes:
    attr(mat, "control") = data.frame(cbind(ci = ci, 
        start = start, end = end), row.names = "")
    mat = t(mat)
    
    # Return Value:
    invisible(mat)
}


# ------------------------------------------------------------------------------


gpdRiskMeasures = 
function(object, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates Quantiles and Expected Shortfalls
    
    # Arguments:
    #   x  - an object of class 'gpdFit'
    #   prob - a numeric value or vector of probability levels 
    
    # FUNCTION:
    
    # Settings:
    u = object@parameter$u
    par.ests = object@fit$par.ests
    xi = par.ests["xi"]
    beta = par.ests["beta"]
    lambda = 1/(1 - object@fit$prob)
    
    # Quantile Risk:
    q = u + (beta * ((lambda * (1 - prob))^( - xi) - 1))/xi 

    # Shortfall Risk:  
    es = (q * (1 + (beta - xi * u)/q)) / (1 - xi) 
  
    # Risk Matrix:
    ans = data.frame(p = prob, quantile = q, shortfall = es)
    
    # Return Value:
    ans
}


################################################################################

