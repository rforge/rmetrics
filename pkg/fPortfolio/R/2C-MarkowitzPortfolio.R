
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

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
# FUNCTION:                 EFFICIENT FRONTIER:
#  'fPFOLIO'                 S4 Portfolio Class
#  portfolioMarkowitz        Mean-variance Markowitz (target) portfolio
#  frontierMarkowitz         Efficient frontier of mean-var portfolio
#  montecarloMarkowitz       Adds randomly created portfolios
# METHODS:                  DESCRIPTION:   
#  print.fPFOLIO             S3: Print method for objects of class fPFOLIO
#  plot.fPFOLIO              S3: Plot method for objects of class fPFOLIO
#  summary.fPFOLIO           S3: Summary method for objects of class fPFOLIO
# EXTRACTOR FUNCTION:       DESCRIPTION:
#  .frontier.default         S3: Extract points on the efficient frontier
# FUNCTIONS:                SPECIAL PORTFOLIOS:
#  .tangencyMarkowitz        Adds tangency portfolio
#  .equalweightsMarkowitz    Adds equal weights Portfolio
#  .frontierShortSelling     Efficient Frontier of Short Selling Markowitz
# FUNCTIONS:                BUILTIN FROM PACKAGE:
#  .portfolio.optim          Function from R-package tseries
#  .solve.QP                 Function from R-package quadprog
################################################################################



setClass("fPFOLIO", 
    representation(
        call = "call",
        method = "character",
        model = "character",
        data = "list",
        pfolio = "list",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------


portfolioMarkowitz = 
function(data, targetReturn, title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Optimizes a mean-var portfolio for a given desired return
    
    # Arguments:
    #   x - portfolio of assets, a matrix or an object which can be 
    #       transformed to a matrix
    #   targetReturn - the desired return, this target value must be 
    #       smaller than the maximum and larger than the minimum asset 
    #       return. Short selling is not allowed.

    # FUNCTION:
    
    # Check Data - Two Choices: 
    if (is.list(data)) {
        # Argument data is a list with entries list(mu, Sigma)
        x.mat = NA
        MU = data$mu
        COV = data$Sigma
        plottype = "mucov"
    } else {
        # Argument data is a rectangular time series object:
        x.mat = as.matrix(data)
        # Mean Vector  and Covariance Matrix:
        MU = apply(x.mat, MARGIN = 2, FUN = mean)
        COV = cov(x.mat)
        plottype = "series"
    }
    stopifnot(targetReturn >= min(MU))
    stopifnot(targetReturn <= max(MU))
    
    # Quadratic Programming:
    opt = .portfolio.optim(pm = targetReturn, returns = MU, covmat = COV)
    
    # Compose Result in a list:
    pfolio = list()
    pfolio$what = "portfolio"
    pfolio$method = "QP"
    pfolio$pw = opt$pw
    pfolio$pm = opt$pm
    pfolio$ps = opt$ps  
    # pfolio$returns = apply(x, 2, mean)
    pfolio$cov = COV  
    pfolio$r.range = c(min(MU), max(MU))
    pfolio$s.range = c(0, max(sqrt(diag(COV))))
    
    # Add Title: 
    if (is.null(title)) 
        title = "Mean-Variance Portfolio Optimization"
    
    # Add Description:
    if (is.null(description))
        description = .description()
    
    # Return Value:
    new ("fPFOLIO", 
        call = as.call(match.call()),
        method = "Quadratic Programming", 
        model = "Markowitz Portfolio",
        data = list(data = data), 
        pfolio = pfolio, 
        title = as.character(title),
        description = as.character(description)
    )     
}


# ------------------------------------------------------------------------------


frontierMarkowitz = 
function(data, Rf = 0, short = FALSE, length = 300, r.range = NULL, 
s.range = NULL, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the efficient frontier from a matrix
    #   of either market or simulated assets given in matrix "x". 
    #   Each time series represents a column in this matrix.
  
    # Arguments:     
    #   data - either a rectangular time series object which can be
    #       transformed into a matrix or a list with two named entries,
    #       mu - the returns of the multivariate series, and
    #       Sigma - the covariance matrix of the multivariate series.
    #   Rf - risk free rate, a numerical value by default 0.
    #   short - a logical flag, if set to TRUE short selling would be
    #       allowed otherwise not wich is the default setting.
    #   length - Number of equidistant return points on the Efficient 
    #       frontier.
    #   r.range - Plot range of returns, if NULL, the range will be created
    #       automatically.
    #   s.range - Plot range of standard deviations, if NULL, the range 
    #       will be created automatically.
  
    # Value:
    #   data - the matrix of asset time series
    #   ps - the portfolio standard deviation on the efficient frontier
    #   pm - the portfolio means on the efficient frontier
    #   cov - the covariance matrix of the assets
    #   r.range - the plot range for the mean returns, y-axis
    #   s.range - the plot range for the standard deviations, x-Axis
    #   diversification - number of portfolio weights on the efficient frontier
    #   call - the call string
    #   type - the type of portfolio optimizatiom applied
    #   series - the name of asset time series
    #   Rf, Rm, Sm, t.weights - not used, added by tangencyPortfolio
    
    # Notes:
    # * Portfolio optimization is done by a method specified by the
    #   argument type, at the moment this is "markowitz". No other
    #   methods are available so far.
    # * The plot range is determined by default automatically from  
    #   the market time series through r,range and s.range, if not 
    #   otherwise specified.
    # * The function returns an object of class portfolio with several
    #   components stored in a list.

    # FUNCTION:
    
    # Short Selling Allowed ?
    if (short) {
        return(.frontierShortSelling(data = data, Rf = Rf, 
            length = length, r.range = r.range, s.range = s.range, 
            title = title, description = description, ...))
    }

    # Check Data - Two Choices: 
    if (is.list(data)) {
        # data is a list with entries list(mu, Sigma)
        x.mat = NA
        mu = data$mu
        Sigma = data$Sigma
        plottype = "mucov"
    } else {
        # data is a rectangular time series object:
        x.mat = as.matrix(data)
        # Mean Vector  and Covariance Matrix:
        mu = apply(x.mat, MARGIN = 2, FUN = mean)
        Sigma = cov(x.mat)
        plottype = "series"
    }
    
    # Ranges for mean and Standard Deviation:
    if (is.null(r.range)) r.range = range(mu)
    if (is.null(s.range)) s.range = c(0, max(sqrt(diag(Sigma))))
  
    # Calculate Efficient Frontier:
    pmmin = min(r.range)
    pmmax = max(r.range)
    ps = pm = diversification = rep(0, times = length)
    eps = 1.0e-6
    k = 0
    for (PM in seq(pmmin+eps, pmmax-eps, length = length)) {
        k = k+1
        ef = .portfolio.optim(pm = PM, returns = mu, covmat = Sigma) 
        pm[k] = ef$pm
        ps[k] = ef$ps
        diversification[k] = length (ef$pw[ef$pw > 0.01])
    }

    # Result:
    pfolio = list(
        what = "frontier",
        plottype = plottype,
        data = data, 
        pw = NA, pm = pm, ps = ps, 
        returns = mu, cov = Sigma, 
        r.range = r.range, s.range = s.range, 
        Rf = NA, Rm = NA, Sm = NA, 
        Rew = NA, Sew = NA,
        t.weights = NA,
        diversification = diversification) 
        
    # Add Tangency Portfolio:
    pfolio = .tangencyMarkowitz(pfolio, Rf = Rf, add = FALSE) 
    
    # Add Equal Weights Portfolio:
    pfolio = .equalweightsMarkowitz(pfolio, add = FALSE)
    
    # Add Title: 
    if (is.null(title)) 
        title = "Mean-Variance Portfolio Optimization"
    
    # Add Description:
    if (is.null(description))
        description = .description()
    
    # Return Value:
    new ("fPFOLIO", 
        call = as.call(match.call()),
        method = "Quadratic Programming", 
        model = "Markowitz Portfolio",
        data = list(x = data), 
        pfolio = pfolio, 
        title = as.character(title),
        description = as.character(description)
    )     
}


# ------------------------------------------------------------------------------


montecarloMarkowitz = 
function(object, mc = 5000, doplot = FALSE, add = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   This function adds by Monte carlo simulation to an effiicient 
    #   frontier plot randomly selected portfolios and determines and
    #   plots a MC efficient frontier.
    
    # Arguments:
    #   object - an object of class 'fPFOLIO'
    #   mc - Number of Monte Carlo steps
    
    # Value:
    #   An updated object of class 'fPFOLIO' with the following 
    #   components added/updated:
    #   mcpm - Monte Carlo mean returns on MC efficient frontier
    #   mcps - Monte Carlo standard deviations on MC efficient forntier

    # FUNCTION:
    
    # Extract Portfolio:
    keep = object
    object = object@pfolio
    
    # Internal function:
    records = 
    function (data, conf.level = 0.95, ...) {
        data = as.numeric(data)
        record = cummax(data)
        expected = cumsum(1/(1:length(data)))
        se = sqrt(expected - cumsum(1/((1:length(data))^2)))
        trial = (1:length(data))[!duplicated(record)]
        record = unique(record)
        number = 1:length(record)
        expected = expected[trial]
        se = se[trial]
        data.frame(number, record, trial, expected, se)
    }
       
    # Settings:
    returns = object$returns
    covar = object$cov
    dimension = length(returns)  
    r.range = object$r.range
    s.range = object$s.range
                    
    # Plot frame:
    if (doplot) {
        plot(x = s.range, y = r.range, type = "n", 
            xlab = "Standard Deviation", ylab = "Return",  ...) 
    }
    
    # Monte Carlo Loop:
    rr = ss = rep(0, times = mc)
    for (k in 1:mc) {  
        weights = abs(rcauchy(dimension))
        weights = weights/sum(weights)           
        rp = weights %*% returns    
        sp = weights %*% covar %*% weights
        sp = sqrt(sp)   
        if (add) points(sp, rp, cex = 0.1)
        ss[k] = sp
        rr[k] = rp 
    }
    
    # Simulated Efficient Frontier:
    # Lower Part:
    rr = -rr
    order = order(ss)
    xsp.lower = ss[order]
    xrp.lower = rr[order]
    trial = records(xrp.lower)$trial
    lines(xsp.lower[trial], -xrp.lower[trial], col = "red")
    # Upper Part:
    rr = -rr
    order = order(ss)
    xsp.upper = ss[order]
    xrp.upper = rr[order]
    trial = records(xrp.upper)$trial
    lines(xsp.upper[trial], xrp.upper[trial], col = "red")
    lines(x = s.range, y = mean(rr)*c(1,1), col = "green")
    
    # Renew Asset Texts:
    if (add) {
      points(sqrt(diag(covar)), returns, col = "red")
      s.delta = diff(s.range)/50
      text(sqrt(diag(covar))+s.delta, returns, as.character(1:dimension), 
        col = "red") 
    }
    
    # Return Monte Carlo Frontier:
    object$mcpm = c(rev(xrp.lower[trial]), xrp.upper[trial])
    object$mcps = c(rev(xsp.lower[trial]), xrp.upper[trial]) 
    
    # Return Value
    invisible(keep)
}


# ******************************************************************************


print.fPFOLIO =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Print Method for an object of class "fPFOLIO"
    
    # Arguments:
    #   x - an object of class "fPFOLIO"
    
    # FUNCTION:
    
    # Extract Portfolio:
    pfolio = x@pfolio
     
    # Title:
    cat("\nTitle:\n ")
    cat(as.character(x@title), "\n")
    
    # Call:
    cat("\nCall:\n ")
    print.default(x@call)
    
    # Portfolio Optimization: 
    if (pfolio$what == "portfolio") {
        # Weights:
        weights = round(pfolio$pw, digits = 2)
        names(weights) = as.character(1:length(weights))
        cat("\nPortfolio Weights:\n ")
            print(weights[weights > 0])
        # Check Sum of Weights:
        cat("\nSum of Weights:\n ")
            print(round(sum(pfolio$pw), digits = 2))
        # Target Returns:   
        cat("\nTarget Return(s):\n ")
            print(round(pfolio$pm, digits = 4))
        # Target Risk:
        cat("\nTarget Risk(s):\n ")
            print(round(pfolio$ps, digits = 4))
    }
    
    # Frontier:
    if (pfolio$what == "frontier") {
        # Efficient Frontier:
        cat("\nEfficient Frontier - Returns:\n ")
        r.digits = abs(round(log10(max(pfolio$pm)), 0)) + 4
        print(round(pfolio$pm, digits = r.digits))
        cat("\nEfficient Frontier - Standard Deviations:\n ")
        s.digits = abs(round(log10(max(pfolio$ps)), 0)) + 4
        print(round(pfolio$ps, digits = s.digits))
        # Tangency Portfolio:
        cat("\nTangency Portfolio:\n ")
        cat("Risk free rate:", round(pfolio$Rf, digits = r.digits),  
            "   Return:", round(pfolio$Rm, digits = r.digits),
            "   Risk:", round(pfolio$Sm, digits = s.digits), "\n")
        # Equal Weights Portfolio:
        cat("\nEqual Weights Portfolio:\n ")
        cat("Risk:", round(pfolio$Rew, digits = r.digits), 
            "   Return:", round(pfolio$Sew, digits = s.digits), "\n") 
    }
       
    # Description:
    cat("\nDescription:\n ")
        cat(x@description)
        
    # Return Value: 
    invisible(x)
}


# ------------------------------------------------------------------------------


plot.fPFOLIO =
function(x, alpha = 0.05, mc = 500, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Print Method for an object of class "fPFOLIO"
    
    # Arguments:
    #   x - an object of class "fPFOLIO"
    
    # FUNCTION:
    
    # Check:
    if (as.character(x@call[1]) == "portfolioMarkowitz")
        stop("Objects returned by portfolioMarkowitz() cannot be plotted")
    
    # Settings:
    .mcSteps <<- mc
    .alphaLevel <<- alpha
    if (which[1] == "ask") {
        .interactive <<- TRUE
    } else {
        .interactive <<- FALSE
    }
          
    # Plot Efficient Frontier:
    plot.1 <<- function(x) {
        pfolio = x@pfolio
        # Extract:
        dim = length(pfolio$returns)
        returns = pfolio$returns
        covar = pfolio$cov
        r.range = pfolio$r.range
        s.range = pfolio$s.range 
        diversification = pfolio$diversification
        # 1. Plot Graph Frame:
        plot(x = s.range, y = r.range, type = "n", 
            xlab = "Standard Deviation", ylab = "Return",
            main = "Mean Variance Portfolio")  
        # 2. Plot Individual Assets:
        points(sqrt(diag(covar)), returns, pch = 20, col = "red")
        risk = sqrt(diag(covar)) + diff(s.range) / 50
        text(risk, returns, as.character(1:dim), col = "red")
        # 3. Plot Efficient Frontier:
        points(pfolio$ps, pfolio$pm, col = "blue", cex = 0.1) 
        # 4. Add Tangency Portfolio:
        # Find the Index (location of the Tangency Point):
        delta.pm = diff(pfolio$pm)
        delta.ps = diff(pfolio$ps)
        slope1 = slope.keep = delta.pm/delta.ps
        slope1 = slope1[slope.keep > 0]
        slope2 = (pfolio$pm-pfolio$Rf) / pfolio$ps
        diff.slope2 = diff(slope2) / 2
        slope2 = slope2[1:length(diff.slope2)] + diff.slope2
        slope2 = slope2[slope.keep > 0]
        cross = abs(slope2 - slope1)
        index = order(cross)[1]
        pm.pos = pfolio$pm[slope.keep > 0]
        ps.pos = pfolio$ps[slope.keep > 0]
        # Here are the mean return and the standard deviation
        mean.m = (pm.pos[index] + pm.pos[index+1]) / 2 
        sigma.m = (ps.pos[index] + ps.pos[index+1]) / 2
        # ... together with the slope
        slope = (mean.m - pfolio$Rf) / sigma.m
        # Add the Capital Market Line and the Tangency Point to the plot
        lines(c(0, max(pfolio$ps)), c(pfolio$Rf, slope* max(pfolio$ps) + 
            pfolio$Rf), lwd = 2 )
        points(sigma.m, slope*sigma.m + pfolio$Rf, col = "green", 
            pch = 20, cex = 1.5) 
        s.delta = max(pfolio$ps) / 10
        text(sigma.m - s.delta, slope*sigma.m + pfolio$Rf, "CML")
        # 5. Add Equal Weights Portfolio:
        # Calculate equally weighted portfolio:
        ew.weights = rep(1/dim, times = dim)
        Rew = (ew.weights %*% returns)
        Sew = (ew.weights %*% covar %*% ew.weights)
        points(sqrt(Sew), Rew, pch = 20, col = "steelblue4", cex = 1.5)
    } 
    # ... add Monte Carlo Portfolios:
    plot.2 <<- function(x) {
        plot.1(x)
        if (.interactive) {
            mc = readline("Number of Monte Carlo Steps > ")
            if (mc != "") .mcSteps <<- as.integer(mc) 
            cat("\nTotal Number of Monte Carlo Steps:", .mcSteps, "\n")
        }
        tmp = montecarloMarkowitz(x, mc = .mcSteps, doplot = FALSE, add = TRUE)
        
    }       
    # Bar Plot of Tangency Weights:
    plot.3 <<- function(x) {
        pfolio = x@pfolio
        weights = pfolio$t.weights ## [pfolio$t.weights > 0]    
        ## pie(weights, col = rainbow(length(weights)), radius = 0.9, 
        ##     main = "Tangency Portfolio Weights")
        barplot(weights, cex.names = 0.7, col = "steelblue")
        abline (h = 0)
    }   
    # Plot Diversification:
    plot.4 <<- function(x) {
        pfolio = x@pfolio
        if (is.na(pfolio$diversification)) {
            cat ("\nNo time series data available.\n")
        } else {
            xx = pfolio$pm
            yy = pfolio$diversification
            data = data.frame(cbind(xx, yy))
            plot(pfolio$pm, pfolio$diversification, type = "l", xlab = "Return", 
                ylab = "Number of Assets", main = "Diversification")
            zz = loess(yy ~ xx, data)
            lines(xx, zz$fit, col = "blue")
            points(pfolio$Rm, length(weights), col = "green", cex = 1.5)
        }
    }   
    # 5. Plot Cumulated Return:
    plot.5 <<- function(x) {
        pfolio = x@pfolio
        if (pfolio$plottype == "mucov") {
            cat ("\nNo time series data available.\n")
        } else {
            plot(cumsum(as.matrix(pfolio$data) %*% pfolio$t.weights), 
                type = "l", col = "steelblue",
                ylab = "Cumulated Return", main = "Portfolio Return")
            grid()
        }
    }
    # 6. Portfolio Risk Histogram:
    plot.6 <<- function(x) {
        pfolio = x@pfolio
        if (pfolio$plottype == "mucov") {
            cat ("\nNo time series data available.\n")
        } else {
            weights = pfolio$t.weights[pfolio$t.weights > 0]    
            pfolioHist(x@data, weights = weights, alpha = .alphaLevel)
        }
    }
    
    # Plot:
    interactivePlot(
        x = x,
        choices = c(
            "Efficient Frontier", 
            "... with Monte Carlo Portfolios",
            "Bar Chart of Tangency Portfolio", 
            "Diversification Plot",
            "Cumulated Return Tangency Portfolio",
            "Risk of Tangency Portfolio"),
        plotFUN = c(
            "plot.1", 
            "plot.2", 
            "plot.3",
            "plot.4",
            "plot.5",
            "plot.6"),
        which = which)
    
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


summary.fPFOLIO = 
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Summary Method for an object of class "fPFOLIO"
    
    # Arguments:
    #   object - an object of class fPFOLIO
    
    # FUNCTION:
  
    # Extract Portfolio:
    pfolio = object@pfolio
    
    # Print Result:
    print(object)
    
    # Plot:
    plot(object, ...)
            
    # Return Value:
    invisible(object)
}

# ------------------------------------------------------------------------------


.frontier =
function (object, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
     # Return Value:
     UseMethod(".frontier")
}


# ------------------------------------------------------------------------------


.frontier.default =
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 method for an object of class "fPFOLIO" to extract
    #   the points on the efficient frontier
    
    # FUNCTION:
    
    # Frontier:
    pfolio = object@pfolio
    mu = pfolio$pm
    sigma = pfolio$ps
    
    # Return Value:
    data.frame(sigma = sigma, mu = mu)
}


################################################################################


.tangencyMarkowitz = 
function(object, Rf = 0, add = TRUE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the properties of the tangency portfolio
    
    # Arguments:
    #   object - an object of class portfolio
    
    # Value:
    #   An updated object of class portfolio with the following 
    #   components added/updated:
    #   Rf - risk free rate of return
    #   Rm - rate of return of the tangency portfolio
    #   Sm - standardard deviation of the tangency portfolio
    #   t.weights - weights of the tangency portfolio
    
    # Note:
    #   Not for all risk free rates there exists a tangency portfolio!!!
    #   In addition adds the "capital market line" and the tangency portfolio, 
    #   Rm and Sm, to the efficient frontier.
    #   I have not yet checked if this works for all values of Rf !!
    #   I hope it works properly even if there exists no tangency portfolio.  
    
    # FUNCTION:
    
    # Find the Index (location of the tangency point:
    delta.pm = diff(object$pm)
    delta.ps = diff(object$ps)
    slope1 = slope.keep = delta.pm/delta.ps
    slope1 = slope1[slope.keep > 0]
    slope2 = (object$pm-Rf) / object$ps
    diff.slope2 = diff(slope2) / 2
    slope2 = slope2[1:length(diff.slope2)] + diff.slope2
    slope2 = slope2[slope.keep > 0]
    cross = abs(slope2 - slope1)
    index = order(cross)[1]
    pm.pos = object$pm[slope.keep > 0]
    ps.pos = object$ps[slope.keep > 0]

    # Here are the mean return and the standard deviation:
    mean.m = (pm.pos[index] + pm.pos[index+1]) / 2 
    sigma.m = (ps.pos[index] + ps.pos[index+1]) / 2
    # ... together with the slope
    slope = (mean.m - Rf) / sigma.m
    x1 = 0
    y1 = Rf
    x2 = max(object$ps)
    y2 = slope*x2 + Rf
    xm = sigma.m
    ym = slope*sigma.m + Rf

    # Add the Capital Market Line and the Tangency Point to the plot
    if (add) {
        lines(c(x1, x2), c(y1, y2), lwd = 2 )
        points(xm, ym, col = "green", cex = 1.5) 
        s.delta = max(object$ps) / 10
        text(xm - s.delta, ym, "CML")}
            
    # Calculate Tangency Portfolio, if it exists!
    # The test is true, when the slope could be evaluated.
    if (!is.na(slope)) {
      tangency = .portfolio.optim(pm = ym, returns = object$returns,
        covmat = object$cov) 
      # Portfolio Weights in Percent:
      pw = 100 * tangency$pw   
      # Remove those smaller 1 %% Promille!!
      pw = round(pw*(sign(pw - 0.1) + 1)/2, digits = 2) 
      object$t.weights = pw/100 
    } 
    
    # Add to Output:
    object$Rf = Rf
    object$Rm = ym
    object$Sm = xm
    
    # Return Value:
    object
}


# ------------------------------------------------------------------------------


.equalweightsMarkowitz = 
function(object, add = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Adds the equal weighted portfolio to efficient frontier plot
    
    # FUNCTION:
    
    # Calculate equally weighted portfolio:
    means = object$returns
    covmat = object$cov
    ew.weights = rep(1/length(object$returns), times = length(object$returns))
    Rew = (ew.weights %*% means)[[1,1]]
    Sew = (ew.weights %*% covmat %*% ew.weights)[[1,1]]
    if (add) {
        points(sqrt(Sew), Rew, col = "steelblue")
    }
    
    # Return Value:
    object$Rew = Rew
    object$Sew = Sew
    object
}


# ------------------------------------------------------------------------------
# Markowitz - Short Selling


.frontierShortSelling = 
function(data, Rf = 0, length = 300, 
r.range = NULL, s.range = NULL,
title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the efficient frontier, short selling allowed
    
    # Details  from a matrix
    #   Calculates the efficient frontier (short selling allowed) from a
    #   a matrix of either market or simulated assets given in matrix "x". 
    #   Each time series represents a column in this matrix.
    
    # Arguments:     
    #   data - either a rectangular time series object which can be
    #       transformed into a matrix or a list with two named entries,
    #       mu - the returns of the multivariate series, and
    #       Sigma - the Covariance matrix of the multivariate series
    #   length - Number of equidistant return points on the Efficient 
    #       frontier
    #   r.range - Plot range of returns, if NULL, the range will be created
    #       automatically
    #   s.range - Plot range of standard deviations, if NULL, the range 
    #       will be created automatically
    
    # FUNCTION:
    
    # Check Data: 
    if (is.list(data)) {
        x.mat = NA
        mu = data$mu
        Sigma = data$Sigma
        plottype = "mucov"
    } else {
        x.mat = as.matrix(x)
        # Mean Vector  and Covariance Matrix:
        mu = apply(x.mat, MARGIN = 2, FUN = mean)
        Sigma = cov(x.mat)
        plottype = "series"
    }
    
    # Settings:
    C0 = 1
    
    # Ranges for Mean and Standard Deviation:
    if (is.null(r.range)) r.range = range(mu)
    if (is.null(s.range)) s.range = c(0, max(sqrt(diag(Sigma))))
    
    # Portfolio Settings:
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = mu %*% invSigma %*% mu
    b = mu %*% invSigma %*% one
    c = one %*% invSigma %*% one
    d = a*c - b^2
    
    # Minimum Variance Portfolio
    mu.mv = (b/c)*C0
    sigma.mv = C0/sqrt(c)
    
    # Plot:
    mu.p = seq(r.range[1], r.range[2], length = length)
    sigma.p = sqrt((c*mu.p^2 - 2*b*C0*mu.p + a*C0^2)/d)
    
    # Tangency Portfolio:
    mu.tg = (a/b)*C0
    sigma.tg = (sqrt(a)/b)*C0
    weights.tg = C0 * as.vector(invSigma %*% mu ) / b 
    
    # Market Portfolio:
    mu.f = 0
    mu.mk = 0
    sigma.mk = 0
    
    # Equal Weights Portfolio:
    ew.weights = rep(1/length(mu), times = length(mu))
    mu.ew = (ew.weights %*% mu)[[1, 1]]
    sigma.ew = (ew.weights %*% Sigma %*% ew.weights)[[1, 1]]
         
    # Result:
    pfolio = list(
        what = "Short Selling Markowitz",
        plottype = plottype,
        data = data, 
        pw = NA, pm = mu.p, ps = sigma.p, 
        returns = mu, cov = Sigma, 
        r.range = r.range, s.range = s.range, 
        # Tangency Portfolio:
        Rf = Rf, Rm = mu.tg, Sm = sigma.tg, 
        # Equal Weights Portfolio:
        Rew = mu.ew, Sew = sigma.ew,
        t.weights = weights.tg,
        diversification = NA) 
        
    # Title: 
    if (is.null(title)) 
        title = "Markowitz Portfolio Optimization"
    
    # Description:
    if (is.null(description))
        description = as.character(date())
    
    # Return Value:
    new ("fPFOLIO", 
        call = as.call(match.call()),
        method = "Analytical Solution", 
        model = "Short Selling Markowitz Portfolio",
        data = list(x = data), 
        pfolio = pfolio, 
        title = as.character(title),
        description = as.character(description)
    ) 
   
}


################################################################################
# BUILTIN: tseries


.portfolio.optim = 
function(pm, returns, covmat) 
{   # A Builtin function modified by Diethelm Wuertz

    # Description: 
    #   Computes a point on the efficient frontier
    
    # Note:
    #   Package: tseries
    #   Version: 0.9-21
    #   Date: 2004-04-23
    #   Title: Time series analysis and computational finance
    #   Author: Compiled by Adrian Trapletti <a.trapletti@bluewin.ch>
    #   Maintainer: Kurt Hornik <Kurt.Hornik@R-project.org>
    #   Description: Package for time series analysis and computational
    #       finance
    #   Depends: R (>= 1.9.0), quadprog
    #   License: GPL (see file COPYING)
    #   Packaged: Thu Apr 22 16:32:16 2004; hornik
    #   Built: R 1.9.0; i386-pc-mingw32; 2004-04-22 17:49:17; windows
        
    # FUNCTION:
    
    # Optimize:
    k = dim(covmat)[2] # dim(x)[2]
    dvec = rep(0, k)
    a1 = rep(1, k)
    a2 = returns # apply(x, 2, mean)    
    a3 = matrix(0, k, k)
    diag(a3) = 1
    b3 = rep(0, k)
    Amat = t(rbind(a1, a2, a3))
    b0 = c(1, pm, b3)    
    res = .solve.QP(covmat, dvec, Amat, bvec = b0, meq = 2)
    weights = res$solution
    ps = sqrt(weights %*% covmat %*% weights)
    
    # Result:
    ans = list(pw = weights, pm = pm, ps = ps)
    
    # Return value:
    ans
}
    

################################################################################
# BUILTIN: quadprog

    
.solve.QP = 
function(Dmat, dvec, Amat, bvec, meq)
{   # A Builtin function modified by Diethelm Wuertz

    # Description:
    #   Solves the quadratic programming problem
    
    # Note:   
    #   Package: quadprog
    #   Version: 1.4-7
    #   Date: 2004-01-31
    #   Title: Functions to solve Quadratic Programming Problems.
    #   Author: S original by Berwin A. Turlach <berwin.turlach@anu.edu.au>
    #       R port by Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
    #   Maintainer: Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
    #   Description: This package contains routines and documentation for
    #       solving quadratic programming problems.
    #   License: GPL version 2 or later
    #   Packaged: Sat Jan 31 13:32:53 2004; hornik
    #   Built: R 1.9.0; i386-pc-mingw32; 2004-03-28 15:03:03
    
    # FUNCTION:
    
    # Solve QP:
    n = nrow(Dmat)
    q = ncol(Amat)  
    iact  = rep(0,q)
    nact  = 0
    r = min(n,q)
    sol = rep(0,n)
    crval = 0
    work = rep(0,2*n+r*(r+5)/2+2*q+1)
    iter = rep(0,2) 
    factorized = FALSE
    res1 = .Fortran("qpgen2",
        as.double(Dmat), dvec = as.double(dvec), as.integer(n), 
        as.integer(n), sol = as.double(sol), crval = as.double(crval),
        as.double(Amat), as.double(bvec), as.integer(n), as.integer(q), 
        as.integer(meq), iact = as.integer(iact), nact = as.integer(nact),
        iter = as.integer(iter), work = as.double(work),
        ierr = as.integer(factorized), PACKAGE = "fPortfolio")
    if (res1$ierr == 1) {
        warning("constraints are inconsistent, no solution!")
    } else if (res1$ierr == 2) {
        warning("matrix D in quadratic function is not positive definite!")
    }
    
    # Return Value:
    list(solution = res1$sol, value = res1$crval,
       unconstrainted.solution = res1$dvec,
       iterations = res1$iter, iact = res1$iact[1:res1$nact]) 
}


################################################################################

