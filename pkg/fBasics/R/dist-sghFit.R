
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:            GENERALIZED DISTRIBUTION:
#  sghFit                Fits parameters of a standardized GH density
################################################################################


sghFit =
function (x, zeta = 1, rho = 0, lambda = 1, include.lambda = TRUE, 
    scale = TRUE, doplot = TRUE, span = "auto", trace = TRUE, 
    title = NULL, description = NULL, ...) 
{
    x.orig = x
    x = as.vector(x)
    if (scale) x = (x-mean(x)) / sd(x)
    
    esghmle = function(x, y = x, trace) {
        f = -sum(log(dsgh(y, x[1], x[2], x[2], log = FALSE)))
        if (trace) {
            cat("\n Objective Function Value:  ", -f)
            cat("\n Parameter Estimates:       ", x[1], x[2], x[2], "\n")
        }
        f
    }
    
    eps = .Machine$double.eps^0.5
    BIG = 1000

    if (include.lambda) 
    {
        # LLH Function:
        esghmle.include = function(x, y = x, trace) {
            f = -sum(log(dsgh(y, x[1], x[2], x[3], log = FALSE)))
            if (trace) {
                cat("\n Objective Function Value:  ", -f)
                cat("\n Parameter Estimates:       ", x[1], x[2], x[3], "\n")
            }
            f
        }
        # LLH Optimization:
        r = nlminb(
            start = c(zeta, rho, lambda), 
            objective = esghmle.include, 
            lower = c(eps, -0.9999, -2), 
            upper = c(BIG, +0.9999, +5), 
            y = x, 
            trace = trace)
        names(r$par) <- c("zeta", "rho", "lambda")
            
    } else {
        # LLH Function:
        esghmle = function(x, y = x, lambda, trace) {
            f = -sum(log(dsgh(y, x[1], x[2], lambda, log = FALSE)))
            if (trace) {
                cat("\n Objective Function Value:  ", -f)
                cat("\n Parameter Estimates:       ", x[1], x[2], "\n")
            }
            f
        }
        # LLH Optimization:
        r = nlminb(
            start = c(zeta, rho), 
            objective = esghmle, 
            lower = c(eps, -0.9999), 
            upper = c(BIG, +0.9999), 
            y = x, 
            lambda = lambda, 
            trace = trace)
        r$par = c(r$par, lambda)
        names(r$par) <- c("zeta", "rho", "fix.lambda")
        
    }    
    
    param = .paramGH(r$par[1], r$par[2], r$par[3])
    if (trace) {
        names(param) = c("alpha", "beta", "delta", "mu")
        cat("\n 1st Parameterization:", "\n ")
        print(param)         
    }
    
    # Default Title and Description:
    if (is.null(title)) 
        title = "Standardized NIG Parameter Estimation"
    if (is.null(description)) 
        description = description()
    
    fit = list(
        estimate = r$par,
        minimum = -r$objective, 
        code = r$convergence,
        param = param, 
        mean = mean(x.orig),
        var = var(x.orig))
    
    # Optional Plot:
    if (doplot) {
        if (span == "auto") span = seq(min(x), max(x), length = 101)
        z = density(x, n = 100, ...)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dsgh(span, zeta = r$par[1], rho = r$par[2], lambda)
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)", ...)
        title("SNIG Parameter Estimation")
        lines(x = span, y = log(y.points), col = "steelblue")
    }
    
    # Return Value:
    new("fDISTFIT", 
        call = match.call(), 
        model = "Normal Inverse Gaussian Distribution", 
        data = as.data.frame(x.orig), 
        fit = fit, 
        title = as.character(title), 
        description = description())
}



################################################################################

