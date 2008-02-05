
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
#  ghtFit               Fits parameters of a skew Student-t density
################################################################################


ghtFit <- 
    function(x, beta = 1e-6, delta = 1, mu = 0, nu = 10, scale = TRUE, 
    doplot = TRUE, span = "auto", trace = TRUE, 
    title = NULL, description = NULL, ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fits parameters of a generalized hyperbolic Student-t density
  
    # FUNCTION:
    
    # Transform:
    x.orig = x
    x = as.vector(x)
    if (scale) {
        SD = sd(x)
        x = x / SD 
    }
    
    # Settings:
    CALL = match.call()
    
    # Log-likelihood Function:
    eghtmle = function(x, y = x, trace) { 
        f = -sum(dght(y, x[1], x[2], x[3], x[4], log = TRUE))
        if (trace) {
            cat("\n Objective Function Value:  ", -f)
            cat("\n Parameter Estimates:       ", x[1], x[2], x[3], x[4], "\n")
        }
        f 
    }
        
    # Variable Transformation and Minimization:
    eps = 1e-20
    BIG = 1000
    r = nlminb(start = c(beta, delta, mu, nu), objective = eghtmle, 
        lower = c(eps, eps, -BIG, 2), upper = BIG, y = x, trace = trace) 
    
    # Add Title and Description:
    if (is.null(title)) title = "Generalized Hyperbolic Parameter Estimation"
    if (is.null(description)) description = .description()
        
    # Result:
    if (scale) {
        r$par = r$par / c(1, 1/SD, 1/SD, 1)
        r$objective = eghtmle(r$par, y = as.vector(x.orig), trace = trace)
    }   
    fit = list(estimate = r$par, minimum = -r$objective, code = r$convergence)
     
    # Optional Plot:
    if (doplot) {
        if (span == "auto") span = seq(min(x), max(x), length = 51)
        z = density(x, n = 100, ...)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dght(span, r$par[1], r$par[2], r$par[3], r$par[4])
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)", ...)
        title("GHT Parameter Estimation")    
        lines(x = span, y = log(y.points), col = "steelblue")
    }
       
    # Return Value:
    new("fDISTFIT",     
        call = as.call(CALL),
        model = "Generalized Hyperbolic Distribution",
        data = as.data.frame(x.orig),
        fit = fit,
        title = as.character(title), 
        description = .description() )
}


################################################################################

