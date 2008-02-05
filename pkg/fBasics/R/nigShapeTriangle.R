
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
# FUNCTION:             DESCRIPTION:
#  nigShapeTriangle      Plots NIG Shape Triangle
################################################################################

   
nigShapeTriangle <- 
    function(object, add = FALSE, labels = TRUE, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots NIG Shape Triangle
    
    # Arguments:
    #   object - an object of class 'fDISTFIT' as returned by the
    #       function nigFit()
    
    # Example:
    #   nigShapeTriangle(nigFit(rnig(100), doplot = FALSE))
    
    # FUNCTION:
    
    # Settings:
    stopifnot(class(object) == "fDISTFIT")
    
    # Plot Frame:
    if (labels) {
        xlab = "Asymmetry: chi"
        ylab = "Steepness: zeta"
        main = "NIG Shape Traingle"
    } else {
        xlab = ylab = main = ""
    }
    if (!add) {
        x = c(-1, 0, 1, -1)
        y = c( 1, 0, 1,  1)
        plot(x, y, type = "l", xlab = xlab, ylab = ylab, main = main, ...)
        for (s in c(0.8, 0.6, 0.4, 0.2)) 
            lines(c(-s, s), c(s, s), lty = 3, col = "grey")  
        lines(c(0, 0), c(0, 1), lty = 3, col = "grey")
    }
    
    # Transform:
    par = object@fit$estimate
    names(par) = c("alpha", "beta", "delta", "mu")
    alpha = par[1] 
    beta = par[2]
    delta = par[3]
    mu = par[4]   
    
    # Add Points:
    zeta = 1/sqrt(1+delta*sqrt(alpha^2-beta^2))
    chi = zeta*(beta/alpha)
    if (labels) {
        points(chi, zeta, pch = 19, ...)
    } else {
        points(chi, zeta, ...)
    }   
    
    # Result:
    ans = list(chi = chi[[1]], zeta = zeta[[1]])
    attr(ans, "control")<-par
    
    # Return Value:
    ans
}


################################################################################

