
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                  EXTREME VALUE COPULA PARAMETER FITTING:
#  evCopulaSim          X      Simulates bivariate extreme value copula
#  evCopulaFit          X      Fits the paramter of an extreme value copula
################################################################################


################################################################################
# FUNCTION:                  EXTREME VALUE COPULA PARAMETER FITTING:
#  evCopulaSim          X      Simulates bivariate extreme value copula
#  evCopulaFit          X      Fits the paramter of an extreme value copula


evCopulaSim = 
function(n, param = NULL, 
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates bivariate extreme value Copula
    
    return("not yet implemented")
    # revCopula() is missing ...
    
    # Match Arguments:
    type = match.arg(type)
      
    # Settings:
    if (is.null(param)) param = .evParam(type)$param
    
    # Random Variates:
    ans = revCopula(n = n, param = parm, type = type) 

    # Control:
    control = list(param = param, copula = "ev", type = type)
    attr(ans, "control")<-unlist(control)
      
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

    
evCopulaFit =
function(u, v = NULL, 
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"), ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fits the paramter of an elliptical copula
    
    # Note:
    #   The upper limit for nu is 100
    
    # FUNCTION:
    
    # Match Arguments:
    type = match.arg(type)
    
    # Settings:
    U = u
    V = v
    if (is.list(u)) {
        u = u[[1]]
        v = u[[2]]
    }
    if (is.matrix(u)) {
        U = u[, 1]
        V = u[, 2]
    }
    U <<- u
    V <<- v

    # Start Values:
    param = .evParam(type)$param
     
    # Estimate Copula:
    fun = function(x, type) {
        -mean( log(evCopula(u = U, v = V, param = x, type = type)) )
    }
    range = .evRange(type)

    # fit = nlminb(start = alpha, objective = fun, 
    #     lower = range[1], upper = range[2],  type = type, ...)
    fit = NA
    
    # Return Value:
    fit
}


################################################################################

