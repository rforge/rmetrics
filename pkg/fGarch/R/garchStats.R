
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
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
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                FORECASTING: 
#  garchKappa               Computes Expection for APARCH Models
#  .funE                    Internal function used by kappa()
################################################################################


garchKappa = 
function(cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
gamma = 0, delta = 2, skew = NA, shape = NA)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Expection for APARCH Models
    
    # FUNCTION:
    
    # Compute kappa:
    kappa = integrate(.funE, lower = -Inf, upper = Inf, cond.dist = 
        cond.dist[1], gamma = gamma, delta = delta, skew = skew, shape = 
        shape)[[1]] 
    names(kappa) = "kappa"
    attr(kappa, "control") = 
        c(gamma = gamma, delta = delta, skew = skew, shape = shape)
    attr(kappa, "cond.dist") = cond.dist[1]
    
    # Return Value:
    kappa
}


# ------------------------------------------------------------------------------


.funE = 
function(x, cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
gamma = 0, delta = 2, skew = NA, shape = NA)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Internal function used by kappa()

    # FUNCTION:
    
    # Compute Expectation Value for ...
    funcE = (abs(x) - gamma*x)^delta
    
    # Select Appropriate Conditional Density:
    cond.dist = cond.dist[1]
    if (cond.dist == "dnorm") {
        fun = funcE * dnorm(x)
    }
    if (cond.dist == "dged") {
        fun = funcE * dged(x, nu = shape) 
    }
    if (cond.dist == "dstd") {
        fun = funcE * dstd(x, nu = shape) 
    }
    if (cond.dist == "dsnorm") {
        fun = funcE * dsnorm(x, xi = skew)
    }
    if (cond.dist == "dsged") {
        fun = funcE * dsged(x, nu = shape, xi = skew) 
    }
    if (cond.dist == "dsstd") {
        fun = funcE * dsstd(x, nu = shape, xi = skew) 
    }
    
    # Return Value:
    fun
} 


################################################################################

