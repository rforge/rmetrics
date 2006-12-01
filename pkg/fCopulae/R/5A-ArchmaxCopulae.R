
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
#                       X     NOT YET IMPEMENTED
# FUNCTION:                  ARMAX COPULAE PARAMETER:
#  .armaxParam          X     Sets parameters for an armax copula
#  .armaxRange          X     Returns the range of valid parameter values
#  .armaxCheck          X     Checks if the parameters are in the valid range
# FUNCTION:                  ARMAX COPULAE GENERATOR FUNCTION:
#  armax*               X
# FUNCTION                   KENDALL'S TAU AND SPEARMAN'S RHO:
#  armaxTau             X     Returns Kendall's tau for armax copulae
#  armaxRho             X     Returns Spearman's rho for armax copulae
# FUNCTION:                  ARMAX COPULAE TAIL COEFFICIENT:
#  armaxTailCoeff       X     Computes tail dependence for armax copulae
#  armaxTailPlot        X     Plots armax tail dependence function
# FUNCTION:                  ARMAX COPULAE RANDOM VARIATES:
#  rarmaxCopula         X     Generates armax copula random variates 
#  rarmaxSlider         X     Generates interactive plots of random variates
# FUNCTION:                  ARMAX COPULAE PROBABILITY:
#  parmaxlCopula              Computes armax copula probability
#  parmaxSlider         X     Generates interactive plots of probability
# FUNCTION:                  ARMAX COPULAE DENSITY:
#  darmaxCopula         X     Computes armax copula density 
#  darmaxSlider         X     Generates interactive plots of armax density
# FUNCTION:                  ARMAX COPULAE PARAMETER FITTING:
#  armaxCopulaSim       X     Simulates bivariate extreme value copula
#  armaxCopulaFit       X     Fits the paramter of an extreme value copula
################################################################################


################################################################################
# ARMAX COPULAE PARAMETER:


.armaxParam =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


# ------------------------------------------------------------------------------


.armaxRange =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


# ------------------------------------------------------------------------------


.armaxCheck =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


################################################################################
# ARMAX COPULAE GENERATOR FUNCTION:


################################################################################
# KENDALL'S TAU AND SPEARMAN'S RHO:


armaxTau =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


# ------------------------------------------------------------------------------


armaxRho =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


################################################################################
# ARMAX COPULAE TAIL COEFFICIENT:


armaxTailCoeff =
function(tail = c("Upper", "Lower"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


# ------------------------------------------------------------------------------


armaxTailPlot =
function(tail = c("Upper", "Lower"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


################################################################################
# ARMAX COPULAE RANDOM VARIATES:


rarmaxCopula =
function(n, param = NULL, type = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


# ------------------------------------------------------------------------------


rarmaxSlider =
function(B = 10)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


################################################################################
# ARMAX COPULAE PROBABILITY:


parmaxCopula =
function (u = 0.5, v = u, alpha = NULL, param = NULL, archmType = 1:22, 
evType = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"), 
output = c("vector", "list")) 
{
    # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Settings:  
    output = match.arg(output)
    
    # Archimedean:
    archmType = match.arg(archmType)
    archmType = as.integer(archmType)
    if (is.null(alpha)) alpha = .archmParam(archmType)$param
    
    # Extreme Value:
    evType = match.arg(evType)
    if (is.null(param)) param = .evParam(evType)$param
    
    # Grid Points:
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 1]
        u = u[, 2]
    }
    
    # Archimax:
    phiX = Phi(u, alpha = NULL, type = archmType)
    phiY = Phi(v, alpha = NULL, type = archmType)  
    A = phiX/(phiX+phiY)
    arg = (phiX+phiY) * Afunc(A, type = evType)
    C.uv = Phi(arg, alpha = NULL, type = archmType, inv = TRUE) 
    C.uv = (C.uv + abs(C.uv))/2
    C.uv[is.na(C.uv)] = 0
    C.uv[which(u == 0)] = 0
    C.uv[which(u == 1)] = v[which(u == 1)]
    C.uv[which(v == 0)] = 0
    C.uv[which(v == 1)] = u[which(v == 1)]
    C.uv[which(u * v == 1)] = 1
    C.uv[which(u + v == 0)] = 0

    # Output:
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        C.uv = list(x = x, y = y, z = matrix(C.uv, ncol = N))
    }
    
    # Add Control:
    control = list(
        alpha = alpha[[1]], 
        param = param, 
        copula = "armax", 
        type = c(archmType, evType))    
    attr(C.uv, "control") <- unlist(control)
  
    # Return Value:
    C.uv
}


# ------------------------------------------------------------------------------


parmaxSlider =
function(type = c("persp", "contour"), B = 10)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}



################################################################################
# ARMAX COPULAE DENSITY:


darmaxCopula =
function(u = 0.5, v = u, param = NULL, type = NULL, 
output = c("vector", "list"))   
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


################################################################################
# ARCHIMEDEAN COPULAE PARAMETER FITTING:


armaxCopulaSim =
function(n)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


# ------------------------------------------------------------------------------


armaxCopulaFit =
function(u, v = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Argumentes:
    
    # FUNCTION:
    
    # Return Value:
    print("Sorry, Not Yet Implemented!")
}


################################################################################

