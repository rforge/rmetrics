
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
# FUNCTION                   KENDALL'S TAU AND SPEARMAN'S RHO:
#  evTau                      Returns Kendall's tau for extreme value copulae
#  evRho                      Returns Spearman's rho for extreme value copulae
# FUNCTION:                  EXTREME VALUE COPULAE TAIL DEPENDENCE:
#  evTailCoeff          X     Computes tail dependence for extreme value copulae
#  evTailPlot           X     Plots extreme value tail dependence function
################################################################################


################################################################################
# FUNCTION                   KENDALL'S TAU AND SPEARMAN'S RHO:
#  evTau                      Returns Kendall's tau for extreme value copulae
#  evRho                      Returns Spearman's rho for extreme value copulae


evTau =
function(param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"), alternative = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Kendall's tau for an extreme value copula
    
    # Example:
    #   evTau(alternative = FALSE)
    #   evTau(alternative = TRUE)
    
    # FUNCTION:
    
    # Kendall's Tau:
    if (!alternative) {
        ans = .ev1Tau(param, type)
    } else {
        ans = .ev2Tau(param, type)
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.ev1Tau =
function(param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz
    # Type:
    type = match.arg(type)
    
    # Default Parameters:
    if (is.null(param)) param = .evParam(type)$param
    
    # Kendall's Tau Integrand:
    fun = function(x, param, type) {
        # To be integrated from 0 to 1 ...
        A = Afunc(x = x, param = param, type = type)
        A2 = .AfuncSecondDer(x, param, type)
        f = (x*(1-x)/A) * A2
        f
    }
    
    # Get control attribute from:
    attribute = Afunc(0.5, param, type)
    
    # Integrate:
    ans = integrate(fun, 0, 1, param = param, type = type)
    Tau = c(Tau = ans[[1]])
    
    # Add Control Attribute:
    attr(Tau, "control")<-attr(attribute, "control")
    
    # Return Value:
    Tau   
}


# ------------------------------------------------------------------------------


.ev2Tau =
function(param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Example:
    #   .ev2Tau()
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Default Parameters:
    if (is.null(param)) param = .evParam(type)$param
    
    # Kendall's Tau Minus Rho/3 Double Integrand:
    fun = function(x, y, ...) {
        D = devCopula(x, y, alternative = FALSE, ...)
        D[is.na(D)] = 0
        f = 4 * 
            ( pevCopula(x, y, alternative = FALSE, ...) - x*y) * D
        f
    }
    
    # Get control attribute from:
    attribute = Afunc(0.5, param, type)
    
    # Integrate:
    ans = integrate2d(fun, param = param, type = type, error = 1e-8)
    Tau = c(Tau = ans[[1]] + .ev2Rho(param, type)/3)
    
    # Add Control Attribute:
    attr(Tau, "control")<-attr(attribute, "control")
    
    # Return Value:
    Tau   
}


# ------------------------------------------------------------------------------


evRho =
function(param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"), alternative = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Spearman's rho for an extreme value copula
    
    # Example:
    #   evRho(alternative = FALSE)
    #   evRho(alternative = TRUE)
    
    # FUNCTION:
    
    # Spearman's Rho:
    if (!alternative) {
        ans = .ev1Rho(param, type)
    } else {
        ans = .ev2Rho(param, type)
    }
    
    # Return Value:
    ans
}
   

# ------------------------------------------------------------------------------


.ev1Rho =
function(param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Spearman's rho for an extreme value copula
    
    # Example:
    #   .ev1Rho()

    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Default Parameters:
    if (is.null(param)) param = .evParam(type)$param

    # Spearman's Rho Integrand:
    fun = function(x, param, type) {
        # To be integrated from 0 to 1 ...
        A = Afunc(x = x, param = param, type = type)
        f = ( 12 / (A+1)^2 ) - 3
        f
    }
    
    # Get control attribute from:
    attribute = Afunc(0.5, param, type)
    
    # Integrate:
    ans = integrate(fun, 0, 1, param = param, type = type)
    Rho = c(Rho = ans[[1]])
    
    # Add Control Attribute:
    attr(Rho, "control")<-attr(attribute, "control")
    
    # Return Value:
    Rho   
}


# ------------------------------------------------------------------------------


.ev2Rho =
function(param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Spearman's rho for an extreme value copula
    
    # Example:
    #   .ev2Rho()
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Default Parameters:
    if (is.null(param)) param = .evParam(type)$param

    # Spearman's Rho Integrand:
    fun = function(x, y, ...) {
        f = 12 * pevCopula(x, y, ...) - 3
        f
    }
    
    # Get control attribute from:
    attribute = Afunc(0.5, param, type)
    
    # Integrate:
    ans = integrate2d(fun, param = param, type = type)
    Rho = c(Rho = ans[[1]])
    
    # Add Control Attribute:
    attr(Rho, "control")<-attr(attribute, "control")
    
    # Return Value:
    Rho   
}


################################################################################
# FUNCTION:                  EXTREME VALUE COPULAE TAIL DEPENDENCE:
#  evTailCoeff          X     Computes tail dependence for extreme value copulae
#  evTailPlot           X     Plots extreme value tail dependence function


evTailCoeff = 
function(param = NULL, type = 1:22)
{   # A function implemented by Diethelm Wuertz

    NA
}


# ------------------------------------------------------------------------------


evTailPlot = 
function(param = NULL, type = 1:22, tail = c("Upper", "Lower"))
{   # A function implemented by Diethelm Wuertz

    NA
}


################################################################################



evTailCoeff =
function(param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tail Dependence for extreme value copulae
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Default Parameters:
    if (is.null(param)) param = .evParam(type)$param

    # Tail Coefficient:
    N = 20
    x = 1 - (1/2)^(1:N)
    lambdaU.Cuv = ( 1 - 2*x + 
        pevCopula(u = x, v = x, param = param, type = type) ) / (1-x)
       
    # Return Value:
    list(x = x, y = lambdaU.Cuv)
}