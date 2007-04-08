
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
# FUNCTION:                  ELLIPTICAL COPULAE PARAMETER FITTING:
#  ellipticalCopulaSim        Simulates bivariate elliptical copula
#  ellipticalCopulaFit        Fits the paramter of an elliptical copula
################################################################################


################################################################################
# FUNCTION:                  ELLIPTICAL COPULAE PARAMETER FITTING:
#  ellipticalCopulaSim        Simulates bivariate elliptical copula
#  ellipticalCopulaFit        Fits the paramter of an elliptical copula


ellipticalCopulaSim = 
function (n, rho = 0.75, param = NULL, type = c("norm", "cauchy", "t")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates bivariate elliptical Copula
    
    # Match Arguments:
    type = match.arg(type)
      
    # "norm" Random Deviates:  
    if (type == "norm") {
        ans = .rnormCopula(n = n, rho = rho)
    }
    
    # "cauchy" Random Deviates:
    if (type == "cauchy") {
        ans = .rcauchyCopula(n = n, rho = rho)
    }
    
    # "t" Random Deviates:
    if (type == "t") {
        if (is.null(param)) {
            param = c(nu = 4)
        } else {
            param = c(nu = param[1])
        }
        ans = .rtCopula(n = n, rho = rho, nu = param)
    }
     
    # "logistic" Random Deviates:
    # NOT YET IMPLEMENTED ...
    
    # "laplace" Random Deviates:
    # NOT YET IMPLEMENTED ...

    
    # "kotz" Random Deviates:
    # NOT YET IMPLEMENTED ...

    # "epower" Random Deviates:
    # NOT YET IMPLEMENTED ...
 
    # Control:
    control = list(rho = rho, param = param, type = type)
    attr(ans, "control") = unlist(control)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

    
ellipticalCopulaFit =
function(u, v = NULL, type = c("norm", "cauchy", "t"), ...)
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

    # Estimate Rho from Kendall's tau for all types of Copula:
    tau = cor(x = U, y = V, method = "kendall") #[1, 2]
    Rho = rho = sin((pi*tau/2))
     
    # Estimate "norm" Copula:
    if (type == "norm") {
        fun = function(x) {
            -mean( log(.dnormCopula(u = U, v = V, rho = x)) )
        }
        fit = nlminb(start = rho, objective = fun, lower = -1, upper = 1, ...)
    }
    
    # Estimate "cauchy" Copula:
    if (type == "cauchy") {
        fun = function(x) {
            -mean( log(.dcauchyCopula(u = U, v = V, rho = x)) ) 
        }
        fit = nlminb(start = rho, objective = fun, lower = -1, upper = 1, ...)
    }
    
    # Estimate "t" Copula:
    if (type == "t") {
        fun = function(x) {
            -mean( log(.dtCopula(u = U, v = V, rho = x[1], nu = x[2])) ) 
        }
        fit = nlminb(start = c(rho = rho, nu = 4), objective = fun, 
             lower = c(-1, 1), upper = c(1, Inf), ...)
        fit$Nu = 4
    }
    
    # Estimate "logistic" Copula:
    if (type == "logistic") {
        # NOT YET IMPLEMENTED ...
        fun = function(x) {
            -mean( log(dellipticalCopula(u = U, v = V, ...)) ) 
        }
        fit = nlminb(start = c(), objective = fun, 
             lower = c(rho = -1, NA), upper = c(rho = 1, NA), ...)
    }
    
    # Estimate "laplace" Copula:
    if (type == "laplace") {
        # NOT YET IMPLEMENTED ...
        fun = function(x) {
            -mean( log(dellipticalCopula(u = U, v = V, ...)) ) 
        }
        fit = nlminb(start = c(), objective = fun, 
             lower = c(rho = -1, NA), upper = c(rho = 1, NA), ...)
    }
    
    # Estimate "kotz" Copula:
    if (type == "kotz") {
        # NOT YET IMPLEMENTED ...
        fun = function(x) {
            -mean( log(dellipticalCopula(u = U, v = V, ...)) ) 
        }
        fit = nlminb(start = c(), objective = fun, 
             lower = c(rho = -1, NA), upper = c(rho = 1, NA), ...)
    }
    
    # Estimate "epower" Copula:
    if (type == "epower") {
        # NOT YET IMPLEMENTED ...
        fun = function(x) {
            -mean( log(dellipticalCopula(u = U, v = V, ...)) ) 
        }
        fit = nlminb(start = c(), objective = fun, 
             lower = c(rho = -1, NA), upper = c(rho = 1, NA), ...)
    }
    
    # Keep Start Value:
    # fit$Rho = Rho
    
    # Return Value:
    fit
}


################################################################################

