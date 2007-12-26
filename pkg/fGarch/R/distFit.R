
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
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:              PARAMETER ESTIMATION:
#  normFit                Fit the parameters for a Normal distribution
#  snormFit               Fit the parameters for a skew Normal distribution
#  gedFit                 Fit the parameters for a GED distribution
#  sgedFit                Fit the parameters for a skew GED distribution
#  stdFit                 Fit the parameters for a Sudent-t distribution
#  sstdFit                Fit the parameters for a skew Sudent-t distribution
################################################################################


normFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fit the parameters for a Normal distribution
    
    # FUNCTION:
    
    # For S-Plus compatibility:
    if (!exists("nlm")) 
        nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
        
    # Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)))

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dnorm(y, x[1], x[2])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


snormFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fit the parameters for a skew Normal distribution
    
    # FUNCTION:
    
    # For S-Plus compatibility:
    if (!exists("nlm")) 
        nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
        
    # Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), xi = 1)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dsnorm(y, x[1], x[2], x[3])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


gedFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fit the parameters for a GED distribution
    
    # FUNCTION:
    
    # For S-Plus compatibility:
    if (!exists("nlm")) 
        nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
        
    # Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 2)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dged(y, x[1], x[2], x[3])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


sgedFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fit the parameters for a skew GED distribution
    
    # FUNCTION:
    
    # For S-Plus compatibility:
    if (!exists("nlm")) 
        nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
        
    # Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 2, xi = 1)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dsged(y, x[1], x[2], x[3], x[4])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


stdFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fit the parameters for a Sudent-t distribution
    #   with unit variance
    
    # FUNCTION:
    
    # For S-Plus compatibility:
    if (!exists("nlm")) 
        nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
        
    # Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 4)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dstd(y, x[1], x[2], x[3])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    Names = c("mean", "sd", "nu")
    names(fit$estimate) = Names
    names(fit$gradient) = Names
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


sstdFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fit the parameters for a skew Sudent-t distribution
    #   with unit variance
    
    # FUNCTION:
    
    # For S-Plus compatibility:
    if (!exists("nlm")) 
        nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
    
    # Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 4, xi = 1)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dsstd(y, x[1], x[2], x[3], x[4])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    Names = c("mean", "sd", "nu", "xi")
    names(fit$estimate) = Names
    names(fit$gradient) = Names
    
    # Return Value:
    fit
}   


################################################################################

