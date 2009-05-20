
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


################################################################################
# FUNCTION:                DESCRIPTION:
#  .garchSetCondDist        Selects conditional density function
#  .garchDist               Defines conditional density function
#  .normCondDist            Normal Distribution
#  .QLMECondDist            QLME with Normal Distribution
#  .snormCondDist           Skew Normal Distribution 
#  .stdCondDist             Student-t Distribution
#  .sstdCondDist            Skew Student-t Distribution
#  .gedCondDist             Generalized Error Distribution
#  .sgedCondDist            Skew Generalized Error Distribution
#  .snigCondDist            Normal Inverse Gaussian Distribution
#  .setfGarchEnv            Set fGarch environment for conditional distribution                  
################################################################################


.garchSetCondDist <-
function(cond.dist = "norm")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Select Conditional Density Function

    # Arguments:
    #   cond.dist - a character string with the name of the
    #       conditional distribution function. Valid strings are:
    #       "norm", "snorm", "std", "sstd", "ged", "sged", "snig".

    # Value:
    #   Returns the selection conditional distribution function
    #   named uniquely '.garchDist'.

    # Details:
    #   Implemented Distributions:
    #    norm - Normal Distribution: nothing to estimate
    #    snorm - Skew Normal Distribution: xi may be estimated
    #    std - Student-t Distribution: nu may be estimated
    #    sstd - Skew Student-t Distribution: nu and xi may be estimated
    #    ged - Generalized Error Distribution: nu may be estimated
    #    sged - Skew Generalized Error Distribution: nu and xi may be estimated

    # FUNCTION:

    # Compose Function:
    fun = paste(".", cond.dist, "CondDist", sep = "")
    .garchDist = match.fun(fun)
    
    
    # Trace the Result:
    if(FALSE) {
        cat("\n Distribution:     ", cond.dist, "\n    .garchDist = ")
        print(.garchDist)
    }

    # Return Value:
    .garchDist
}


# ------------------------------------------------------------------------------


.normCondDist <- 
function(z, hh, skew, shape)
{
    # FUNCTION:
    
    # Normal Distribution:
    # Use fGarch::dnorm 
    .garchDist = function(z, hh, skew, shape) {
        dnorm(x = z/hh, mean = 0, sd = 1) / hh  
    }
}
    

# ------------------------------------------------------------------------------


.QLMECondDist <- 
function(z, hh, skew, shape)
{
    # FUNCTION:
    
    # Normal Distribution:
    # Use fGarch::dnorm
    .garchDist = function(z, hh, skew, shape) {  
        dnorm(x = z/hh, mean = 0, sd = 1) / hh  
    }
}
        
        
# ------------------------------------------------------------------------------

  
.snormCondDist <- 
function(z, hh, skew, shape)
{
    # FUNCTION:
    
    # Skew Normal Distribution:
    # Use fGarch::dsnorm
    .garchDist = function(z, hh, skew, shape) {
        dsnorm(x = z/hh, mean = 0, sd = 1, xi = skew) / hh  
    }
}
    

# ------------------------------------------------------------------------------

   
.stdCondDist <- 
function(z, hh, skew, shape)
{   
    # FUNCTION:
    
    # Standardized Student-t Distribution:
    # Use fGarch::dstd
    .garchDist = function(z, hh, skew, shape) {
        dstd(x = z/hh, mean = 0, sd = 1, nu = shape) / hh  
    }
}
    

# ------------------------------------------------------------------------------

   
.sstdCondDist <- 
function(z, hh, skew, shape)
{      
    # FUNCTION:
    
    # Skew Standardized Student-t Distribution:
    # Use fGarch::dsstd
    .garchDist = function(z, hh, skew, shape) {
        dsstd(x = z/hh, mean = 0, sd = 1, nu = shape, xi = skew) / hh
    }
}

    

# ------------------------------------------------------------------------------

   
.gedCondDist <- 
function(z, hh, skew, shape)
{
    # FUNCTION:
    
    # Generalized Error Distribution:
    # Use fGarch::dged
    .garchDist = function(z, hh, skew, shape) {
        dged(x = z/hh, mean = 0, sd = 1, nu = shape) / hh
    }
}
    

# ------------------------------------------------------------------------------

   
.sgedCondDist <- 
function(z, hh, skew, shape)
{

    # FUNCTION:
    
    # Skew Generalized Error Distribution:
    # Use fGarch::dsged
    .garchDist = function(z, hh, skew, shape) {
        dsged(x = z/hh, mean = 0, sd = 1, nu = shape, xi = skew) / hh
    }
}
    

# ------------------------------------------------------------------------------

   
.snigCondDist <- 
function(z, hh, skew, shape)
{

    # FUNCTION:
    
    # (Skew) Normal Inverse Gaussian Distribution:
    # Use fBasics::dsnig
    .garchDist = function(z, hh, skew, shape) {
        dsnig(x = z/hh, zeta = shape, rho = skew) / hh
    }
}
 


# ------------------------------------------------------------------------------
   

.setfGarchEnv(.garchDist = .garchSetCondDist("norm"))

    
################################################################################

