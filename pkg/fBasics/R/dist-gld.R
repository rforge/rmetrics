
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


################################################################################
# FUNCTION:             DESCRIPTION:
#  .dgld                 Returns density for Generalized Lambda DF
#   .xgld                 Utility function
#  .pgld                 Returns probability for Generalized Lambda DF
#  .qgld                 Returns quantiles for Generalized Lambda DF 
#  .rgld                 Returns random variates for Generalized Lambda DF
################################################################################


# DW:
#   This a simple implementation for the GLD using the RS parameterisation.
#   Note it doe not check for parameter consistency.
#   See also the packages gld,  GLDEX, and Davies for GLD functions


.dgld <-
    function (x, lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns density for Generalized Lambda DF 
    
    # Example:
    #   .dgld( (-25:25)/10 )
    
    # FUNCTION:
    
    # Density:
    p = .pgld(x, lambda1, lambda2, lambda3, lambda4)
    d = .xgld(p, lambda2, lambda3, lambda4)
    
    # Return Value:
    d
}


# ------------------------------------------------------------------------------


.xgld <-
    function (x, lambda2=-1, lambda3=-1/8, lambda4=-1/8) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Utility Function
      
    # FUNCTION:
    
    # Density:
    d = 1 / (lambda3*x^(lambda3-1) + lambda4*(1-x)^(lambda4-1)) / lambda2
      
    # Return Value:
    d
}


# ------------------------------------------------------------------------------


.pgld <-
    function (q, lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns probability for Generalized Lambda DF
     
    # Example:
    #   .pgld( (-25:25)/10 )
    
    # FUNCTION:
    
    # Probability:
    f <- function(p, lambda1, lambda2, lambda3, lambda4, q) { 
        .qgld(p = p, lambda1, lambda2, lambda3, lambda4)  - q }
    p = rep(NA, times = length(q))
    for (i in 1:length(q))
    p[i] = uniroot(f, c(0, 1), tol = 1e-06, 
        lambda1=lambda1, lambda2=lambda2, lambda3=lambda3, lambda4=lambda4,
        q=q[i])$root

    
    # Return Value:
    p
}


# ------------------------------------------------------------------------------


.qgld <-
    function (p, lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns quantiles for Generalized Lambda DF 
      
    # Example:
    #   .qgld((1:99)/100)
    
    # FUNCTION:
    
    # Quantiles:
    q = lambda1 + (p^lambda3 - (1 - p)^lambda4) / lambda2
    
    # Return Value:
    q
}


# ------------------------------------------------------------------------------


.rgld <-
    function (n = 100, lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns random variates for Generalized Lambda DF 
      
    # FUNCTION:
    
    # Random Variates:
    r = .qgld(runif(n), lambda1, lambda2, lambda3, lambda4)
    
    # Return Value:
    r    
}


################################################################################

