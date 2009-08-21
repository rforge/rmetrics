
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
#   1999 - 2009, Rmetrics Association, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org


################################################################################
# FUNCTION:           DESCRIPTION:
#  .nigMean            Computes the mean of the normal inverse Gaussian PDF
#  .nigVar             Computes the variance of the normal inverse Gaussian PDF
#  .nigSkew            Computes the skewness of the normal inverse Gaussian PDF
#  .nigKurt            Computes the kurtosis of the normal inverse Gaussian PDF
################################################################################


.nigMean <- 
    function(alpha = 1, beta = 0, delta = 1, mu = 0)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the mean of the Normal Inverse Gaussian PDF
    
    # FUNCTION:
    
    # Mean:
    gamma = sqrt(alpha^2 - beta^2)
    ans = mu + delta * beta / gamma
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.nigVar <- 
    function(alpha = 1, beta = 0, delta = 1, mu = 0)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the variance of the Normal Inverse Gaussian PDF
    
    # FUNCTION:
    
    # Variance:
    gamma = sqrt(alpha^2 - beta^2)
    ans = delta * alpha^2 / gamma^3
    
    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


.nigSkew <- 
    function(alpha = 1, beta = 0, delta = 1, mu = 0)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the skewness of the Normal Inverse Gaussian PDF
    
    # FUNCTION:
    
    # Skewness:
    gamma = sqrt(alpha^2 - beta^2)
    ans = 3*beta / ( alpha * sqrt(delta*gamma) ) 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.nigKurt <- 
    function(alpha = 1, beta = 0, delta = 1, mu = 0)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the excess kurtosis of the Normal Inverse Gaussian PDF
    
    # FUNCTION:
    
    # Skewness:
    gamma = sqrt(alpha^2 - beta^2)
    ans = 3 * ( 1 + 4 * beta^2 / alpha^2) / (delta * gamma)
    
    # Return Value:
    ans
}


################################################################################

