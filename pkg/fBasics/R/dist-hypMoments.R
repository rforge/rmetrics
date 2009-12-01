
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
# FUNCTION:                     DESCRIPTION:
#  hypMean                       Returns raw mean
#  hypVar                        Returns raw variance
#  hypSkew                       Returns raw skewness
#  hypKurt                       Returns raw kurtosis
################################################################################


hypMean <-
function(alpha=1, beta=0, delta=1, mu=0)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    mean = .ghMuMoments(k=1, alpha, beta, delta, mu, lambda=1)[[1]]
    mean
}


# ------------------------------------------------------------------------------


hypVar <- 
function(alpha=1, beta=0, delta=1, mu=0)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    var = .ghMuMoments(k=2, alpha, beta, delta, mu, lambda=1)[[1]]
    var
}


# ------------------------------------------------------------------------------


hypSkew <- 
function(alpha=1, beta=0, delta=1, mu=0)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Moments
    k2 = .ghMuMoments(k=2, alpha, beta, delta, mu, lambda=1)[[1]] 
    k3 = .ghMuMoments(k=3, alpha, beta, delta, mu, lambda=1)[[1]]
    
    # Return Value:
    skew = k3/(k2^(3/2))     
    skew          
}


# ------------------------------------------------------------------------------


hypKurt <- 
function(alpha=1, beta=0, delta=1, mu=0)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Moments:
    k2 = .ghMuMoments(k=4, alpha, beta, delta, mu, lambda=1)[[1]]
    k4 = .ghMuMoments(k=4, alpha, beta, delta, mu, lambda=1)[[1]]

    # Return Value:
    kurt = k4/k2^2 - 3 
    kurt
}


################################################################################

