
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
# FUNCTION:             DESCRIPTION:
#  dght                  Hyperbolic Distribution - Skew Symmetric Student-t
################################################################################


dght =
function(x, beta = 1e-6, delta = 1, mu = 0, nu = 10, log = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Hyperbolic Distribution - Skew Symmaetric Student-t:
    #   dght(x, beta = 0.1, delta = 1, mu = 0, nu = 10, log = FALSE) 
        
    # FUNCTION:
    
    # Density:
    D = sqrt( delta^2 + (x-mu)^2 )
    A1 = ((1-nu)/2) * log(2)
    A2 = nu * log(delta) 
    A3 = ((nu+1)/2) * log(abs(beta))
    A4 = log(besselK(abs(beta)*D, (nu+1)/2, expon.scaled = TRUE)) - abs(beta)*D
    A5 = beta*(x-mu)
    B1 = lgamma(nu/2) 
    B2 = log(sqrt(pi))
    B3 = ((nu+1)/2) * log(D)
    
    # Log:
    ans = (A1 + A2 + A3 + A4 + A5) - (B1 + B2 + B3)
    if (!log) ans = exp(ans)
    
    # Return Value:
    ans
}


################################################################################

