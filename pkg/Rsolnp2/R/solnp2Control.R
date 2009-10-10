
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
# FUNCTION:                  DESCRIPTION
#  solnp2Control              Returns control list
################################################################################


solnp2Control <-
function(rho = 1, majit = 10, minit = 10, delta = 1e-5, tol = 1e-4)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns control list
    
    # Arguments:
    #   rho - a numeric value. The penalty parameter 
    #   majit - an integer value. The maximum number of major iterations 
    #   minit - an integer value. The maximum number of minor iterations 
    #   delta - a numeric value. The relative step size in forward 
    #       difference evaluation 
    #   tol - a numeric value. The tolerance on feasibility and optimality 
    
    # FUNCTION:
    
    # Control List:
    ans <- list(
        rho = rho,
        majit = majit,
        minit = minit,
        delta = delta,
        tol = tol)
        
    # Return Value:
    ans
}


################################################################################

        