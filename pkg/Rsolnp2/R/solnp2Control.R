
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
#  solnp2Control            Returns control list
################################################################################


# Package: Rsolnp
# Type: Package
# Title: Non-linear Programming with non-linear Constraints
# Version: 0.3
# Date: 2009-09-15
# Author: Alexios Ghalanos and Stefan Theussl
# Maintainer: Alexios Ghalanos <alexios@4dscape.com>
# Depends: stats
# Description: Non-linear Optimization Using Augmented Lagrange Multiplier 
#    Method Version for Rmetrics Portfolio Optimization
# LazyLoad: yes
# License: GPL


################################################################################


# control list
#   RHO  : penalty parameter
#   MAJIT: maximum number of major iterations
#   MINIT: maximum number of minor iterations
#   DELTA: relative step size in forward difference evaluation
#   TOL  : tolerance on feasibility and optimality
# defaults RHO=1, MAJIT=10, MINIT=10, DELTA=1.0e-5, TOL=1.0e-4


################################################################################


solnp2Control <-
function(rho = 1, majit = 10, minit = 10, delta = 1e-5, tol = 1e-4)
{
    # Description:
    #   Returns control list
    
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

        