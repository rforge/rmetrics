
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
# You should have received A copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:            DESCRIPTION:
#  dsnig                Returns density of the snig distribution
#  psnig                Returns probabilities of the snig distribution
#  qsnig                Returns quantiles of the snig distribution
#  rsnig                Generates snig distributed random variates
################################################################################


dsnig <-  
    function(x, zeta = 1, rho = 0, log = FALSE) 
{
    # Description:
    #   Returns density of the snig distribution
    
    # FUNCTION:
 
    # Compute Density - Quick and Dirty:
    dsgh(x, zeta, rho, lambda = -0.5, log = log)
}


# ------------------------------------------------------------------------------


psnig <-  
    function(q, zeta = 1, rho = 0) 
{
    # Description:
    #   Returns probabilities of the snig distribution
    
    # FUNCTION:
    
    # Compute Probabilities - Quick and Dirty:
    psgh(q, zeta, rho, lambda = -0.5)
}


# ------------------------------------------------------------------------------


qsnig <-  
    function(p, zeta = 1, rho = 0) 
{
    # Description:
    #   Returns quantiles of the snig distribution
    
    # FUNCTION:
    
    # Compute Quantiles:
    qsgh(p, zeta, rho, lambda = -0.5)
}


# ------------------------------------------------------------------------------


rsnig <-  
    function(n, zeta = 1, rho = 0) 
{
    # Description:
    #   Generates snig distributed random variates
    
    # FUNCTION:
    
    # Generate Random Numbers:
    rsgh(n, zeta, rho, lambda = -0.5)
}


################################################################################

