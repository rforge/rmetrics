
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port: 
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                 DESCRIPTION:
#  rsocpControl              Control list for "rsocp" Solver
################################################################################

        
rsocpControl <- 
    function(abs.tol = 1e-18, rel.tol = 1e-16, target = 0, 
    max.iter = 500, Nu = 10, out.mode = 0, BigM.K = 2, BigM.iter = 5, 
    scale = 1, penalty = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   A copy of function socpControl from package Rsocp
    
    # Note:
    #   Here we have added the entries scale and penealty
    
    # FUNCTION:
    
    # Return Value:
    list(
        abs.tol = abs.tol, 
        rel.tol = rel.tol,
        target = target,
        max.iter = max.iter,
        Nu = Nu,
        out.mode = out.mode,
        BigM.K = BigM.K,
        BigM.iter = BigM.iter,
        scale = scale,
        penalty = penalty)
}


################################################################################

