
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
#  donlp2Control            Returns control list
################################################################################


donlp2Control <- 
    function()         
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Control list
    
    # Arguments:
    #   none
    
    # FUNCTION:
    
    # Control list:
    ans = list(
        iterma = 4000,
        nstep = 20, 
        fnscale = 1,
        report = FALSE, 
        rep.freq = 1,
        tau0 = 1.0, 
        tau = 0.1, 
        del0 = 1.0,
        epsx = 1.0e-5, 
        delmin = 0.1, # suggested 0.1*del0
        epsdif = 1e-8, 
        nreset.multiplier = 1,
        difftype = 3, 
        epsfcn = 1.0e-16, 
        taubnd = 1.0,
        hessian = FALSE,
        te0 = TRUE, 
        te1 = FALSE, 
        te2 = FALSE, 
        te3 = FALSE,
        silent = TRUE,
        intakt = TRUE)
        
    # Return Value:
    ans
}


################################################################################

