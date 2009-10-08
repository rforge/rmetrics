
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
#  nlminb2Control           Returns control list
################################################################################


nlminb2Control <- 
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
        eval.max = 500, 
        iter.max = 400,
        trace = 0,
        abs.tol = 1e-20, 
        rel.tol = 1e-10,
        x.tol = 1.5e-8, 
        step.min = 2.2e-14,
        scale = 1,
        R = 1,
        beta = 0.01,
        steps.max = 10,
        steps.tol = 1e-6)
        
   # Return Value:
   ans
}


################################################################################

