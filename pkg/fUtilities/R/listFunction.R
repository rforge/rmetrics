
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
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION:
#  listFunctions             Lists all functions in a package
#  countFunctions            Counts all functions in a package
################################################################################


listFunctions <- 
    function(package) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Lists all functions in a package
    
    # Arguments:
    #   package - a character string, the name of the Package

    # Author:
    #   Original code from B. Ripley
    
    # FUNCTION:
    
    # List:
    if(require(package, character.only = TRUE)) {
        env <- paste("package", package, sep = ":")
        nm <- ls(env, all = TRUE)
        ans = nm[unlist(lapply(nm, function(n) exists(n, where = env,
            mode = "function", inherits = FALSE)))]
    } else {
        ans = character(0)
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


countFunctions <- 
    function(package) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Counts all functions in a package
    
    # Arguments:
    #   package - a character string, the name of the Package
    
    # FUNCTION:
    
    # Count:
    ans = length(.listFunctions(package))
    names(ans) = package
    
    # Return Value:
    ans
}


################################################################################

