
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:
#  normalTest            Normality tests S-Plus compatible
################################################################################


normalTest <- 
    function(x, method = c("sw", "jb"), na.rm = FALSE) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Shapiro-Wilk and Jarque-Bera Test
    
    # Notes:
    #   This function is for S-Plus compatibility

    # FUNCTION:
    
    # Convert Type:
    if (class(x) == "fREG") x = residuals(x)
    x = as.vector(x)
    if (na.rm) x = x[!is.na(x)]
    
    # Method:
    #   Don't use: method = match.arg(method)
    method = method[1]
    
    # Test:
    if (method == "sw") {
        ans = shapiroTest(x) 
    } else if (method == "jb") {
       ans = jarqueberaTest(x)
    }
    
    # Additional Tests:
    if (method == "ks") {
        ans = ksnormTest(x)
    }
    if (method == "da") {
        ans = dagoTest(x)
    }
    if (method == "ad") {
        ans = adTest(x)
    }
 
    # Return Value:
    ans
}


################################################################################

