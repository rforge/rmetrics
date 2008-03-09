
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
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION:
#  isUnivariate              Tests if 'timeSeries' object is univariate
#  isMultivariate            Tests if 'timeSeries' object is multivariate
################################################################################


isUnivariate <- 
    function(x)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests if a time series or rectangular object is univariate

    # FUNCTION:

    # Return Value:
    if (NCOL(x) == 1) return(TRUE) else return(FALSE)
}


# ------------------------------------------------------------------------------


isMultivariate <- 
    function(x)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests if a time series or rectangular object is multivariate

    # Examples:
    #   isMultivariate(as.timeSeries(data(daxRet)))

    # FUNCTION:

    # Return Value:
    if (NCOL(x) > 1) return(TRUE) else return(FALSE)
}


################################################################################

