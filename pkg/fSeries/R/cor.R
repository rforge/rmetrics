
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
# FUNCTION:                 COLUMN STATISTICS IN FUTILITIES:
#  cov.timeSeries            Returns  covariance for a 'timeSeries' object
#  cor.timeSeries            Returns correlations for a 'timeSeries' object
################################################################################


# Defined in fUtilites/BasicExtensions.R
# cov = function() UseMethod("cov")
# cov.default <- stats::cov
# cor = function() UseMethod("cor")
# cov.default <- stats::cov


# Add robust methods ...


cov.timeSeries =
function(x, y = NULL, 
use = "all.obs", 
method = c("pearson", "kendall", "spearman"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns variance/covariance for a 'timeSeries' object
    
    # FUNCTION:
    
    # Settings:
    x = x@Data
    if (!is.null(y)) y = y@Data
    
    # CoVariance:
    ans = cov.default(x, y, use = use, method = method) 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


cor.timeSeries =
function(x, y = NULL, 
use = "all.obs", 
method = c("pearson", "kendall", "spearman"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns correlations for a 'timeSeries' object
    
    # FUNCTION:
    
    # Settings:
    x = x@Data
    if (!is.null(y)) y = y@Data
    
    # CoVariance:
    ans = cor.default(x, y, use = use, method = method) 
    
    # Return Value:
    ans
}

   
################################################################################

