
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
# FUNCTION:                 DESCRIPTION:                                                                                                   
#  Sys.putenv                depreciated after 2.4.1   
#  head.ts                   Adds head method                        
################################################################################


.conflicts.OK = TRUE


# ------------------------------------------------------------------------------


if (!exists("Sys.setenv"))
{
    Sys.setenv =
    function(...)
    {
        x <- list(...)
        nm <- names(x)
        val <- as.character(unlist(x))
        x <- paste(nm, val, sep = "=")
        invisible(.Internal(putenv(x)))
    }
}


################################################################################


## head.ts <- 
##     function(x, n = 6, ...)
## {
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Adds head method
    
    # Example:
    #   ts = ts(rnorm(50)); head(ts)
    #   mts = ts(cbind(rnorm(50), rnorm(50))); head(mts) 
    
    # FUNCTION:
    
##     if (NCOL(x) == 1) {
##         return(stats::as.ts(x[1:n], ...)) 
##     } else {
##         return(stats::as.ts(x[1:n, ], ...))
##     }
    
## }


################################################################################

