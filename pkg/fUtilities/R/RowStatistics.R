
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
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 row STATISTICS:
#  rowStats                  Computes sample statistics by row
#  rowSums                   Computes sums of all values in each row
#  rowMeans                  Computes means of all values in each row
#  rowSds                    Computes standardard deviation of each row
#  rowVars                   Computes sample variance by row
#  rowSkewness               Computes sample skewness by row
#  rowKurtosis               Computes sample kurtosis by row
#  rowMaxs                   Computes maximum values in each row
#  rowMins                   Computes minimum values in each row
#  rowProds                  Computes product of all values in each row
# FUNCTION:                 NO LONGER USED:
#  rowAvgs                   Computes sample mean by row
#  rowStdevs                 Computes sample variance by row
################################################################################


################################################################################
# @comments
#   DW 2007-09-23           completely new implemented
#   DW 2007-09-20           further row* functions added, see ADDONS
################################################################################


################################################################################
# @todo
#
################################################################################



.conflicts.OK = TRUE


rowStats =
function(x, FUN, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample statistics by row

    # FUNCTION:
    
    # Statistics:
    apply(na.omit(as.matrix(x), ...), 1, FUN, ...)
}


rowSums = 
function(x, ...) 
{
    if (class(x) == "timeSeries") {
        return(rowStats(x, "sum", ...))
    } else {
        return(base::rowSums(x, ...))
    }
}
    

rowMeans = 
function(x, ...)
{
    if (class(x) == "timeSeries") {
        return(rowStats(x, "mean", ...))
    } else {
        return(base::rowMeans(x, ...))
    }
}


rowSds = function(x, ...) { rowStats(x, "sd", ...) }       
rowVars = function(x, ...) { rowStats(x, "var", ...) }      
rowSkewness = function(x, ...) { rowStats(x, "skewness", ...) }   
rowKurtosis = function(x, ...) { rowStats(x, "kurtosis", ...) } 
rowMaxs = function(x, ...) { rowStats(x, "max", ...) } 
rowMins = function(x, ...) { rowStats(x, "min", ...) }      
rowProds = function(x, ...) { rowStats(x, "prod", ...) } 


rowQuantiles = 
function(x, prob = 0.05, ...) { 
    stopifnot(length(prob) == 1)
    rowStats(x, "quantile", probs = prob, ...) } 


rowAvgs <- rowMeans
rowStdevs <- rowSds


################################################################################

    