
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
# FUNCTION:                 COLUMN STATISTICS:
#  colStats                  Computes sample statistics by column
#  colSums                   Computes sums of all values in each column
#  colMeans                  Computes means of all values in each column
#  colSds                    Computes standardard deviation of each column
#  colVars                   Computes sample variance by column
#  colSkewness               Computes sample skewness by column
#  colKurtosis               Computes sample kurtosis by column
#  colMaxs                   Computes maximum values in each colum
#  colMins                   Computes minimum values in each colum
#  colProds                  Computes product of all values in each colum
# FUNCTION:                 NO LONGER USED:
#  colAvgs                   Computes sample mean by column
#  colStdevs                 Computes sample standard deviation by column
#  mean.timeSeries           Computes sample means by column
#  var.timeSeries            Computes sample variance by column
################################################################################


.conflicts.OK = TRUE


# ------------------------------------------------------------------------------


colStats <- 
    function(x, FUN, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample statistics by column

    # FUNCTION:
    
    # Statistics:
    apply(na.omit(as.matrix(x), ...), 2, FUN, ...)
}


# ------------------------------------------------------------------------------


colSums <- 
    function(x, ...) 
{
    # FUNCTION:
    
    if (class(x) == "timeSeries") {
        return(colStats(x, "sum", ...))
    } else {
        return(base::colSums(x, ...))
    }
}
    

# ------------------------------------------------------------------------------


colMeans <- 
    function(x, ...)
{
    # FUNCTION:
    
    if (class(x) == "timeSeries") {
        return(colStats(x, "mean", ...))
    } else {
        return(base::colMeans(x, ...))
    }
}


# ------------------------------------------------------------------------------


colSds <- function(x, ...) { colStats(x, "sd", ...) }       
colVars <- function(x, ...) { colStats(x, "var", ...) }      
colSkewness <- function(x, ...) { colStats(x, "skewness", ...) }   
colKurtosis <- function(x, ...) { colStats(x, "kurtosis", ...) } 
colMaxs <- function(x, ...) { colStats(x, "max", ...) } 
colMins <- function(x, ...) { colStats(x, "min", ...) }      
colProds <- function(x, ...) { colStats(x, "prod", ...) } 


# ------------------------------------------------------------------------------


colQuantiles <-  
function(x, prob = 0.05, ...) 
{ 
    # FUNCTION:
    
    stopifnot(length(prob) == 1)
    colStats(x, "quantile", probs = prob, ...) 
} 


# ------------------------------------------------------------------------------


colAvgs <- colMeans
colStdevs <- colSds


# ------------------------------------------------------------------------------


mean.timeSeries <- colMeans
var.timeSeries <- colVars


################################################################################

    