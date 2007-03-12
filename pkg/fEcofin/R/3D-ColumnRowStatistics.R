
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
# FUNCTION:             ROW STATISTICS:
#  rowStats              Computes sample statistics by row
#   rowAvgs               Computes sample mean by row
#   rowVars               Computes sample variance by row
#   rowStdevs             Computes sample variance by row
#   rowSkewness           Computes sample skewness by row
#   rowKurtosis           Computes sample kurtosis by row
#   rowCumsums            Computes sample cumulated sums by row
# FUNCTION:             COLUMN STATISTICS:
#  colStats              Computes sample statistics by column
#   colAvgs               Computes sample mean by column
#   colVars               Computes sample variance by column
#   colStdevs             Computes sample variance by column
#   colSkewness           Computes sample skewness by column
#   colKurtosis           Computes sample kurtosis by column
#   colCumsums            Computes sample cumulated sums by column
################################################################################


################################################################################
# FUNCTION:             ROW STATISTICS:
#  rowStats              Computes sample statistics by row
#   rowAvgs               Computes sample mean by row
#   rowVars               Computes sample variance by row
#   rowStdevs             Computes sample variance by row
#   rowSkewness           Computes sample skewness by row
#   rowKurtosis           Computes sample kurtosis by row
#   rowCumsums            Computes sample cumulated sums by row


rowStats = 
function(x, FUN, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample statistics by column
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = FUN, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = FUN, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowAvgs = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample mean by column
    
    # Note:
    #   R's base package comes already with a colMeans!

    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = mean, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = mean, ...) 
    }
    result = t(t(result))
    colnames(result) = "Mean"
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowVars = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample variance by column
 
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = var, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = var, ...) 
    }
    result = t(t(result))
    colnames(result) = "Variance"
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowStdevs = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample standard deviation by column
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = sqrt(apply(na.remove(x), MARGIN = 1, FUN = var, ...))
    } else {
        result = sqrt(apply(x, MARGIN = 1, FUN = var, ...))
    }
    result = t(t(result))
    colnames(result) = "StDev"
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowSkewness = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample skewness by column
   
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = skewness, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = skewness, ...) 
    }
    result = t(t(result))
    colnames(result) = "Skewness"
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowKurtosis = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample kurtosis by column

    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 1, FUN = kurtosis, ...) 
    } else {
        result = apply(x, MARGIN = 1, FUN = kurtosis, ...) 
    }
    result = t(t(result))
    colnames(result) = "Kurtosis"
        
    # Return Value:
    result 
}


# ------------------------------------------------------------------------------


rowCumsums = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column
      
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = cumsum, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = cumsum, ...) 
    }
    colnames(result) = paste(1:ncol(x))
        
    # Return Value:
    result
}


################################################################################
# FUNCTION:             COLUMN STATISTICS:
#  colStats              Computes sample statistics by column
#   colAvgs               Computes sample mean by column
#   colVars               Computes sample variance by column
#   colStdevs             Computes sample variance by column
#   colSkewness           Computes sample skewness by column
#   colKurtosis           Computes sample kurtosis by column
#   colCumsums            Computes sample cumulated sums by column


colStats = 
function(x, FUN, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample statistics by column
  
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = FUN, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = FUN, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colAvgs = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample mean by column
    
    # Note:
    #   R's base package comes already with a colMeans!
  
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm){
        result = apply(na.remove(x), MARGIN = 2, FUN = mean, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = mean, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colVars = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample variance by column
 
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) { 
        result = apply(na.remove(x), MARGIN = 2, FUN = var, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = var, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colStdevs = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample standard deviation by column

    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = sqrt(apply(na.remove(x), MARGIN = 2, FUN = var, ...))
    } else {
        result = sqrt(apply(x, MARGIN = 2, FUN = var, ...))
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colSkewness = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample skewness by column
 
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = skewness, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = skewness, ...) 
    }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colKurtosis = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample kurtosis by column

    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = kurtosis, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = kurtosis, ...) 
    }
        
    # Return Value:
    result 
}


# ------------------------------------------------------------------------------


colCumsums = 
function(x, na.rm = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Statistics:
    if (na.rm) {
        result = apply(na.remove(x), MARGIN = 2, FUN = cumsum, ...) 
    } else {
        result = apply(x, MARGIN = 2, FUN = cumsum, ...) 
    }
        
    # Return Value:
    result 
}
  

################################################################################

