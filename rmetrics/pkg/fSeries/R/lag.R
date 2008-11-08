
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
# FUNCTION:                 DESCRIPTION:
#  lag.timeSeries            Lags a 'timeSeries' object  
################################################################################


lag.timeSeries = 
function(x, k = 1, trim = FALSE, units = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Lags 'timeSeries' objects.
    
    # Arguments:
    #   x - a 'timeSeries' object.
    #   k - an integer indicating which lag to use. 
    #       By default 1.
    #   trim - a logical. Should NAs at the befinning of the
    #       series be removed? 
    
    # Value:
    #   Returns a lagged object of class 'timeSeries'.

    # FUNCTION:
    
    # Column Names:
    colNames = units
    
    # Internal Function:
    tslagMat = function(x, k = 1) {
        # Internal Function:
        tslag1 = function(x, k) {
            y = x
            if (k > 0) y = c(rep(NA, times = k), x[1:(length(x)-k)])
            if (k < 0) y = c(x[(-k+1):length(x)], rep(NA, times = -k))
            y }
        # Bind:
        ans = NULL
        for (i in k) {
            ans = cbind(ans, tslag1(x, i)) }
        # As Vector:
        if (length(k) == 1) ans = as.vector(ans)
        # Return Value:
        ans }
        
    # Convert:
    y = as.data.frame(x)
    y = as.matrix(y)
    Dim = dim(y)[2]
    
    # Lag on each Column:
    z = NULL
    for (i in 1:Dim) {
        ts = tslagMat( y[, i], k = k)     #, trim = FALSE)
        z = cbind(z, ts) }
    
    # Add Names:
    rownames(z) = rownames(y)    
    colnames(z) = rep(colnames(y), each = length(k)) 
        
    # Return Value:
    ans = timeSeries(data = z, charvec = rownames(z), units = colnames(z),
        format = x@format, FinCenter = x@FinCenter,
        title = x@title, documentation = x@documentation)
    
    # Trim:
    if (trim) {
        idx = !is.na(apply(ans@Data, 1, sum))
        ans = ans[idx,] 
    }
        
    # Augment Colnames:
    a = colnames(z)
    kcols = rep(k, times = ncol(y))
    b = paste("[", kcols, "]", sep="") 
    ab = paste(a, b, sep = "")
    colnames(ans@Data) <- ab
    
    # Record IDs:
    df = x@recordIDs
    if (trim) {
        if (sum(dim(df)) > 0) {
            TRIM = dim(df)[1] - dim(ans)[1]
            df = df[-(1:TRIM), ]
        }
    }
    ans@recordIDs = df
    
    # Return Value:
    ans      
}

   
################################################################################

