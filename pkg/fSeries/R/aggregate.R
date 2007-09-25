
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
#  aggregate.timeSeries      Aggregates a 'timeSeries' object
################################################################################


aggregate.timeSeries =
function(x, by = c("monthly", "quarterly"), FUN = colMeans, units = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Aggregates a 'timeSeries' object
    
    # Details:
    #   This function can be used to aggregate and coursen a 
    #   'timeSeries' object.
    
    # Arguments:
    #   x - a 'timeSeries' object to be aggregated
    #   by - calendarical block, only active when both 'from' 
    #       and 'to' are NULL
    #   FUN - function to be applied, by default 'colAvgs'
    #   units - a character vector with column names, allows to 
    #       overwrite the column names of the input 'timeSeries'
    #       object.
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:
    
    # Settings:
    format = x@format
    zone = x@FinCenter
    FinCenter = x@FinCenter
    recordIDs = data.frame()
    title = x@title
    documentation = x@documentation
    
    # Check object:
    stopifnot(is.timeSeries(x))
    
    # Monthly and Quarterly from and to timeDate Objects:
    if (by[1] == "monthly") {
        # Use monthly blocks:
        from = unique(timeFirstDayInMonth(seriesPositions(x)))
        to = unique(timeLastDayInMonth(seriesPositions(x)))
    } else if (by[1] == "quarterly") {
        from = unique(timeFirstDayInQuarter(seriesPositions(x)))
        to = unique(timeLastDayInQuarter(seriesPositions(x)))
    } else {
        stop("by must be eiter monthly or quarterly")
    } 
    from@FinCenter = to@FinCenter = FinCenter
    
    # Column Names:
    colNames = units
    
    # Function:
    fun = match.fun(FUN)
    
    # Blocks:
    j.pos = as.POSIXct(seriesPositions(x))
    j.from = as.POSIXct(from)
    j.to = as.POSIXct(to)
    
    # Iterate:
    y = x@Data
    pos = seriesPositions(x)
    rowNames = rownames(x@Data)
    rowBind = NULL
    for (i in 1:from@Dim) {
        test = (j.pos >= j.from[i] & j.pos <= j.to[i])
        # make sure that cutted is a matrix ...
        cutted = as.matrix(y[test, ])
        ### if (sum(test)>0) rownames(cutted) <- rowNames[test]
        ans = fun(cutted, ...)
        rowBind = rbind(rowBind, ans) 
    }
    rownames(rowBind) = as.character(to)
    if (is.null(colNames)) {
        units = x@units 
    } else {
        units = colNames 
    }
    
    # Return Value:
    timeSeries(data = rowBind, charvec = as.character(to), units = units, 
        format = format, zone = zone, FinCenter = FinCenter, recordIDs = 
        recordIDs, title = title, documentation = documentation, ...)       
} 

   
################################################################################

