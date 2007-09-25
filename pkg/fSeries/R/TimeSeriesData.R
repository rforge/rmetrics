
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
#  fapply                    Applies a function to 'timeSeries' windows 
#  .align.timeSeries         Aligns a timeSeries object
################################################################################


fapply =
function(x, from, to, FUN, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Applies a function to 'timeSeries' windows
    
    # Details:
    #   This function can be used to aggregate and coursen a 
    #   'timeSeries' object.
    
    # Arguments:
    #   x - a 'timeSeries' object to be aggregated
    #   from, to - two 'timeDate' position vectors which size the blocks
    #   FUN - function to be applied, by default 'colAvgs'
    
    # Value:
    #   Returns a S4 object of class 'timeSeries' if FUN returns 
    #   a time series object, otherwise a list, where the entries
    #   for each window is the output of the function FUN.
    
    # Notes:
    #   The size of the 'moving' window and the selection of an
    #   'adj'-acent endpoint are not needed, all the information
    #   is kept in the 'from' and 'to' position vectors.
  
    # FUNCTION:
    
    # Check object:
    if (class(x) != "timeSeries") stop("s is not a timeSeries object")
    
    # Monthly and Quarterly from and to:
    if (is.null(from) & is.null(to)) {
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
        from@FinCenter = to@FinCenter = x@FinCenter
    }
    
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
    
    # Compute for the first window ...
    i = 1
    test = (j.pos >= j.from[i] & j.pos <= j.to[i])
    # make sure that cutted is a matrix ...
    cutted = as.matrix(y[test, ])
    ### if (sum(test)>0) rownames(cutted) <- rowNames[test]
    ans = fun(cutted, ...)
    
    if (is.timeSeries(ans)) {
        ## DW can this happen - check ?
        rowBind = ans
        for (i in 2:from@Dim) {
            test = (j.pos >= j.from[1] & j.pos <= j.to[1])
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
        ans = timeSeries(data = rowBind, charvec = as.character(to), 
            units = units, format = format, zone = x@zone, FinCenter = 
            x@FinCenter, recordIDs = x@recordIDs, title = x@title, 
            documentation = x@documentation, ...) 
        return(ans)
    } else {  
        listBind = list()
        ## DW [] -> [[]]
        listBind[[1]] = ans
        for (i in 2:from@Dim) {
            test = (j.pos >= j.from[i] & j.pos <= j.to[i])
            # make sure that cutted is a matrix ...
            cutted = as.matrix(y[test, ])
            ### if (sum(test)>0) rownames(cutted) <- rowNames[test]
            ans = fun(cutted, ...)
            ## DW [] -> [[]]
            listBind[[i]] = ans 
        }
        # Return Value:
        ans = listBind
        attr(ans, "control") <- list(x = x, from = from, to = to)
        return(invisible(ans))
    }
    
    # Return Value:
    return()
} 


# ------------------------------------------------------------------------------


.align.timeSeries = 
function(x, method = c("before", "after", "interp"), startOn = "hours",
by = "30 m")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Aligns a 'timeSeries' object
    
    # FUNCTION:
 
    # Settings:
    method = match.arg(method)
    numberOfColumns = dim(x)[2]
    typeMethod = c(interp = "linear", before = "constant", after = "constant")
    fMethod = c(interp = 0.5, before = 0, after = 1)
    by = .by2seconds(by)
    
    # Convert to GMT:
    tD = timeDate(x@positions, zone = x@FinCenter, FinCenter = "GMT")

    # Convert to POSIX:
    Positions = as.POSIXct(tD, tz = "GMT")
    N = length(Positions)
    Start = as.POSIXct(trunc(Positions[1], startOn), tz = "GMT")  
    End   = as.POSIXct(trunc(Positions[N], startOn), tz = "GMT") + 24*3600
    print(Start)
    print(End)
    
    # Compute Positions:
    X = as.integer(difftime(Positions, Start, units = "sec"))
    
    # Compute New Positions:
    timeDiff = as.integer(difftime(End, Start, units = "secs"))
    lengthOut = trunc(timeDiff/by) + 1
    posix = seq(from = Start, by = paste(by, "sec"), length.out = lengthOut)
    newX = as.integer(difftime(posix, Start, units = "secs"))  
    
    # Align:  
    matY = NULL
    for (i in 1:numberOfColumns) {
        Y = as.vector(x[, i])
        newY = approx(X, Y, xout = newX, method = typeMethod[method], 
            f = fMethod[method])$y
        matY = cbind(matY, newY)
        
    }

    # Create Series in local time:
    print(head(as.character(posix)))
    ans = timeSeries(matY, as.character(posix), 
        units = x@units, zone = "GMT", FinCenter = x@FinCenter,
        recordIDs = data.frame())
    
    # Return Value:
    ans       
}

   
################################################################################

