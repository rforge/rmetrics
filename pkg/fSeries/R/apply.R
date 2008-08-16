
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
        for (i in 2:length(from)) {
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
        for (i in 2:length(from)) {
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


################################################################################

