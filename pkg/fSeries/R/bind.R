
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
#  merge.timeSeries          Merges two 'timeSeries' objects
#  cbind.timeSeries          Binds columns of two 'timeSeries' objects
#  rbind.timeSeries          Binds rows of two 'timeSeries' objects
#  c.timeseries              Binds rows of two 'timeSeries' objects
################################################################################

merge.timeSeries <-
    function(x, y, units = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    # Modified by Yohan chalabi

    # Description:
    #   Merges two 'timeSeries' objects

    # Arguments:
    #   x, y - 'timeSeries' objects

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:

    # convert series y to FinCenter of series x
    y <- as.timeSeries(y, zone = y@FinCenter, FinCenter = x@FinCenter)

    # check if x and y have same date format,
    # if not convert to the most extended one
    if (y@format != x@format) {
        if (nchar(y@format) > nchar(x@format)) {
            x@positions <- format(time(x), format = y@format)
            rownames(x@Data) <- x@positions
            x@format <- y@format
        } else {
            y@positions <- format(time(y), format = x@format)
            rownames(y@Data) <- y@positions
            y@format <- x@format
        }
    }

    # Manipulate in matrix form:
    positions <- as.character(c(x@positions, y@positions))
    LENGTH <- length(as.character(x@positions))
    DUP1 <- duplicated(positions)[1:LENGTH]
    DUP2 <- duplicated(positions)[-(1:LENGTH)]
    M1 <- as.matrix(x)
    M2 <- as.matrix(y)
    dim1 <- dim(M1)
    dim2 <- dim(M2)
    X1 <- matrix(rep(NA, times = dim1[1]*dim2[2]), ncol = dim2[2])
    X2 <- matrix(rep(NA, times = dim2[1]*dim1[2]), ncol = dim1[2])
    colnames(X1) <- colnames(M2)
    NC <- (dim1 + dim2)[2]+1
    Z <- rbind(cbind(M1, X1, DUP1), cbind(X2, M2, DUP2))
    Z <- Z[order(rownames(Z)), ]
    NC1 <- dim1[2]+1
    IDX <- (1:(dim1+dim2)[1])[Z[, NC] == 1]
    Z[IDX-1, NC1:(NC-1)] <- Z[IDX, NC1:(NC-1)]
    Z <- Z[!Z[, NC], -NC]

    if (is.null(units)) units <- c(x@units, y@units)

    # Create time series:
    ans <- timeSeries(data = Z, charvec = rownames(Z), zone =
                      x@FinCenter, FinCenter = x@FinCenter, units = units, ...)

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

cbind.timeSeries <- merge.timeSeries

# ------------------------------------------------------------------------------


rbind.timeSeries <-
    function(x, y, units = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    # Modified by Yohan chalabi

    # Check Arguments:
    stopifnot(is.timeSeries(x) & is.timeSeries(y))
    stopifnot(dim(x)[2] == dim(y)[2])

    y <- as.timeSeries(y, zone = y@FinCenter, FinCenter = x@FinCenter)

    # check if x and y have same date format,
    # if not convert to the most extended one
    if (y@format != x@format) {
        if (nchar(y@format) > nchar(x@format)) {
            x@positions <- format(time(x), format = y@format)
            rownames(x@Data) <- x@positions
            x@format <- y@format
        } else {
            y@positions <- format(time(y), format = x@format)
            rownames(y@Data) <- y@positions
            y@format <- x@format
        }
    }

    # Bind:
    data <- as.matrix(rbind(x@Data, y@Data))
    positions <- c(x@positions, y@positions)
    recordIDs <- as.data.frame(rbind(x@recordIDs, y@recordIDs))

    # Order series
    order <- order(positions)
    data <- data[order,]
    positions <- positions[order]
    zone <- FinCenter <- x@FinCenter
    recordIDs <- recordIDs[order,]

    ans <- timeSeries(data = data, charvec = positions,
                      zone = zone, FinCenter = FinCenter,
                      units = units, ...)

    # Return Value
    ans
}

# ------------------------------------------------------------------------------

c.timeSeries <- rbind.timeSeries

################################################################################

