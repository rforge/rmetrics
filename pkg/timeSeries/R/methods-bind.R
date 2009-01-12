#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################
# FUNCTION:                 DESCRIPTION:
#  cbind.timeSeries          Binds columns of two 'timeSeries' objects
#  rbind.timeSeries          Binds rows of two 'timeSeries' objects
################################################################################

setMethod("cbind2",
          c("timeSeries", "timeSeries"),
          function(x, y)
      {
          # A function implemented by Diethelm Wuertz and Yohan Chalabi

          # Description:
          #   Merges two 'timeSeries' objects

          # Arguments:
          #   x, y - 'timeSeries' objects

          # Value:
          #   Returns a S4 object of class 'timeSeries'.

          # FUNCTION:

          test = as.integer((x@format == "counts") + (y@format == "counts"))
          switch(as.character(test),
                 # convert series y to FinCenter of series x
                 "0" = { FinCenter <- finCenter(y) <- finCenter(x) },
                 # if one of the two series are signal series, the other
                 # series is converted to a signal series
                 "1" = { x <- timeSeries(x, format = "counts");
                       y <- timeSeries(y, format = "counts") })

          ###  # YC: unnecessary if c(time(x), time(y)) is used in
          ###  # next statement

          ###  # check if x and y have same date format,
          ###  # if not convert to the most extended one
          ###  if (y@format != x@format) {
          ###      if (nchar(y@format) > nchar(x@format)) {
          ###          x@positions <- format(time(x), format = y@format)
          ###          rownames(x) <- x@positions
          ###          x@format <- y@format
          ###      } else {
          ###          y@positions <- format(time(y), format = x@format)
          ###          rownames(y) <- y@positions
          ###          y@format <- x@format
          ###      }
          ###  }

          # Manipulate in matrix form:
          positions <- as.character(c(time(x), time(y)))
          LENGTH <- nrow(x)
          DUP1 <- duplicated(positions)[1:LENGTH]
          DUP2 <- duplicated(positions)[-(1:LENGTH)]
          # YC : avoid troubles with different format in @positions
          M1 <- getDataPart(x)
          M2 <- getDataPart(y)
          rownames(M1) <- positions[1:LENGTH]
          rownames(M2) <- positions[-(1:LENGTH)]
          dim1 <- dim(M1)
          dim2 <- dim(M2)
          X1 <- matrix(rep(NA, times = dim1[1]*dim2[2]), ncol = dim2[2])
          X2 <- matrix(rep(NA, times = dim2[1]*dim1[2]), ncol = dim1[2])
          # YC : Does not seem to be necessary
          # colnames(X1) <- colnames(M2)
          NC <- (dim1 + dim2)[2]+1
          Z <- rbind(cbind(M1, X1, DUP1), cbind(X2, M2, DUP2))
          Z <- Z[order(rownames(Z)), ]
          NC1 <- dim1[2]+1
          IDX <- (1:(dim1+dim2)[1])[Z[, NC] == 1]
          Z[IDX-1, NC1:(NC-1)] <- Z[IDX, NC1:(NC-1)]
          Z <- Z[!Z[, NC], -NC]

          # FIXME : what about @recordIDs ?

          units <- c(colnames(x), colnames(y))
          # change colnames if there are the same
          if (length(unique(units)) != length(units)) {
              for (name in unique(units)) {
                  pos <- grep(name, units)
                  if (length(pos) != 1)
                      units[pos] <- paste(units[pos], seq(pos), sep = ".")
              }
          }

          # Create time series:
          timeSeries( data = Z, charvec = rownames(Z), zone =
              finCenter(x), FinCenter = finCenter(x), units = units)
      })

# ------------------------------------------------------------------------------

setMethod("cbind2", c("timeSeries", "ANY"),
          function(x,y) callGeneric(x, as(y, "timeSeries")))
setMethod("cbind2", c("ANY", "timeSeries"),
          function(x,y) callGeneric(as(x, "timeSeries"), y))
setMethod("cbind2", c("timeSeries", "missing"), function(x,y) x)

# ------------------------------------------------------------------------------

setMethod("rbind2", c("timeSeries", "timeSeries"),
          function(x, y)
      {
          # A function implemented by Diethelm Wuertz and Yohan Chalabi

          # Check Arguments:
          stopifnot(dim(x)[2] == dim(y)[2])

          test = as.integer((x@format == "counts") + (y@format == "counts"))
          switch(as.character(test),
                 # convert series y to FinCenter of series x
                 "0" = { FinCenter <- finCenter(y) <- finCenter(x) },
                 # if one of the two series are signal series, the other
                 # series is converted to a signal series
                 "1" = { x <- timeSeries(x, format = "counts");
                       y <- timeSeries(y, format = "counts") })

          ###  # check if x and y have same date format,
          ###  # if not convert to the most extended one
          ###  if (y@format != x@format) {
          ###      if (nchar(y@format) > nchar(x@format)) {
          ###          x@positions <- format(time(x), format = y@format)
          ###          rownames(x) <- x@positions
          ###          x@format <- y@format
          ###      } else {
          ###          y@positions <- format(time(y), format = x@format)
          ###          rownames(y) <- y@positions
          ###          y@format <- x@format
          ###      }
          ###  }

          # Bind:
          positions <- as.character(c(time(x), time(y)))
          data <- rbind(getDataPart(x), getDataPart(y))
          # YC : Does not seem necessary
          # if (x@format == "counts") positions <- as.numeric(positions)
          recordIDs <- as.data.frame(rbind(x@recordIDs, y@recordIDs))

          # Order series
          order <- order(positions)
          data <- data[order,,drop=FALSE]
          positions <- positions[order]
          recordIDs <- recordIDs[order,,drop=FALSE]
          units <- paste(colnames(x), colnames(y), sep = "_")

          timeSeries(data = data, charvec = positions, zone =
                     finCenter(x), FinCenter = finCenter(x), units = units)
      })

# ------------------------------------------------------------------------------

setMethod("rbind2", c("timeSeries", "ANY"),
          function(x,y) callGeneric(x, as(y, "timeSeries")))
setMethod("rbind2", c("ANY", "timeSeries"),
          function(x,y) callGeneric(as(x, "timeSeries"), y))
setMethod("rbind2", c("timeSeries", "missing"), function(x,y) x)

################################################################################
