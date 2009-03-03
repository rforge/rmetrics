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
#  timeSeries                Creates a 'timeSeries' object from scratch
################################################################################

## .signalSeries : generate units, title, documentation if NULL
##                 data must be a matrix
.signalSeries <-
    function(data, charvec, units = NULL, format, zone = "",
             FinCenter = "", recordIDs = data.frame(), title = NULL,
             documentation = NULL, ...)
{
    stopifnot(inherits(data, "matrix"))

    # Add units, title and Documentation:
    if (is.null(units))
        units <- colnames(data)
    if (is.null(units))
        units <- paste("SS.", seq.int(dim(data)[2]), sep = "")
    if (is.null(title)) title = "Signal Series Object"
    if (is.null(documentation)) documentation = as.character(date())

    # remove rownames of data but keep colnames for
    # functions like var, cov ...
    # Note that if it fails, new("timeSeries" should fail to - normal
    try(dimnames(data) <- list(NULL, units), silent = TRUE)

###     new("signalSeries",
###         .Data = data,
###         units = units,
###         recordIDs = recordIDs,
###         title = title,
###         documentation = documentation)

    new("timeSeries",
        .Data = data,
        units = units,
        positions = numeric(0),
        FinCenter = "",
        format = "counts",
        recordIDs = recordIDs,
        title = title,
        documentation = documentation)

}

## .timeSeries : generate units, title, documentation if NULL
##               data must be a matrix and charvec a timeDate object
.timeSeries <-
    function(data, charvec, units = NULL, format, zone = "",
             FinCenter = "", recordIDs = data.frame(), title = NULL,
             documentation = NULL, ...)
{

    stopifnot(inherits(data, "matrix"))
    stopifnot(is.numeric(charvec))

###     # it should be safe to only check first elt
###     # this avoid checking the whole vec ...
###     if (is.na(charvec[1]))
###         return(.signalSeries(data = data, units = units,
###                              recordIDs = recordIDs,
###                              title = title,
###                              documentation = documentation, ...))

    # Add units, title and Documentation:
    if (is.null(units))
        units <- colnames(data)
    if (is.null(units))
        units <- paste("TS.", seq.int(dim(data)[2]), sep = "")
    if (is.null(title)) title = "Time Series Object"
    if (is.null(documentation)) documentation = as.character(date())
    if (missing(format))
        format <- "%Y-%m-%d"
    if (identical("", FinCenter))
        FinCenter <- "GMT"

    # remove rownames of data but keep colnames for
    # functions like var, cov ...
    # Note that if it fails, new("timeSeries" should fail to - normal
    try(dimnames(data) <- list(NULL, units), silent = TRUE)

    positions <- charvec # as.numeric(charvec, "sec")
    attributes(positions) <- NULL

    new("timeSeries",
        .Data = data,
        positions =  positions,
        units = units,
        format = format, # charvec@format,
        FinCenter = FinCenter, # charvec@FinCenter,
        recordIDs = recordIDs,
        title = title,
        documentation = documentation)
}

# ------------------------------------------------------------------------------

## # to generate all combinations one can do
## data = c("missing", "matrix", "ANY")
## charvec = c("missing", "timeDate", "numeric", "Date", "POSIXct", "POSIXlt", "ANY")
## grid <- expand.grid(data = data, charvec = charvec)
## grid[order(grid$data),]

## missing  missing
## missing timeDate
## missing      ANY
##  matrix  missing
##  matrix timeDate
##  matrix      ANY
##     ANY  missing
##     ANY timeDate
##     ANY      ANY

# ------------------------------------------------------------------------------
## missing ANY

setMethod("timeSeries",
          signature(data = "missing", charvec = "ANY"),
          function (data, charvec, units = NULL, format = NULL, zone = "",
                    FinCenter = "", recordIDs = data.frame(), title = NULL,
                    documentation = NULL, ...)
          .signalSeries(data = matrix(NA),
                        units = units,
                        recordIDs = recordIDs,
                        title = title,
                        documentation = documentation,
                        ...)
          )

setMethod("timeSeries",
          signature(data = "ANY", charvec = "ANY"),
          function (data, charvec, units = NULL, format = NULL, zone = "",
                    FinCenter = "", recordIDs = data.frame(), title = NULL,
                    documentation = NULL, ...)
      {
          t <- try(data <- as.matrix(data))
          if (inherits(t, "try-error"))
              stop("Could not coerce 'data' to a matrix")
          callGeneric()
      })

## # ------------------------------------------------------------------------------
## ## missing  missing

## setMethod("timeSeries",
##           signature(data = "missing", charvec = "missing"),
##           function (data, charvec, units = NULL, format = NULL, zone = "",
##                     FinCenter = "", recordIDs = data.frame(), title = NULL,
##                     documentation = NULL, ...)
##           .signalSeries(data = matrix(NA),
##                         units = units,
##                         recordIDs = recordIDs,
##                         title = title,
##                         documentation = documentation,
##                         ...)
##           )

## # ------------------------------------------------------------------------------
## ## missing timeDate

## setMethod("timeSeries",
##           signature(data = "missing", charvec = "timeDate"),
##           function (data, charvec, units = NULL, format = NULL, zone = "",
##                     FinCenter = "", recordIDs = data.frame(), title = NULL,
##                     documentation = NULL, ...)
##           .signalSeries(data = matrix(NA),
##                         units = units,
##                         recordIDs = recordIDs,
##                         title = title,
##                         documentation = documentation,
##                         ...)
##           )

## # ------------------------------------------------------------------------------
## ## missing      ANY

## setMethod("timeSeries",
##           signature(data = "missing", charvec = "ANY"),
##           function (data, charvec, units = NULL, format = NULL, zone = "",
##                     FinCenter = "", recordIDs = data.frame(), title = NULL,
##                     documentation = NULL, ...)
##           .signalSeries(data = matrix(NA),
##                         units = units,
##                         recordIDs = recordIDs,
##                         title = title,
##                         documentation = documentation,
##                         ...)
##           )

# ------------------------------------------------------------------------------
##  matrix  missing

setMethod("timeSeries",
          signature(data = "matrix", charvec = "missing"),
          function (data, charvec, units = NULL, format = NULL, zone = "",
                    FinCenter = "", recordIDs = data.frame(), title = NULL,
                    documentation = NULL, ...)
      {
          charvec <- rownames(data)
          if (is.null(charvec)) {
              .signalSeries(data = data, units = units,
                            recordIDs = recordIDs, title = title,
                            documentation = documentation, ...)
           } else {
               callGeneric(data = data,
                          charvec = charvec,
                          units = units,
                          format = format,
                          zone = zone,
                          FinCenter = FinCenter,
                          recordIDs = recordIDs,
                          title = title,
                          documentation = documentation,
                          ...)
           }
      })

# ------------------------------------------------------------------------------
##  matrix timeDate

setMethod("timeSeries",
          signature(data = "matrix", charvec = "timeDate"),
          function (data, charvec, units = NULL, format = NULL, zone = "",
                    FinCenter = "", recordIDs = data.frame(), title = NULL,
                    documentation = NULL, ...)
      {
          if (any(!c(zone, FinCenter) %in% ""))
              charvec <- timeDate(charvec, format = format,
                                  zone = zone, FinCenter = FinCenter)
          .timeSeries(data = data,
                      charvec = as.numeric(charvec, "sec"),
                      units = units,
                      format = charvec@format,
                      FinCenter = charvec@FinCenter,
                      recordIDs = recordIDs,
                      title = title,
                      documentation = documentation, ...)
      })

# ------------------------------------------------------------------------------
##  matrix numeric

setMethod("timeSeries",
          signature(data = "matrix", charvec = "numeric"),
          function (data, charvec, units = NULL, format = NULL, zone = "",
                    FinCenter = "", recordIDs = data.frame(), title = NULL,
                    documentation = NULL, ...)
      {
          .timeSeries(data = data,
                      charvec = charvec,
                      units = units,
                      recordIDs = recordIDs,
                      title = title,
                      documentation = documentation, ...)
      })

# ------------------------------------------------------------------------------
##  matrix      ANY

setMethod("timeSeries",
          signature(data = "matrix", charvec = "ANY"),
          function (data, charvec, units = NULL, format = NULL, zone = "",
                    FinCenter = "", recordIDs = data.frame(), title = NULL,
                    documentation = NULL, ...)
      {
          # if charvec NULL returns a signal series
          if (is.null(charvec))
              return(.signalSeries(data = data, units = units,
                                   recordIDs = recordIDs, title = title,
                                   documentation = documentation, ...))

          # coerce charvec to timeDate
          charvec <- timeDate(charvec = charvec, format = format,
                              zone = zone, FinCenter = FinCenter)

          if (any(is.na(charvec)))
              # Note : there is already a warning in timeDate if there are NA's
              .signalSeries(data = data, units = units,
                            recordIDs = recordIDs, title = title,
                            documentation = documentation, ...)
          else
              .timeSeries(data = data,
                          charvec = as.numeric(charvec, "sec"),
                          units = units,
                          format = charvec@format,
                          FinCenter = charvec@FinCenter,
                          recordIDs = recordIDs,
                          title = title,
                          documentation = documentation,
                          ...)
      })

# ------------------------------------------------------------------------------
##     ANY  missing

## setMethod("timeSeries",
##           signature(data = "ANY", charvec = "missing"),
##           function (data, charvec, units = NULL, format = NULL, zone = "",
##                     FinCenter = "", recordIDs = data.frame(), title = NULL,
##                     documentation = NULL, ...)
##       {
##           t <- try(data <- as.matrix(data))
##           if (inherits(t, "try-error"))
##               stop("Could not coerce 'data' to a matrix")
##           callGeneric()
##       })

## # ------------------------------------------------------------------------------
## ##     ANY timeDate

## setMethod("timeSeries",
##           signature(data = "ANY", charvec = "timeDate"),
##           function (data, charvec, units = NULL, format, zone = "",
##                     FinCenter = "", recordIDs = data.frame(), title = NULL,
##                     documentation = NULL, ...)
##       {
##           t <- try(data <- as.matrix(data))
##           if (inherits(t, "try-error"))
##               stop("Could not coerce 'data' to a matrix")
##           callGeneric()
##       })

## # ------------------------------------------------------------------------------
## ##     ANY      ANY

## setMethod("timeSeries",
##           signature(data = "ANY", charvec = "ANY"),
##           function (data, charvec, units = NULL, format = NULL, zone = "",
##                     FinCenter = "", recordIDs = data.frame(), title = NULL,
##                     documentation = NULL, ...)
##       {
##           t <- try(data <- as.matrix(data))
##           if (inherits(t, "try-error"))
##               stop("Could not coerce 'data' to a matrix")
##           callGeneric()
##       })

################################################################################
