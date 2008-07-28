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
# METHOD:                   SUBSETTING METHODS ON DATA:
#  [,timeSeries              Subsets of a 'timeSeries' object
#  [<-,timeSeries            Assign value to subsets of a 'timeSeries' object
################################################################################

################################################################################
# index
################################################################################
setClassUnion("index_timeSeries",
              members =  c("numeric", "logical", "character"))

################################################################################
#  [,timeSeries              Subsets of a 'timeSeries' object
################################################################################

.subset_timeSeries <-
    function(x, i, j)
{

    if (is(i, "character")) {
        pos <- time(x)
        i <-
            if (timeDate:::.subsetCode(i) == "SPAN")
                # Subsetting by Span Indexing:
                timeDate:::.subsetBySpan(pos, i)
            else
                # Subsetting by Python Indexing:
                timeDate:::.subsetByPython(pos, i)
        i <- pmatch(as.character(i),
                    slot(x, "positions"), duplicates.ok = TRUE)
        if (any(is.na(i)))
            stop("subscript out of bounds", call. = FALSE)
    }
    if (is(j, "character")) {
        j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
        if (any(is.na(j)))
            stop("subscript out of bounds", call. = FALSE)
    }

    # subset data and positions
    slot(x, ".Data") <- .subset(slot(x, ".Data"), i, j, drop = FALSE)
    slot(x, "positions") <- .subset(slot(x, "positions"), i, drop = FALSE)
    slot(x, "units") <- .subset(slot(x, "units"), j, drop = FALSE)

    # Record IDs:
    df <- slot(x, "recordIDs")
    slot(x, "recordIDs") <- if (prod(dim(df))) df[i, , drop = FALSE] else df

    # Result
    x
}

# ------------------------------------------------------------------------------
# index_timeSeries

setMethod("[",
          signature(x = "timeSeries", i = "index_timeSeries",
                         j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
              .subset_timeSeries(x, i, j))


setMethod("[",
          signature(x = "timeSeries", i = "index_timeSeries",
                         j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if(nargs() == 2) {
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[i]
          } else {
              .subset_timeSeries(x, i, TRUE)
          }
      })

setMethod("[",
          signature(x = "timeSeries", i = "missing",
                         j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
          .subset_timeSeries(x, TRUE, j))

# ------------------------------------------------------------------------------
# timeDate

setMethod("[",
          signature(x = "timeSeries", i = "timeDate",
                    j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {
          # series settings
          if (x@format != "counts") {
              FinCenter <- finCenter(x)
              # convert FinCenter of index_timeSeries to FinCenter of timeSeries
              i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
          }
          i <- as(i, "character")

          .subset_timeSeries(x, i, j)
      })

setMethod("[",
          signature(x = "timeSeries", i = "timeDate", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          # series settings
          if (x@format != "counts") {
              FinCenter <- finCenter(x)
              # convert FinCenter of index_timeSeries to FinCenter of timeSeries
              i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
          }

          i <- as(i, "character")

          .subset_timeSeries(x, i, TRUE)
      })

# ------------------------------------------------------------------------------
# matrix

setMethod("[",
          signature(x = "timeSeries", i = "matrix", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
          .subset_timeSeries(x, as.vector(i), j))

setMethod("[",
          signature(x = "timeSeries", i = "matrix", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if(nargs() == 2) {
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[i]
          } else {
              .subset_timeSeries(x, as.vector(i), TRUE)
          }
      })

# ------------------------------------------------------------------------------
# timeSeries

setMethod("[",
          signature(x = "timeSeries", i = "timeSeries", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {
          if (x@format != "counts" &&
              i@format != "counts" &&
              finCenter(x) != finCenter(i))
              stop("FinCenter of timeSeries and subset do not match")

          .subset_timeSeries(x, as.vector(i), j)
      })

setMethod("[",
          signature(x = "timeSeries", i = "timeSeries", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if (x@format != "counts" &&
              i@format != "counts" &&
              finCenter(x) != finCenter(i))
              stop("FinCenter of timeSeries and subset do not match")

          if(nargs() == 2) {
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[as.vector(i)]
          } else {
              .subset_timeSeries(x, as.vector(i), TRUE)
          }
      })

# ------------------------------------------------------------------------------
# all missing

setMethod("[",
          signature(x = "timeSeries", i = "missing", j = "missing"),
          function(x, i, j, ..., drop = FALSE) x)

# ------------------------------------------------------------------------------
# ANY

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "ANY"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------

################################################################################
#  [<-,timeSeries            Assign value to subsets of a 'timeSeries' object
################################################################################

# ------------------------------------------------------------------------------

.assign_timeSeries <-
    function(x, i, j, value)
{
    if (is(i, "character")) {
        # i <- match(i, x@positions)
        # not sure if better to use pmatch
        i <- pmatch(i, slot(x, "positions"), duplicates.ok = TRUE)
        if (any(is.na(i)))
            stop("subscript out of bounds", call. = FALSE)
    }
    if (is(j, "character")) {
        j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
        if (any(is.na(j)))
            stop("subscript out of bounds", call. = FALSE)
    }

    x@.Data[i, j] <- value

    # Result
    x
}

# ------------------------------------------------------------------------------
# index_timeSeries

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "index_timeSeries",
                           j = "index_timeSeries"),
                 function(x, i, j, value)
                 .assign_timeSeries(x, i, j, value))

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "index_timeSeries",
                           j = "missing"),
                 function(x, i, j, value)
                 .assign_timeSeries(x, i, TRUE, value))

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "missing",
                           j = "index_timeSeries"),
                 function(x, i, j, value)
                 .assign_timeSeries(x, TRUE, j, value))

# ------------------------------------------------------------------------------
# timeDate

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "timeDate",
                           j = "index_timeSeries"),
                 function(x, i, j, value)
             {
                 # series settings
                 if (x@format != "counts") {
                     FinCenter <- finCenter(x)
                     # convert FinCenter of index_timeSeries to
                     # FinCenter of timeSeries
                     i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
                 }

                 i <- as(i, "character")

                 .assign_timeSeries(x, i, j, value)
             })

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "timeDate", j = "missing"),
                 function(x, i, j, value)
             {

                 # series settings
                 if (x@format != "counts") {
                     FinCenter <- finCenter(x)
                     # convert FinCenter of index_timeSeries to
                     # FinCenter of timeSeries
                     i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
                 }

                 i <- as(i, "character")

                 .assign_timeSeries(x, i, TRUE, value)
             })

# ------------------------------------------------------------------------------
# matrix

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "matrix",
                           j = "index_timeSeries"),
                 function(x, i, j, value)
                 .assign_timeSeries(x, as.vector(i), j))

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "matrix", j = "missing"),
                 function(x, i, j, value)
                     .assign_timeSeries(x, as.vector(i), TRUE))

# ------------------------------------------------------------------------------
# timeSeries

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "timeSeries",
                           j = "index_timeSeries"),
                 function(x, i, j, value)
             {
                 if (x@format != "counts" &&
                     i@format != "counts" &&
                     finCenter(x) != finCenter(i))
                     stop("FinCenter of timeSeries and subset do not match")

                 .assign_timeSeries(x, as.vector(i), j, value)
             })

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "timeSeries", j = "missing"),
                 function(x, i, j, value)
             {
                 if (x@format != "counts" &&
                     i@format != "counts" &&
                     finCenter(x) != finCenter(i))
                     stop("FinCenter of timeSeries and subset do not match")

                 .assign_timeSeries(x, as.vector(i), TRUE, value)
             })

# ------------------------------------------------------------------------------
# all missing

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "missing", j = "missing"),
                 function(x, i, j, value) {x@.Data[] <- value; x})

# ------------------------------------------------------------------------------
# ANY

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "ANY", j = "ANY"),
                 function(x, i, j, value)
                 stop("invalid or not-yet-implemented 'timeSeries' assignment"))

################################################################################
