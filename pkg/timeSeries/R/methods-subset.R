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

# ------------------------------------------------------------------------------
# index
setClassUnion("index_timeSeries", members =  c("numeric", "logical"))

.subset_timeSeries <-
    function(x, i, j)
{

    # subset data and positions
    x@.Data <- .subset(x, i, j, drop = FALSE)
    x@positions <- .subset(x@positions, i, drop = FALSE)
    x@units <- .subset(x@units, j, drop = FALSE)

    # to handle special case when
    # series(x)[i, j, drop = drop] returns numeric(0)
    #  if (!NROW(data) || !NCOL(data)) data <- NULL

    # Record IDs:
    df <- x@recordIDs
    x@recordIDs <- if (prod(dim(df))) df[i, , drop = FALSE] else df

    # Result
    x
}

setMethod("[",
          signature(x = "timeSeries", i = "index_timeSeries",
                         j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {
          .subset_timeSeries(x, i, j)
      })

setMethod("[",
          signature(x = "timeSeries", i = "index_timeSeries",
                         j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if(nargs() == 2) {
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[i]
          } else {
              .subset_timeSeries(x, i = i, j = min(1, NCOL(x)):NCOL(x))
          }
      })

setMethod("[",
          signature(x = "timeSeries", i = "missing",
                         j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
          .subset_timeSeries(x, i = min(1, NROW(x)):NROW(x), j = j))


setMethod("[",
          signature(x = "timeSeries", i = "missing", j = "missing"),
          function(x, i, j, ..., drop = FALSE) x)


# ------------------------------------------------------------------------------
# character

setMethod("[",
          signature(x = "timeSeries", i = "character", j = "character"),
          function(x, i, j, ..., drop = FALSE)
      {
          i <- (x@positions %in% i)
          j <- (x@units %in% j)

          if (!any(i) || !any(j))
              stop("subscript out of bounds")

          .subset_timeSeries(x, i=i, j=j)
      })

setMethod("[",
          signature(x = "timeSeries", i = "character",
                         j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {
          i <- (x@positions %in% i)

          if (!any(i))
              stop("subscript out of bounds")

          .subset_timeSeries(x, i=i, j=j)
      })

setMethod("[",
          signature(x = "timeSeries", i = "character", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          i <- (x@positions %in% i)

          if (!any(i))
              stop("subscript out of bounds")

          .subset_timeSeries(x, i=i, j = min(1, NCOL(x)):NCOL(x))
      })

setMethod("[",
          signature(x = "timeSeries", i = "index_timeSeries", j = "character"),
          function(x, i, j, ..., drop = FALSE)
      {
          j <- (x@units %in% j)

          if (!any(j))
              stop("subscript out of bounds")

          .subset_timeSeries(x, i=i, j=j)
      })

setMethod("[",
          signature(x = "timeSeries", i = "missing", j = "character"),
          function(x, i, j, ..., drop = FALSE)
      {
          # j <- (x@units %in% j)
          ## DW & MB:
          ##    Must be !!!
          j <- match(j, x@units)
          
          if (!any(j))
              stop("subscript out of bounds")

          .subset_timeSeries(x, i = min(1, NROW(x)):NROW(x), j=j)
      })


# ------------------------------------------------------------------------------
# timeDate

setMethod("[",
          signature(x = "timeSeries", i = "timeDate", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {
          # series settings
          if (x@format != "counts") {
              FinCenter <- finCenter(x)
              # convert FinCenter of index_timeSeries to FinCenter of timeSeries
              i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
          }

          i <- as(i, "character")

          i <- (x@positions %in% i)
          if (!any(i))
              stop("subscript out of bounds")

          .subset_timeSeries(x, i=i, j=j)
      })

setMethod("[",
          signature(x = "timeSeries", i = "timeDate", j = "character"),
          function(x, i, j, ..., drop = FALSE)
      {
          # series settings
          if (x@format != "counts") {
              FinCenter <- finCenter(x)
              # convert FinCenter of index_timeSeries to FinCenter of timeSeries
              i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
          }

          i <- as(i, "character")

          i <- (x@positions %in% i)
          j <- (x@units %in% j)

          if (!any(i) || !any(j))
              stop("subscript out of bounds")

          .subset_timeSeries(x, i=i, j=j)
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

          i <- (x@positions %in% i)
          if (!any(i))
              stop("subscript out of bounds")

          .subset_timeSeries(x, i = i, j = min(1, NCOL(x)):NCOL(x))
      })

# ------------------------------------------------------------------------------
# matrix

setMethod("[",
          signature(x = "timeSeries", i = "matrix", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
          .subset_timeSeries(x, i = as.vector(i), j = j))

setMethod("[",
          signature(x = "timeSeries", i = "matrix", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if(nargs() == 2) {
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[i]
          } else {
              .subset_timeSeries(x, i = as.vector(i), j = min(1, NCOL(x)):NCOL(x))
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

          .subset_timeSeries(x, i = as.vector(i), j = j)
      })

setMethod("[",
          signature(x = "timeSeries", i = "timeSeries", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if(nargs() == 2) {
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[as.vector(i)]
          } else {
              .subset_timeSeries(x, i = as.vector(i),
                                j = min(1, NCOL(x)):NCOL(x))
          }
      })

# ------------------------------------------------------------------------------
# ANY

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "ANY"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------

.assign_timeSeries <- function(x, i, j, value) { x[i, j] <- value; x }

# ------------------------------------------------------------------------------
# character

setMethod("[<-",
          signature(x = "timeSeries", i = "character", j = "character"),
          function(x, i, j, value)
      {
          i <- (x@positions %in% i)
          j <- (x@units %in% j)
          if (!any(i) || !any(j)) stop("subscript out of bounds")

          .assign_timeSeries(x, i, j, value)

      })

setMethod("[<-",
          signature(x = "timeSeries", i = "character"),
          function(x, i, j, value)
      {
          i <- (x@positions %in% i)
          if (!any(i)) stop("subscript out of bounds")

          .assign_timeSeries(x, i, j, value)

      })

setMethod("[<-",
          signature(x = "timeSeries", i = "character", j = "missing"),
          function(x, i, j, value)
      {
          i <- (x@positions %in% i)
          if (!any(i)) stop("subscript out of bounds")

          .assign_timeSeries(x, i, j = min(1, NCOL(x)):NCOL(x), value)

      })

setMethod("[<-",
          signature(x = "timeSeries", j = "character"),
          function(x, i, j, value)
      {
          j <- (x@units %in% j)

          if (!any(j))
              stop("subscript out of bounds")

          .assign_timeSeries(x, i, j, value)

      })

setMethod("[<-",
          signature(x = "timeSeries", i = "missing", j = "character"),
          function(x, i, j, value)
      {
          j <- (x@units %in% j)

          if (!any(j))
              stop("subscript out of bounds")

          .assign_timeSeries(x, i = min(1, NROW(x)):NROW(x), j, value)

      })

# ------------------------------------------------------------------------------
# timeDate

setMethod("[<-",
          signature(x = "timeSeries", i = "timeDate", j = "character"),
          function(x, i, j, value)
      {

          # series settings
          if (x@format != "counts") {
              FinCenter <- finCenter(x)
              # convert FinCenter of index_timeSeries to FinCenter of timeSeries
              i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
          }

          i <- as(i, "character")

          i <- (x@positions %in% i)
          j <- (x@units %in% j)
          if (!any(i) || !any(j)) stop("subscript out of bounds")

          .assign_timeSeries(x, i, j, value)

      })

setMethod("[<-",
          signature(x = "timeSeries", i = "timeDate"),
          function(x, i, j, value)
      {

          # series settings
          if (x@format != "counts") {
              FinCenter <- finCenter(x)
              # convert FinCenter of index_timeSeries to FinCenter of timeSeries
              i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
          }

          i <- as(i, "character")

          i <- (x@positions %in% i)
          if (!any(i)) stop("subscript out of bounds")

          .assign_timeSeries(x, i, j, value)

      })

setMethod("[<-",
          signature(x = "timeSeries", i = "timeDate", j = "missing"),
          function(x, i, j, value)
      {

          # series settings
          if (x@format != "counts") {
              FinCenter <- finCenter(x)
              # convert FinCenter of index_timeSeries to FinCenter of timeSeries
              i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
          }

          i <- as(i, "character")

          i <- (x@positions %in% i)
          if (!any(i)) stop("subscript out of bounds")

          .assign_timeSeries(x, i,  j = min(1, NCOL(x)):NCOL(x), value)

      })

################################################################################

