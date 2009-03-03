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
              members =  c("numeric", "logical")) # , "character"))

################################################################################
#  [,timeSeries              Subsets of a 'timeSeries' object
################################################################################

## x <- "timeSeries"
## i <- c("index_timeSeries", "character", "timeDate", "timeSeries",
##        "matrix", "missing", "ANY")
## j <- c("index_timeSeries", "character", "missing", "ANY")
## expand.grid(x = x, i = i, j = j)

##             x                i                j
## 1  timeSeries index_timeSeries index_timeSeries
## 2  timeSeries        character index_timeSeries
## 3  timeSeries         timeDate index_timeSeries
## 4  timeSeries       timeSeries index_timeSeries
## 5  timeSeries           matrix index_timeSeries
## 6  timeSeries          missing index_timeSeries
## 7  timeSeries              ANY index_timeSeries
## 8  timeSeries index_timeSeries        character
## 9  timeSeries        character        character
## 10 timeSeries         timeDate        character
## 11 timeSeries       timeSeries        character
## 12 timeSeries           matrix        character
## 13 timeSeries          missing        character
## 14 timeSeries              ANY        character
## 15 timeSeries index_timeSeries          missing
## 16 timeSeries        character          missing
## 17 timeSeries         timeDate          missing
## 18 timeSeries       timeSeries          missing
## 19 timeSeries           matrix          missing
## 20 timeSeries          missing          missing
## 21 timeSeries              ANY          missing
## 22 timeSeries index_timeSeries              ANY
## 23 timeSeries        character              ANY
## 24 timeSeries         timeDate              ANY
## 25 timeSeries       timeSeries              ANY
## 26 timeSeries           matrix              ANY
## 27 timeSeries          missing              ANY
## 28 timeSeries              ANY              ANY

# ------------------------------------------------------------------------------

.subset_timeSeries <-
    function(x, i, j)
{

    stopifnot(inherits(x, "timeSeries"))
    stopifnot(is(i, "index_timeSeries"))
    stopifnot(is(j, "index_timeSeries"))

    # subset data and positions
    data <- .subset(x, i, j, drop = FALSE)
    pos <-
        if (length(x@positions)>0)
            .subset(x@positions, i)
        else
            numeric(0)
    units <- .subset(x@units, j)

    # Record IDs:
    df <- x@recordIDs
    if (prod(dim(df)))
        df <- df[i, , drop = FALSE]

    # Result
    new("timeSeries",
        .Data = data,
        title = x@title,
        documentation = x@documentation,
        format = x@format,
        FinCenter = x@FinCenter,
        units = units,
        recordIDs = df,
        positions = pos)

}

# ------------------------------------------------------------------------------

.findIndex <- function(ipos, pos)
{

    # FIXME : if NAs ...

    attributes(ipos) <- NULL

    if (unsorted <- is.unsorted(pos)) {
        or <- order(pos)
        pos <- pos[or]
    }

    i <- findInterval(ipos, pos)

    if (!identical(as.numeric(ipos), as.numeric(pos[i]))) #-> avoid troubles with int
        stop("subscript out of bounds", call. = FALSE)

    if (unsorted) i <- or[i]
    i
}

# ------------------------------------------------------------------------------

## FIXME : deal with signal series

# ------------------------------------------------------------------------------
## 1  timeSeries index_timeSeries index_timeSeries

setMethod("[",
          signature(x = "timeSeries", i = "index_timeSeries",
                    j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
              .subset_timeSeries(x, i, j))

# ------------------------------------------------------------------------------
## 2  timeSeries        character index_timeSeries

setMethod("[",
          signature(x = "timeSeries", i = "character", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {

          # FIXME

          # check if :: separator

          td <- timeDate(i)
          if (any(is.na(td))) return(as.vector(NA))

          i <- .findIndex(as.numeric(td, units = "secs"), x@positions)

          # Return
          .subset_timeSeries(x, i, j)

      })

# ------------------------------------------------------------------------------
## 3  timeSeries         timeDate index_timeSeries

setMethod("[",
          signature(x = "timeSeries", i = "timeDate", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {
          i <- .findIndex(as.numeric(i, units = "secs"), x@positions)
          .subset_timeSeries(x, i, j)
      })

# ------------------------------------------------------------------------------
## 4  timeSeries       timeSeries index_timeSeries

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

# ------------------------------------------------------------------------------
## 5  timeSeries           matrix index_timeSeries

setMethod("[",
          signature(x = "timeSeries", i = "matrix", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
          .subset_timeSeries(x, as.vector(i), j))

# ------------------------------------------------------------------------------
## 6  timeSeries          missing index_timeSeries

setMethod("[",
          signature(x = "timeSeries", i = "missing",
                         j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
          .subset_timeSeries(x, TRUE, j))

# ------------------------------------------------------------------------------
## 7  timeSeries              ANY index_timeSeries

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------
## 8  timeSeries index_timeSeries        character

setMethod("[",
          signature(x = "timeSeries", i = "index_timeSeries", j = "character"),
          function(x, i, j, ..., drop = FALSE)
      {
          j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
          if (any(is.na(j)))
              stop("subscript out of bounds", call. = FALSE)
          .subset_timeSeries(x, i, j)
      })

# ------------------------------------------------------------------------------
## 9  timeSeries        character        character

setMethod("[",
          signature(x = "timeSeries", i = "character", j = "character"),
          function(x, i, j, ..., drop = FALSE)
      {
          j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
          if (any(is.na(j)))
              stop("subscript out of bounds", call. = FALSE)
          callGeneric(x=x, i=i, j=j, drop=drop)
      })

# ------------------------------------------------------------------------------
## 10 timeSeries         timeDate        character

setMethod("[",
          signature(x = "timeSeries", i = "timeDate", j = "character"),
          function(x, i, j, ..., drop = FALSE)
      {
          i <- .findIndex(as.numeric(i, units = "secs"), x@positions)
          j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
          if (any(is.na(j)))
              stop("subscript out of bounds", call. = FALSE)
          .subset_timeSeries(x, i, j)
      })

# ------------------------------------------------------------------------------
## 11 timeSeries       timeSeries        character

## FIXME

# ------------------------------------------------------------------------------
## 12 timeSeries           matrix        character

## FIXME

# ------------------------------------------------------------------------------
## 13 timeSeries          missing        character

setMethod("[",
          signature(x = "timeSeries", i = "missing", j = "character"),
          function(x, i, j, ..., drop = FALSE)
      {
          j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
          if (any(is.na(j)))
              stop("subscript out of bounds", call. = FALSE)
          .subset_timeSeries(x, TRUE, j)
      })

# ------------------------------------------------------------------------------
## 14 timeSeries              ANY        character

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------
## 15 timeSeries index_timeSeries          missing

setMethod("[",
          signature(x = "timeSeries", i = "index_timeSeries",
                         j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if(nargs() == 2) { # same sub-setting as matrix
              if (is.character(i))
                  as.numeric(NA)
              else if(any(as.logical(i)) || prod(dim(x)) == 0)
                  as.vector(x)[i]
          } else {
              .subset_timeSeries(x, i, TRUE)
          }
      })

# ------------------------------------------------------------------------------
## 16 timeSeries        character          missing

# FIXME
setMethod("[",
          signature(x = "timeSeries", i = "character", j = "missing"),
          function(x, i, j, ..., drop = FALSE) x[i,TRUE])

# ------------------------------------------------------------------------------
## 17 timeSeries         timeDate          missing

setMethod("[",
          signature(x = "timeSeries", i = "timeDate", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          i <- .findIndex(as.numeric(i, units = "secs"), x@positions)
          .subset_timeSeries(x, i, TRUE)
      })

# ------------------------------------------------------------------------------
## 18 timeSeries       timeSeries          missing

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
## 19 timeSeries           matrix          missing

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
## 20 timeSeries          missing          missing

setMethod("[",
          signature(x = "timeSeries", i = "missing", j = "missing"),
          function(x, i, j, ..., drop = FALSE) x)

# ------------------------------------------------------------------------------
## 21 timeSeries              ANY          missing

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------
## 22 timeSeries index_timeSeries              ANY

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------
## 23 timeSeries        character              ANY

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------
## 24 timeSeries         timeDate              ANY

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------
## 25 timeSeries       timeSeries              ANY

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------
## 26 timeSeries           matrix              ANY

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------
## 27 timeSeries          missing              ANY

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

# ------------------------------------------------------------------------------
## 28 timeSeries              ANY              ANY

setMethod("[",
          signature(x = "timeSeries", i = "ANY", j = "index_timeSeries"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))


################################################################################
#  $,timeSeries            Subset by column names
################################################################################

# should behave the same way as $,data.frame
# if none or more than one match returns NULL

setMethod("$",
          signature(x = "timeSeries", name = "character"),
          function (x, name)
      {
          idx <- grep(name, x@units)
          if (length(idx) == 1) # if none or more than one match -> NULL
              .subset(x, TRUE, idx)
          else
              NULL
      })

setMethod("$",
          signature(x = "timeSeries", name = "ANY"),
          function(x, name)
          NULL )

################################################################################
#  [<-,timeSeries            Assign value to subsets of a 'timeSeries' object
################################################################################

## i <- c("timeDate", "character", "index_timeSeries", "timeSeries", "matrix")
## j <- c("index_timeSeries", "character", "missing")

# ------------------------------------------------------------------------------
# timeDate

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "timeDate",
                           j = "index_timeSeries"),
                 function(x, i, j, value)
             {
                 i <- .findIndex(as.numeric(i, units = "secs"), x@positions)
                 callGeneric(x=x, i=i, j=j, value=value)
             })

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "timeDate", j = "character"),
                 function(x, i, j, value)
             {
                 j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
                 if (any(is.na(j)))
                     stop("subscript out of bounds", call. = FALSE)
                 callGeneric(x=x, i=i, j=j, value=value)
             })

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "timeDate", j = "missing"),
                 function(x, i, j, value)
                 callGeneric(x=x, i=i, j=TRUE, value=value))

# ------------------------------------------------------------------------------
# character

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "character", j = "index_timeSeries"),
                 function(x, i, j, value)
             {
                 i <- timeDate(i)
                 callGeneric(x=x, i=i, j=j, value=value)
             })

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "character", j = "character"),
                 function(x, i, j, value)
             {
                 i <- timeDate(i)
                 j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
                 if (any(is.na(j)))
                     stop("subscript out of bounds", call. = FALSE)
                 callGeneric(x=x, i=i, j=j, value=value)
             })

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "character", j = "missing"),
                 function(x, i, j, value)
             {
                 i <- timeDate(i)
                 callGeneric(x=x, i=i, j=TRUE, value=value)
             })

# ------------------------------------------------------------------------------
# index_timeSeries

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "index_timeSeries", j = "character"),
                 function(x, i, j, value)
             {
                 j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
                 if (any(is.na(j)))
                     stop("subscript out of bounds", call. = FALSE)
                 callGeneric(x=x, i=i, j=j, value=value)
             })

# Note
# index_timeSeries,index_timeSeries
# index_timeSeries,missing
# works by default

# ------------------------------------------------------------------------------
# matrix

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "matrix", j = "character"),
                 function(x, i, j, value)
             {
                 j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
                 if (any(is.na(j)))
                     stop("subscript out of bounds", call. = FALSE)
                 callGeneric(x=x, i=i, j=j, value=value)
             })

# Note
# matrix,index_timeSeries
# matrix,missing
# works by default

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
                 callGeneric(x=x, i=as.vector(i), j=j, value=value)
             })

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "timeSeries", j = "character"),
                 function(x, i, j, value)
             {
                 j <- pmatch(j, slot(x, "units"), duplicates.ok = TRUE)
                 if (any(is.na(j)))
                     stop("subscript out of bounds", call. = FALSE)
                 callGeneric(x=x, i=i, j=j, value=value)
             })

setReplaceMethod("[",
                 signature(x = "timeSeries", i = "timeSeries", j = "missing"),
                 function(x, i, j, value)
                 callGeneric(x=x, i=i, j=TRUE, value=value))

################################################################################
