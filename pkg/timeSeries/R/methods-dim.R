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
# S4 METHOD:                   DIM OPERATIONS ON DATA:
#  dim,timeSeries            Returns dimension of a 'timeSeries' object
#  dimnames,timeDSeries      Returns dimension names of a 'timeSeries' object
#  dimnames<-,timeSeries     Assign dimension names of a 'timeSeries' object
#  colnames,timeSeries       Return column names to a 'timeSeries' object
#  rownames,timeSeries       Return row names to a 'timeSeries' object
#  colnames<-,timeSeries     Assigns column names to a 'timeSeries' object
#  rownames<-,timeSeries     Assigns row names to a 'timeSeries' object
#  is.array,timeSeries       Allows that NCOL and NROW work properly
################################################################################


# Base Functions:

    # Generate from Matrix:
    # edhec.tS = timeSeries(edhec.mat, rownames(edhec.mat))
    # edhec.ts = ts(edhec.mat, start = c(1997, 1), frequency = 12)

    # Univariate time Series:
    # edhec1.tS = edhec.tS[, 1]

    #   dim
    #                       dim(edhec.tS)                       # 20 4
    #                       dim(edhec1.tS)                      # 20 1


    #   DIM
    #                       DIM = function(x) {c(NROW(x), NCOL(x))}
    #                       DIM(edhec.tS)                       # 20 4
    #                       DIM(edhec1.tS)                      # 20 1


    #   length
    #                       length(edhec.tS)                    # 1

    #
    #   LENGTH
    #                       LENGTH = function(x) NROW(x)
    #                       LENGTH(edhec.tS)                    # 20
    #                       LENGTH(edhec1.tS)                   # 20


    #
    #   ncol / nrow
    #                       ncol(edhec.tS)                      # 4

    #
    #                       ncol(edhec1.tS)                     # 1

    #
    #  NCOL / NRWO
    #                       NCOL(edhec.tS)                      # 4

    #
    #                       NCOL(edhec1.tS)                     # 1

    #
    #  isUnivariate
    #                       isUnivariate = function(x) NCOL(x) == 1
    #                       isUnivariate(edhec.tS)
    #                       isUnivariate(edhec1.tS)


    #
    # isMultivariate        # Just Negation of isUnivariate
    #
    #
    #

# ------------------------------------------------------------------------------


# length
# dim
# ncol
# nrow


# LENGTH
# DIM
# NCOL
# NROW

# ------------------------------------------------------------------------------

# note it is faster to access attribute rather than accessing @.Data
setMethod("dim", "timeSeries", function(x) attr(x, "dim"))

# ------------------------------------------------------------------------------

setMethod("dimnames", "timeSeries", function(x) list(x@positions, x@units))

# ------------------------------------------------------------------------------

setMethod("colnames<-", "timeSeries",
          function(x, value)
      {
          units <- as.character(value)

          if(!length(units))
              if (x@format == "counts")
                  units <- paste("SS", seq(NCOL(x)), sep = ".")
              else
                  units <- paste("TS", seq(NCOL(x)), sep = ".")

          if (length(units) != NCOL(x))
              stop("length of 'colnames' not equal to array extent",
                   call. = FALSE)
          x@units <- units

          x
      })

# ------------------------------------------------------------------------------

setMethod("rownames<-", c("timeSeries", "timeDate"),
          function (x, value)
      {
          positions <- value
          if (length(positions) != NROW(x))
              stop("length of 'rownames' not equal to array extent",
                   call. = FALSE)
          x@FinCenter <- finCenter(positions)
          x@positions <- as.character(positions)
          x@format <- positions@format
          x
      })

# ------------------------------------------------------------------------------

setMethod("rownames<-", "timeSeries",
          function (x, value)
      {

          positions <- as.character(value)

          if(!length(positions))
              positions <- .signalCounts(seq(NROW(x)))

          if (length(positions) != NROW(x))
              stop("length of 'rownames' not equal to array extent",
                   call. = FALSE)

          if (identical(positions, .signalCounts(seq(NROW(x))))) {
              x@positions <- as.character(positions)
              x@format <- "counts"
              x@FinCenter <- ""
          }
          else {
              format <- whichFormat(positions, silent = TRUE)
              if (format %in% c("unknown", "%Y")) {
                  x@positions <- .signalCounts(seq(NROW(x)))
                  x@format <- "counts"
                  x@FinCenter <- ""
              }
              else {
                  positions <- timeDate(positions, format = format)
                  x@format <- positions@format
                  x@positions <- as.character(positions)
              }
          }

          x
      })

# ------------------------------------------------------------------------------

setMethod("dimnames<-", c("timeSeries", "list"),
          function(x, value)
      {
          rownames(x) <- value[[1]]
          colnames(x) <- value[[2]]
          x
      })

# ------------------------------------------------------------------------------

# colnames # default methods works fine
# rownames # default methods works fine
# colnames<- # default methods works fine because it uses dimnames defined above
# rownmaes<- # default methods works fine because it uses dimnames defined above

################################################################################

