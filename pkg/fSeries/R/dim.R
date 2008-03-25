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

setMethod("dim", "timeSeries", function(x) callGeneric(as(x, "matrix")))

setMethod("dimnames", "timeSeries", function(x) callGeneric(as(x, "matrix")))

setReplaceMethod("dimnames", "timeSeries",
                 function(x, value)
             {
                 data <- callGeneric(as(x, "matrix"), value)
                 charvec <- rownames(data)
                 units <- colnames(data)
                 format <- NULL
                 zone <- finCenter(x)
                 FinCenter <- finCenter(x)
                 recordIDs <- x@recordIDs
                 title <- x@title
                 documentation <- x@documentation

                 new("timeSeries", data, charvec, units, format, zone,
                     FinCenter, recordIDs, title, documentation)
             })


# ------------------------------------------------------------------------------


# colnames # default methods works fine
# rownames # default methods works fine
# colnames<- # default methods works fine because it uses dimnames defined above
# rownmaes<- # default methods works fine because it uses dimnames defined above

# ------------------------------------------------------------------------------


is.array.timeSeries <-
    function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Allows that NCOL and NROW work properly

    # FUNCTION:

    # Is an array:
    ans = TRUE

    # Return Value:
    ans
}


################################################################################

