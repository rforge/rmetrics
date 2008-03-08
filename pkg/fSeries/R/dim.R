
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
# METHOD:                   DIM OPERATIONS ON DATA:
#  dim.timeSeries            Returns dimension of a 'timeSeries' object
#  dimnames<-.timeSeries     NYI
#  dimnames.timeDSeries      Returns dimension names of a 'timeSeries' object
#  colnames.timeSeries       NYI
#  rownames.timeSeries       NYI
#  colnames<-.timeSeries     Assigns column names to a 'timeSeries' object
#  rownames<-.timeSeries     Assigns row names to a 'timeSeries' object
#  is.array.timeSeries       Allows that NCOL and NROW work properly
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


dim.timeSeries =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the dimension of a 'timeSeries' object

    # FUNCTION:

    # Dimension:
    ans = dim(x@Data)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


dimnames.timeSeries =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the dimension names of a 'timeSeries' object

    # Note:
    #   dimnames() is .Primitive
    # FUNCTION:

    # Dimension Names:
    ans = dimnames(x@Data)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


# "dimnames<-"


# ------------------------------------------------------------------------------


# colnames
# rownames


# ------------------------------------------------------------------------------


"colnames<-.timeSeries" <-
    function(x, value)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Assigns column names to a 'timeSeries' object

    # FUNCTION:

    # Assign Column Names:
    X <- x@Data
    dn <- dimnames(X)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd <- length(dim(X))) < 2) stop(
            "attempt to set colnames on object with less than two dimensions")
        dn <- vector("list", nd)
    }
    if(length(dn) < 2) stop(
        "attempt to set colnames on object with less than two dimensions")
    dn[2] <- if (is.null(value)) list(NULL) else list(value)
    dimnames(X) <- dn

    # DW addded for timeSeries objects
    x@Data <- X
    x@units <- as.character(value) # YC as.character needed if value == NULL
    x
}


# ------------------------------------------------------------------------------


"rownames<-.timeSeries" =
function(x, value)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Assigns row names to a 'timeSeries' object

    # FUNCTION:

    # Assign Row Names:
    X = x@Data
    dn <- dimnames(X)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd <- length(dim(X))) < 2) stop(
            "attempt to set colnames on object with less than two dimensions")
        dn <- vector("list", nd)
    }
    if(length(dn) < 2) stop(
        "attempt to set colnames on object with less than two dimensions")
    if(is.null(value)) dn[1] <- list(NULL) else dn[[1]] <- value
    dimnames(X) <- dn

    # DW addded for timeSeries objects
    x@Data = X
    x@positions = rownames(X)

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


is.array.timeSeries =
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

