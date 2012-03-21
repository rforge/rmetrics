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
#  getArgs                   Gets arguments from a S4 function and signature
# DEPRECATED:
#  .getArgs
# SEE ALSO:                 DESCRIPTION:
#  methods::getMethod        Gets or Tests for the Definition of a Method
################################################################################

getArgs <- function(f, signature = character())
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Gets arguments from a S4 function and signature

    # Examples:
    #   args(returns)
    #   getArgs(returns)
    #   getArgs("returns")
    #   getArgs(returns, "timeSeries")
    #   getArgs("returns", "timeSeries")

    # FUNCTION:

    # Get Arguments:
    fun <- getMethod(f, signature)@.Data
    test <- class(try(body(fun)[[2]][[3]], silent = TRUE))
    if (test == "function") {
        ans <- args(body(fun)[[2]][[3]])
    } else {
        ans <- args(fun)
    }
    cat(substitute(f), ",", paste(signature, collapse = ","),
        ":\n", sep = "")

    # Return Value:
    ans
}

################################################################################

.getArgs =
function(...)
{
    # Deprecated:
    .Deprecated(new = "getArgs", package = "timeSeries")

}

################################################################################
