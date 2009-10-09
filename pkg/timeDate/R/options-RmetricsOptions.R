
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:                 DESCRIPTION:
#  myUnits                   Sets date units
################################################################################


.RmetricsOptions <- new.env(hash = TRUE)


# ------------------------------------------------------------------------------


setRmetricsOptions <-
    function(...)
{
    # A function implemented by Yohan Chalabi

    x <- list(...)
    nm <- names(x)
     if (is.null(nm) || "" %in% nm)
        stop("all arguments must be named")
    sapply(nm, function(nm) assign(nm, x[[nm]],
        envir = .RmetricsOptions))
    invisible()
}


# ------------------------------------------------------------------------------


getRmetricsOption <-
    function(x = NULL, unset = "")
{
    # A function implemented by Yohan Chalabi

    if (is.null(x))
        x <- ls(all.names = TRUE, envir = .RmetricsOptions)
    unlist(mget(x, envir = .RmetricsOptions, mode = "any",
        ifnotfound = as.list(unset)))
}

# YC : 2009-10-06
# kept for compatibility purpose but should be eventually deprecated
getRmetricsOptions <- getRmetricsOption

################################################################################

