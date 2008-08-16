
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
# You should have received A copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION:
#  listIndex                 Extracts R package index
################################################################################


listIndex <-
    function(package = "Rmetrics", character.only = FALSE)
{
    # A function implemented by Diethelm Wuertz & Yohan Chalabi

    # Description:
    #   Extracts R package index

    # Example:
    #   listIndex("fSeries")

    # FUNCTION:

    # Extract Index:
    if (!character.only)
        package <- as.character(substitute(package))
    cmd = paste("library(help =", package, ")", sep = "" )
    ans = eval(parse(text = cmd))
    name = ans$name
    parh = ans$path
    description = ans$info[[1]]
    index = ans$info[[2]]
    cat("\n", package, "Index:\n\n")
    cat(paste(" ", index), sep = "\n")
}


################################################################################

