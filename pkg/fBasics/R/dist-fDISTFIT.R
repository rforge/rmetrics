
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:            DESCRIPTION:
#  'fDISTFIT'           S4 Class representation
#  show.fDISTFIT        Prints Results from a Fitted Distribution
################################################################################


setClass("fDISTFIT",
    representation(
        call = "call",
        model = "character",
        data = "data.frame",
        fit = "list",
        title = "character",
        description = "character"
    )
)


# ------------------------------------------------------------------------------

setMethod("show", "fDISTFIT",
      function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Prints Results from a Fitted Distribution

    # FUNCTION:

    # Title:
    cat("\nTitle:\n ")
    cat(object@title, "\n")

    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"),
        "\n", sep = "")

    # Model:
    cat("\nModel:\n ", object@model, "\n", sep = "")

    # Estimate:
    cat("\nEstimated Parameter(s):\n")
    print(object@fit$estimate)

    # Description:
    cat("\nDescription:\n ")
    cat(object@description, "\n\n")

    # Return Value:
    invisible()
})


# ------------------------------------------------------------------------------





################################################################################

