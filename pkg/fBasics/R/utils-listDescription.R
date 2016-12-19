
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


################################################################################
# FUNCTION:                 DESCRIPTION:
#  listDescription           Extracts R package description
################################################################################


listDescription <-
function(package, character.only = FALSE)
{
    # A function implemented by Diethelm Wuertz & Yohan Chalabi

    # Description:
    #   Extracts package description

## MM: I would
### .Deprecated("packageDescription")

    # FUNCTION:

    # Extract Description:
    if (!character.only && !missing(package))
        package <- as.character(substitute(package))
    if(missing(package) || length(package) == 0)
        stop("Must specify a package by name")
    ans <- library(help = package, character.only=TRUE)
    ## DW: gsub(): for proper insertion added
    description <- gsub("\n", "\n    ", ans$info[[1]])
    cat("\n", package, "Description:\n\n")
    cat(paste(" ", description), sep = "\n")
}


################################################################################

