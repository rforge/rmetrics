
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
#  show.timeDate             Prints 'timeDate' object
################################################################################


setMethod("show", "timeDate",
    function (object)
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz
    
    # when creating empty new("timeDate")
    if (!length(slot(object, "Data")))
        return(str(object))

    output <- format(object)
    layout <- paste("[", output, "]", sep = "")

    # Print Results:
    cat(object@FinCenter, "\n", sep = "")
    print(layout, quote = FALSE)

    # Return Value:
    invisible(NULL) # 'show' returns an invisible 'NULL'. (cf. ?show)
})


################################################################################
