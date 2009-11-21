
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA

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
# FUNCTION:                 GENERAL EXTRACTORS FOR S4:
#  isS4                      Checks if a function is a S4 object
#  getCall                   Extracts the call slot from a S4 object
#  getModel                  Extracts the model slot from a S4 object
#  getTitle                  Extracts the title slot from a S4 object
#  getDescription            Extracts the description slot from a S4 object
#  getSlot                   Extracts a specified slot from a S4 object
################################################################################


## isS4 <-
## function(object)
## {
##     # A function implemented by Rmetrics

##     # Description:
##     #   Tests if an object is an object of class S4

##     # Arguments:
##     #   object - an object of class S4

##     # FUNCTION:

##     if (is.null(slotNames(object))) {
##         ans = FALSE
##     } else {
##         ans = TRUE
##     }

##     # Return Value:
##     ans
## }


# ------------------------------------------------------------------------------


getCall <-
function(object)
{
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the "call" slot from an object of class 4

    # Arguments:
    #   object - an object of class S4

    # FUNCTION:

    # Return Value:
    getSlot(object, "call")
}


# ------------------------------------------------------------------------------


getModel <-
function(object)
{
    # A function implemented by Rmetrics

    # Description:
    #   gets the "model" slot from an object of class 4

    # Arguments:
    #   object - an object of class S4

    # FUNCTION:

    # Return Value:
    getSlot(object, "model")
}


# ------------------------------------------------------------------------------


getTitle <-
function(object)
{
    # A function implemented by Rmetrics

    # Description:
    #   gets the "title" slot from an object of class 4

    # Arguments:
    #   object - an object of class S4

    # FUNCTION:

    # Return Value:
    getSlot(object, "title")
}


# ------------------------------------------------------------------------------


getDescription <-
function(object)
{
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the "description" slot from an object of class 4

    # Arguments:
    #   object - an object of class S4

    # FUNCTION:

    # Return Value:
    getSlot(object, "description")
}


# ------------------------------------------------------------------------------


getSlot <-
function(object, slotName)
{
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the a specified slot from an object of class 4

    # Arguments:
    #   object - an object of class S4
    #   slotName - the name of the slot ot be extracted from an S4 object

    # FUNCTION:

    # isS4
    stopifnot(isS4(object))

    # exists @call ?
    stopifnot (any(slotNames(object) == slotName))

    # Command to execute:
    command = paste("object@", slotName, sep = "")

    # Return Value:
    eval(parse(text = command))
}


################################################################################

