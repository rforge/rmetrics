
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


################################################################################
# FUNCTION:                 GENERAL EXTRACTORS FOR S4:
#  getCall                   Extracts the call slot from a S4 object
#  getModel                  Extracts the model slot from a S4 object
#  getTitle                  Extracts the title slot from a S4 object
#  getDescription            Extracts the description slot from a S4 object
#  getSlot                   Extracts a specified slot from a S4 object
#  getArgs                   Shows the arguments of a S4 functiong
################################################################################


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


# ------------------------------------------------------------------------------


getArgs <- 
function(f, signature = character())  
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Shows the arguments of a S4 functiong
    
    # Arguments:
    #   f - a generic function or the character-string name of one
    #   signature - the signature of classes to match to the arguments 
    #       of f 
    
    # Note:
    #   such a function is missing in the methods package,
    #   see, e.g he function getMethod()
    
    # FUNCTION:
    
    # Get arguments:
    fun = getMethod(f, signature)@.Data 
    test = class(try(body(fun)[[2]][[3]], silent = TRUE))
    if (test == "function") {
        ans = args(body(fun)[[2]][[3]])
    } else {
        ans = args(fun)
    } 
    cat(substitute(f), ",", signature, ":\n", sep = "")
    
    # Return Value:
    ans
} 


################################################################################


