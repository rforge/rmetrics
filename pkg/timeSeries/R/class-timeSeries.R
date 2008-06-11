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
# CLASS:                    REPRESENTATION:
#  'timeSeries'              S4 Class representation
################################################################################


setClass("timeSeries",
         # A class implemented by Diethelm Wuertz and Yohan Chalabi

         # Description:
         #   Class representatation for 'timeSeries' Objects.

         # CLASS:

         representation(
                        .Data = "matrix",
                        positions = "character",
                        format = "character",
                        FinCenter = "character",
                        units = "character",
                        recordIDs = "data.frame",
                        title = "character",
                        documentation = "character"),
         contains = "structure",
         )


## setMethod("slot<-", signature(object = "timeSeries"),
##           function(object, name, check = TRUE, value) {

##           if (name == "Data") {
##               name <- ".Data"
##               warning("slot '@Data' in class 'timeSeries' is deprecated")
##           }

##           if (check)
##                value <- checkSlotAssignment(object, name, value)
##           .Call("R_set_slot", object, name, value, PACKAGE = "methods")

##       })

## setMethod("slot", signature(object = "timeSeries"),
##           function(object, name) {

##           if (name == "Data") {
##               name <- ".Data"
##               warning(gettextf("slot '@Data' in class 'timeSeries' is deprecated.\nUse '%s@.Data' (or just '%s')" , as.character(call[[2]]), as.character(call[[2]]), domain = NULL), call. = FALSE)
##           }

##           .Call("R_get_slot", object, name, PACKAGE = "methods")

##       })


## "%@%" <- base::.Primitive("@")
## "@" <-
##     function(...)
## {

##     call <- match.call(expand.dots = TRUE)
##     call$drop.unused.levels <- TRUE

##     call[[1]] <- as.name("%@%")

##     if (is(eval(call[[2]], parent.frame()), "timeSeries") &&
##         call[[3]] == "Data") {
##         call[[3]] <- as.name(".Data")
##         warning(gettextf("slot '@Data' in class 'timeSeries' is deprecated.\nUse '%s@.Data' (or just '%s')" , as.character(call[[2]]), as.character(call[[2]]), domain = NULL), call. = FALSE)
##     }

##     eval(call, parent.frame())
## }

## setMethod("@<-", signature(object = "timeSeries"),
##           function(object, name, value) {

##               arg <- substitute(name)
##               if (is.name(arg))
##                   name <- as.character(arg)
##               `slot<-`(object, name, TRUE, value)

##           })
