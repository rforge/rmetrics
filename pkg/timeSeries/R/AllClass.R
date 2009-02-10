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
         validity = function(object) {
             if (NROW(getDataPart(object)) != length(object@positions))
                 return("length of '@positions' not equal to '@.Data' extent")
             if (NCOL(getDataPart(object)) != length(object@units))
                 return("length of '@units' not equal to '@.Data' extent")
             TRUE
         })

# ------------------------------------------------------------------------------

## setMethod("initialize", "timeSeries", function(.Object, ...)
##       {
##           .Object <- callNextMethod()

##           # FIXME
##           # define format
##           # check if valid FinCenter
##           # note might be too time consuming !!!

##           # ISO Date/Time Format:
##           isoDate   <- "%Y-%m-%d"
##           isoFormat <- "%Y-%m-%d %H:%M:%S"

## ###           # extract numerical value
## ###           td <-
## ###           num <- c(unclass(timeDate(.Object@positions), )

##           if (all(is.na(num))) {
##               # no need to look for a format if @Data has only NA's
##               .Object@format <- character(1)
##           } else {

##               # convert - DST
##               num <- .formatFinCenterNum(num, .Object@FinCenter, "gmt2any")

##               # check if num is a multiple of days
##               test <- !(abs(num %% 86400) > 0)
##               .Object@format <- ifelse(all(na.omit(test)), isoDate, isoFormat)
##           }

##           .Object
##       })
