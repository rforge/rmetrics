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
#  'signalSeries'            S4 Class representation
#  'timeSeries'              S4 Class representation
################################################################################

## setClass("signalSeries",
##          representation(
##                         .Data = "matrix",
##                         units = "character",
##                         recordIDs = "data.frame",
##                         title = "character",
##                         documentation = "character"),
##          contains = "structure",
##          validity = function(object) {
##              if (NCOL(getDataPart(object)) != length(object@units))
##                  return("length of '@units' not equal to '@.Data' extent")
##              TRUE
##          })

## # ------------------------------------------------------------------------------

## setClass("timeSeries",
##          representation(positions = "numeric",
##                         format = "character",
##                         FinCenter = "character"),
##          contains = "signalSeries",
##          validity = function(object) {
##              if (NROW(getDataPart(object)) != length(object@positions))
##                  return("length of '@positions' not equal to '@.Data' extent")
##              if (NCOL(getDataPart(object)) != length(object@units))
##                  return("length of '@units' not equal to '@.Data' extent")
##              TRUE
##          })

################################################################################


# Note if slots are added or removed, don't forget to edit
# getDataPart,timeSeries-method and setDataPart,timeSeries-method !!


setClass("timeSeries",
         representation(.Data = "matrix",
                        units = "character",
                        positions = "numeric",
                        format = "character",
                        FinCenter = "character",
                        recordIDs = "data.frame",
                        title = "character",
                        documentation = "character"),
         contains = "structure",
         validity = function(object) {
             if ((length(object@positions) > 0) &&
                 NROW(getDataPart(object)) != length(object@positions))
                 return("length of '@positions' not equal to '@.Data' extent")
             if (NCOL(getDataPart(object)) != length(object@units))
                 return("length of '@units' not equal to '@.Data' extent")
             TRUE
         })

         
# ------------------------------------------------------------------------------


# Note it is faster to assign manually all slots of the timeSeries objects.
setMethod("initialize", "timeSeries",
          function(.Object,
                   .Data = new("matrix"),
                   units = character(0),
                   positions = numeric(0),
                   format = character(0),
                   FinCenter = "",
                   #<< FIXME: use identical in code rather than FinCenter == ""
                   recordIDs = data.frame(),
                   title = character(0),
                   documentation = character(0))
      {
          .Object <- setDataPart(.Object, value = .Data)
          `slot<-`(.Object, "units", value = units)
          `slot<-`(.Object, "positions", value = positions)
          `slot<-`(.Object, "format", value = format)
          `slot<-`(.Object, "FinCenter", value = FinCenter)
          `slot<-`(.Object, "recordIDs", value = recordIDs)
          `slot<-`(.Object, "title", value = title)
          `slot<-`(.Object, "documentation", value = documentation)
          validObject(.Object)
          .Object
      })

      
################################################################################

