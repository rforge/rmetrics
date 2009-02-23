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
#  'signalSeries'              S4 Class representation
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


setClass("timeSeries",
         representation(
                        .Data = "matrix",
                        units = "character",
                        positions = "numeric",
                        format = "character",
                        FinCenter = "character",
                        recordIDs = "data.frame",
                        title = "character",
                        documentation = "character"),
         contains = "structure",
         validity = function(object) {
             if (length(object@positions > 0) &&
                 NROW(getDataPart(object)) != length(object@positions))
                 return("length of '@positions' not equal to '@.Data' extent")
             if (NCOL(getDataPart(object)) != length(object@units))
                 return("length of '@units' not equal to '@.Data' extent")
             TRUE
         })
