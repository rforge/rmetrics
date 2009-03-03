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
             if (length(object@positions > 0) &&
                 NROW(getDataPart(object)) != length(object@positions))
                 return("length of '@positions' not equal to '@.Data' extent")
             if (NCOL(getDataPart(object)) != length(object@units))
                 return("length of '@units' not equal to '@.Data' extent")
             TRUE
         })

# ------------------------------------------------------------------------------

setMethod("initialize", "timeSeries",
          function(.Object,
                   .Data = new("matrix"),
                   units = character(0),
                   positions = numeric(0),
                   format = character(0),
                   FinCenter = character(0),
                   recordIDs = data.frame(),
                   title = character(0),
                   documentation = character(0),
                   check = TRUE)
      {
          .Object <- setDataPart(.Object, value = .Data, check = check)
          `slot<-`(.Object, "units", check = check, value = units)
          `slot<-`(.Object, "positions", check = check, value = positions)
          `slot<-`(.Object, "format", check = check, value = format)
          `slot<-`(.Object, "FinCenter", check = check, value = FinCenter)
          `slot<-`(.Object, "recordIDs", check = check, value = recordIDs)
          `slot<-`(.Object, "title", check = check, value = title)
          `slot<-`(.Object, "documentation", check = check, value = documentation)
          .Object
      })

################################################################################

## # before init methods
## system.time(new("timeSeries", .Data = as.matrix(1:1e6L), units = "hello", positions = 1:1e6L))
## ###    user  system elapsed
## ###   0.230   0.180   0.405

## system.time(timeSeries(1:1e6L, units = "hello", positions = 1:1e6L))
###    user  system elapsed
###    0.40    0.10    0.49

## # after
## system.time(new("timeSeries", .Data = as.matrix(1:1e6L), units = "hello", positions = 1:1e6L))
## ###    user  system elapsed
## ###    0.15    0.12    0.27
###    user  system elapsed
###    0.06    0.09    0.15

## system.time(timeSeries(1:1e6L, units = "hello", positions = 1:1e6L))
###    user  system elapsed
###   0.200   0.050   0.273

## system.time(timeSeries(as.matrix(1:1e6L), units = "hello", positions = 1:1e6L))
## ###   user  system elapsed
## ###   0.150   0.120   0.267

## # twice faster !!
## # and without check
## system.time(new("timeSeries", .Data = as.matrix(1:1e6L), units = "hello", positions = 1:1e6L, check = FALSE))
## # does not seem faster ....

