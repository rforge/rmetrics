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

setGeneric("returns",
           function(x, ...) standardGeneric("returns"), package = "timeSeries")

setGeneric("rowCumsums", function(x, na.rm = FALSE, ...)
           standardGeneric("rowCumsums"), package = "timeSeries")

setGeneric("series", function(x) standardGeneric("series"), package = "timeSeries")
setGeneric("series<-",
           function(x, value) standardGeneric("series<-"), package = "timeSeries")

## setGeneric("index", function(x, ...) standardGeneric("index"), package = "timeSeries")
## setGeneric("index<-",
##            function(x, value) standardGeneric("index<-"), package = "timeSeries")

setGeneric("outlier",  function(x, sd = 5, complement = TRUE, ...) standardGeneric("outlier"))

setGeneric("timeSeries",
           function (data, charvec, units = NULL, format = NULL, zone = "",
                     FinCenter = "", recordIDs = data.frame(), title = NULL,
                     documentation = NULL, ...)
           standardGeneric("timeSeries"))
