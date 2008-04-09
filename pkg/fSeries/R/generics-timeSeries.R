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
           function(x, ...) standardGeneric("returns"), package = "fSeries")

setGeneric("rowCumsums", function(x, na.rm = FALSE, ...)
           standardGeneric("rowCumsums"), package = "fSeries")

setGeneric("series", function(x) standardGeneric("series"), package = "fSeries")
setGeneric("series<-",
           function(x, value) standardGeneric("series<-"), package = "fSeries")

## setGeneric("index", function(x) standardGeneric("index"), package = "fSeries")
## setGeneric("index<-",
##            function(x, value) standardGeneric("index<-"), package = "fSeries")
