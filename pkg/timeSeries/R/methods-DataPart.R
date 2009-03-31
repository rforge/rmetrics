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
# S4 METHODS:               PRINT AND PLOT FUNCTIONS:
#  getDataPart,timeSeries
#  setDataPart,timeSeries

################################################################################

# this makes getDataPart a bit faster than default function
setMethod("getDataPart", "timeSeries", #"signalSeries",
          function(object)
      {
          lattrs <- list(dim = dim(object),
                         dimnames = list(NULL, object@units))
          attributes(object) <- lattrs
          object
      })

# ------------------------------------------------------------------------------

# this makes setDataPart a bit faster than default function

if (getRversion() < "2.8.0") {
    setMethod("setDataPart", "timeSeries",
              function(object, value)
          {
              value <- as(value, "matrix")

              supplied <- attributes(object)
              valueAttrs <- attributes(value)

              supplied[names(valueAttrs)] <- valueAttrs
              attributes(value) <- supplied

              asS4(value, TRUE)
          })
} else {
    setMethod("setDataPart", "timeSeries",
              function(object, value, check = TRUE)
          {
              if (check) value <- as(value, "matrix")

              supplied <- attributes(object)
              valueAttrs <- attributes(value)

              supplied[names(valueAttrs)] <- valueAttrs
              attributes(value) <- supplied

              asS4(value, TRUE)
          })
}

################################################################################
