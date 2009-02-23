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
#  getDataPart,timeSeries    Summarizes a 'timeSeries' object
################################################################################

# this makes getDataPart a bit faster than default function
setMethod("getDataPart", "timeSeries", #"signalSeries",
          function(object)
      {
          dim <- dim(object)
          attributes(object) <- NULL
          dim(object) <- dim
          object
      })

################################################################################
