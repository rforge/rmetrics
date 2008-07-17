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
# FUNCTION:                 DESCRIPTION:
#  comment, timeSeries
#  comment<-,timeSeries
################################################################################

# A function implemented by Yohan Chalabi and Diethelm Wuertz

setMethod("comment", "timeSeries", function(x) x@documentation)

setMethod("comment<-", "timeSeries",
          function(x, value)
          {
              x@documentation <- paste(value, collapse = " ")

              # Results
              x
          })

################################################################################

