
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
#  filter,timeSeries  Find the longest consecutive of non-missing values
################################################################################

setMethod("filter", "timeSeries",
          function(x, filter, method = c("convolution", "recursive"),
                   sides = 2, circular = FALSE, init = NULL)
      {
          ans <- filter(getDataPart(x), filter = filter, method = method,
                        sides = sides, circular = circular, init = init)
          #-> Note : do not use as.matrix because ts objects might
          #-> not be coerced properly
          ans <- as(ans, "matrix")
          colnames(ans) <- colnames(x)
          setDataPart(x, ans)
      })

################################################################################
