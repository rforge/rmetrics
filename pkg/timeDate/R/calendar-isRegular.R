
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:                  DESCRIPTION:
#  isRegular,timeDate-method  Tests if a timeDate object has regular time stamps
################################################################################



setMethod("isRegular", "timeDate",  function(x)
      {
          # A function implemented by Diethelm Wuertz

          # Descriptions:
          #   Tests if a timeDate object has regular time stamps

          # Example:
          #   isRegular(timeSequence(by = "day", length.out = 20))
          #   isRegular(timeCalendar())
          #   isRegular(timeSequence(by = "hour", length.out = 100))

          # Details:
          #   Definition: A timeDate Object is a Regular timeDate object
          #   if the timeDate object is either monthly or quarterly,
          #   otherwise not.

          # Arguments:
          #   x - an object of class timeDate

          # FUNCTION:

          # Regular ?
          (isMonthly(x) | isQuarterly(x))
      })


################################################################################

