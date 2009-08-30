
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
# FUNCTION:                 DESCRIPTION:
#  isMonthly                 Tests if a date/time vector has monthly time stamps
#  isMonthly.timeDate        Tests if a timeDate object has monthly time stamps
################################################################################

setMethod("isMonthly", "timeDate", function(x)
      {
          # A function implemented by Diethelm Wuertz

          # Descriptions:
          #   Tests if a timeDate object has monthly time stamps

          # Arguments:
          #   x - an object of class timeDate

          # Details:
          #   Definition: A timeDate Object is a Monthly timeDate object
          #   if we have not more than one date/time stamp per month.
          #   Note a monthly series is also a daily series.

          # Example:
          #   isMonthly(timeSequence(by = "day", length.out = 20))
          #   isMonthly(timeCalendar())
          #   isDaily(timeCalendar())
          #   isMonthly(timeSequence(by = "hour", length.out = 100))

          # FUNCTION:

          # Monthly ?
          m <- c(timeDate::months(x)) #-> c() to remove attributes
          # (m[1] -1) -> shift vector to match first entry in m
          monthly <- seq(from = m[1]-1, length.out=length(m)) %% 12 + 1

          # Return
          (identical(monthly, m))
      })

################################################################################

