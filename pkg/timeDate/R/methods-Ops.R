
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

# Copyrights (C)
# for this R-port:
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# MEHODS:                   DESCRIPTION:
#  Ops.timeDate              Group 'Ops' operations on 'timeDate' objects
#  +.timeDate                Performs + operation on 'timeDate' objects
#  -.timeDate                Performs - operation on 'timeDate' objects
################################################################################

setMethod("Ops", c("timeDate", "timeDate"),
        function(e1, e2)
    {
        # A function implemented by Diethelm Wuertz

        # Description:
        #   Uses group 'Ops' generic functions for 'timeDate' objects

        # Arguments:
        #   e1 - an object of class 'timeDate'
        #   e2 - an object of class 'timeDate'

        # Value:
        #   Returns the 'Ops' grouped object.

        # FUNCTION:
        ans <- callGeneric(e1@Data, e2@Data)

        if (inherits(ans, "POSIXt"))
            ans <- timeDate(as.character(ans),
                            zone = "GMT", FinCenter = e1@FinCenter)

        # Return Value:
        ans
    })

# ------------------------------------------------------------------------------

setMethod("+", c("timeDate", "numeric"),
          function(e1, e2)
      {

          ans <- callGeneric(e1@Data, e2)
          ans <- timeDate(ans, zone = "GMT", FinCenter = e1@FinCenter)

          # Return Value:
          ans
      })

# ------------------------------------------------------------------------------

setMethod("+", c("timeDate", "timeDate"),
          function(e1, e2)
          stop("binary '+' is not defined for \"timeDate\" objects"))

# ------------------------------------------------------------------------------

setMethod("-", c("timeDate", "numeric"),
          function(e1, e2)
      {

          ans <- callGeneric(e1@Data, e2)
          ans <- timeDate(ans, zone = "GMT", FinCenter = e1@FinCenter)

          # Return Value:
          ans
      })

# ------------------------------------------------------------------------------

setMethod("-", c("timeDate", "timeDate"),
          function(e1, e2) callGeneric(e1@Data, e2@Data))

################################################################################
