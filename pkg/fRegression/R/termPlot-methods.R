
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA.

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTIONS:                REGRESSION TERMS:
#  termPlot.fREG             Displays 'fREG' Model Term Plots
################################################################################

termPlot.fREG <-
## setMethod(f = "termPlot", signature(model = "fREG"), definition =
    function(model, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays 'fREG' Model Term Plots

    # Arguments:
    #   model - an object of class fREG as returned by the function
    #       regFit

    # FUNCTION:

    # Formula:
    ans = termplot(slot(model, "fit"), ...)

    # Return Value:
    ans
}
#)



################################################################################

