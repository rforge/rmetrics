
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:
#  xjulian               Compute Julian minute numbers from ISO-8601 dates
#  xdate                 Compute ISO-8601 date/times from Julian minute numbers
#  xday.of.week          Compute day of the week for ISO-8601 dates
#  xleap.year            Return T/F if date/times belong to leap years or not
# FUNCTION:             DESCRIPTION:
#  fxdata.contributors   Creates a table with contributor names    
#  fxdata.parser         Parses FX contributors and delay times
#  fxdata.filter         Filters price and spread values from FX data records
#  fxdata.varmin         Aggregates data records to variable minutes data format
# FUNCTION:             DESCRIPTION:
#  xts.log               Calculates logarithms for xts time series values
#  xts.diff              Differentiates xts time series values with lag=1
#  xts.cut               Cuts a piece out of a xts time series
#  xts.interp            Interpolates for equidistant time steps
#  xts.map               Creates a volatility adjusted time-mapping 
#  xts.upsilon           Interpolates a time series in upsilon time
#  xts.dvs               Creates a de-volatilizised time series
#  xts.dwh               Plots intra-daily/weekly histograms 
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(HighFrequencyDataTools); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
} 
 

# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fCalendar/test/runit026A.R")
    printTextProtocol(testResult)
}


################################################################################
   
