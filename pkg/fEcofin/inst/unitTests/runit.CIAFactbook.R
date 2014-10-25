
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
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FINCTIONS:                     WORLD FACTBOOK FROM CIA:
#  ciaCountries                   Returns a list of CIA country codes
#  print.ciaCountries             S3 print method for 'ciaIndicators'
#  ciaIndicators                  Returns a list of CIA indicator codes
#  print.ciaIndicators            S3 print method for 'ciaIndicators'
#  ciaByCountry                   Returns all Indicators by country
#  ciaByIndicator                 Returns for all countries indicator ranking
# FUNCTIONS:                     FOR INTERNAL USE, ONLY - DO NOT RUN:
#  .createFactbook                Creates CIA Data for use with Rmetrics
#  .createIndicators              List of indicator for use with  Rmetrics
################################################################################


test.ciaCountries <-
    function()
{
    # CIA Countries:
    print(head(ciaCountries()))
    country <- ciaCountries()[212, 3]
    current <- "CH"
    names(current) <- "Country"
    checkIdentical(country, current)
    country <- ciaCountries()[212, "Name"]
    current <- "Switzerland"
    names(current) <- "Name"
    checkIdentical(country, current)
    cat("\n")

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.ciaIndicators <-
    function()
{
    # CIA Indicators:
    print(head(ciaIndicators()))
    indicator <- ciaIndicators()[19, 1]
    current <- "2095"
    names(current) <- "Code"
    checkIdentical(indicator, current)
    indicator <- ciaIndicators()[19, "Indicator"]
    current <- "Labor force"
    names(current) <- "Indicator"
    checkIdentical(indicator, current)
    cat("\n")

    # Return Value:
    return()
}


################################################################################

