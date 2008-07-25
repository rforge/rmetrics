
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# Rmetrics is distributed in the hope that it will be useful,
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
#   1999 - Diethelm Wuertz, GPL, wuertz@phys.ethz.ch
#   2008 - Rmetrics Foundation, GPL, <Rmetrics-core@r-project.org>
#   www.rmetrics.org
# Copyrights and Authors for code used from R's base packages, from
#   contributed R-packages, and/or other sources is mentioned at the
#   places  where used.


################################################################################


test.aggregate.timeSeries <-
function()
{
    # RUnit Test:

    # aggregate.timeSeries(x, by = c("monthly", "quarterly"), FUN = colMeans,
    #   units = NULL, ...)

    # A daily Series:
    charvec <- timeSequence(length.out = 365)
    x <- timeSeries(rnorm(365), charvec)

    # Aggregate Returns Monthly:
    by <- unique(timeFirstDayInMonth(charvec))
    aggregate(x, by, sum, units = "MonthReturns")

    # Count Monthly Records:
    aggregate(sign(abs(x)), end(charvec), sum, units = "NoOfRecords")

    # Aggregate Returns Quarterly:
    by <- unique(timeLastDayInQuarter(charvec))
    aggregate(x, by, sum, units = "QrtReturns")

    # Another example
    if (require(fEcofin)) {
        x <- as.timeSeries(data(LPP2005REC))[,1:4]
        by <- timeSequence(from = "2006-01-01",
                           to = "2008-01-01",
                           by = "quarter")
        aggregate(x, by, mean)
    }

    x <- timeSeries(seq(12), timeCalendar())
    by <- timeSequence(from = "2008-01-01",
                       to = "2009-01-01",
                       by = "quarter")
    x
    a <- aggregate(x, by, sum)
    a
    checkEquals(sum(x[1]), a[1])
    checkEquals(sum(x[2:4]), a[2])
    checkEquals(sum(x[5:7]), a[3])
    checkEquals(sum(x[8:10]), a[4])
    checkEquals(sum(x[11:12]), a[5])

}

################################################################################

