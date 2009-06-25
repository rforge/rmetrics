
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


test.bind <-
function()
{
    ts <- dummySeries()
    ts@documentation <- character(0)

    # --------------------------------------------------------------------------
    # if NULL are in args, result identical except @documentation
    cts <- cbind(ts, NULL)
    rts <- rbind(ts, NULL)
    checkTrue(!identical(slot(cts, "documentation"), ts@documentation))
    checkTrue(!identical(slot(rts, "documentation"), ts@documentation))

    # check that the rest is identical
    cts@documentation <- character(0)
    rts@documentation <- character(0)
    checkIdentical(cts, ts)
    checkIdentical(rts, ts)

    # --------------------------------------------------------------------------
    ts1 <- ts[seq(1, nrow(ts), by = 2),]
    ts0 <- ts[seq(2, nrow(ts), by = 2),]

    # test rbind
    checkTrue(all(time(rbind(ts1, ts0)) == c(time(ts1),time(ts0))))

    # test cbind
    checkIdentical(as.vector(is.na(cbind(ts1, ts0))),
                   c(rep(c(FALSE, TRUE), 12), rep(c(TRUE, FALSE), 12)))
    checkTrue(all(time(cbind(ts1, ts0)) == time(ts)))

}


################################################################################

