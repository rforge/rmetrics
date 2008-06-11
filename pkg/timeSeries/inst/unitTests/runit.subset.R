
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


test.subset <-
function()
{

ts <- timeSeries()
mat <- as.matrix(ts)

# we want the same subset-ting rules as for a matrix
# but we always print result in vertical style !

# ------------------------------------------------------------------------------
# index

ts[seq(4),2]
mat[seq(4),2]

ts[rep(FALSE, 3), 1]
mat[rep(FALSE, 3), 1]

ts[FALSE, 1]
mat[FALSE, 1]

ts[rep(TRUE), 2]
mat[rep(TRUE), 2]

ts["2008-01-01", 1]
mat["2008-01-01", 1]

ts[seq(4),]
mat[seq(4),]

ts[rep(FALSE, 3), ]
mat[rep(FALSE, 3), ]

ts[FALSE, ]
mat[FALSE, ]

ts[rep(TRUE), ]
mat[rep(TRUE), ]

ts["2008-01-01", ]
mat["2008-01-01", ]

ts[,2]
mat[,2]

ts[2,FALSE]
mat[2,FALSE]

# prefer to have an empty timeSeries instead of empty data with row names
ts[,FALSE]
mat[,FALSE]

ts[,TRUE ]
mat[,TRUE ]

ts[, "TS.1"]
mat[, "TS.1"]


# ------------------------------------------------------------------------------
# timeDate

ts[timeCalendar()[1:5], 2]
ts[timeCalendar()[1:5], ]

# ------------------------------------------------------------------------------
# logical matrix and timeSeries

i <- ts < 0.4

checkException(ts[series(i), ])
checkException(ts[i, ])
checkException(mat[series(i), ]) # it fails as expected

ts[series(i)[,1], ]
ts[i[,1], ]
mat[series(i)[,1], ]

ts[series(i)[,1],1]
ts[i[,1],1]
mat[series(i)[,1],1]

# this should fail
checkException(ts[series(i), 2], silent = TRUE)
checkException(ts[i, 2], silent = TRUE)
checkException(ts[series(i), 1], silent = TRUE)

checkException(ts[series(i),1])
checkException(ts[i,1])
checkException(mat[series(i),1])

checkException(ts[series(i),])
checkException(mat[series(i),])

ts[series(i)]
ts[i]
mat[series(i)]

}


################################################################################

