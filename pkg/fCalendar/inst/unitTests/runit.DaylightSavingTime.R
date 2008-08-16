
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# It is distributed in the hope that it will be useful,
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################


test.zurich =
function()
{
    # DST Rules for Zurich:
    head(Zurich())
    tail(Zurich())

    # Return Value:
    return()
}


if (!try(system("zdump"))) {
    test.DST <- function()
    {
        # works only if OS is well configured !!!

        finCenter <- listFinCenter()

        for (k in seq_along(finCenter)) {

            zdump <- try(system(paste("zdump ", finCenter[k], sep=" "), intern=TRUE))
            zdump <- strsplit(zdump, " +" )
            zdump <- unlist(zdump)


            dts <- paste(zdump[c(3, 4, 6)], collapse = " ")
            tms <- zdump[5]
            timeSys <- timeDate(paste(dts, tms), format =  "%b %d %Y %H:%M:%S",
                                zone = finCenter[k], FinCenter = finCenter[k])

            timeTest <- Sys.timeDate(finCenter[k])

            # round and compare
            cat("\nSimple DST test for", finCenter[k], "\n")
            cat("System\t\t", as.character(timeSys), "\n")
            cat("fCalendar\t", as.character(timeTest), "\n")
            checkTrue(abs(as.numeric(timeSys - timeTest)) < 5)
        }
    }

}

################################################################################
